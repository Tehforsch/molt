use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::cmp_syn::CmpSyn;
use crate::match_ctx::MatchCtx;
use crate::node_list::List;
use crate::{
    Config, Id, ListMatchingMode, NodeId, NodeList, NodeType, PatNodeList, Pattern, RealNodeList,
    Rule, RuleKey, Rules, Single, SingleMatchingMode, VarDecl,
};

type MatchId = usize;

#[derive(Clone, Debug)]
struct Comparison {
    ast: Id,
    pat: Id,
    pat_type: PatType,
}

impl Comparison {
    fn new<T>(ast: NodeId<T>, pat: NodeId<T>, pat_type: PatType) -> Comparison {
        Comparison {
            ast: ast.into(),
            pat: pat.into(),
            pat_type,
        }
    }
}

#[derive(Clone, Debug)]
struct Fork {
    cmps: Vec<Comparison>,
    make_multi_match: bool,
}

impl Fork {
    fn new(cmps: Vec<Comparison>, make_multi_match: bool) -> Fork {
        Self {
            cmps,
            make_multi_match,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pat: Option<Id>,
    ast: Option<Id>,
}

impl Binding {
    fn new(pat: Option<Id>) -> Self {
        Self { pat, ast: None }
    }
}

#[derive(Debug, Clone)]
pub struct MultiBinding {
    pub pat: Option<Id>,
    pub ast: Vec<Id>,
}

#[derive(Clone, Copy, Debug)]
pub enum PatType {
    FromAst,
    FromPat,
}

#[derive(Debug)]
pub struct Matcher<'a> {
    rules: &'a Rules,
    bindings: HashMap<Id, Binding>,
    cmps: Vec<Comparison>,
    forks: Vec<Fork>,
    valid: bool,
    pat_type: PatType,
    id: MatchId,
    multi_match_id: Option<MatchId>,
}

#[derive(Debug)]
pub struct Match {
    bindings: HashMap<Id, MultiBinding>,
}

impl<'a> Matcher<'a> {
    fn new_root(vars: &[VarDecl], rules: &'a Rules) -> Self {
        Self {
            rules,
            bindings: vars
                .iter()
                .map(|var| (var.id, Binding::new(var.node)))
                .collect(),
            cmps: vec![],
            forks: vec![],
            valid: true,
            pat_type: PatType::FromPat,
            id: 0,
            multi_match_id: None,
        }
    }

    fn cmp_ids<N: NodeType + CmpSyn>(
        &mut self,
        ctx: &MatchCtx<N>,
        ast_id: Id,
        pat_id: Id,
        pat_type: PatType,
    ) {
        // Need to double check this logic, but the idea here is that
        // as long as we're comparing things with a given pat type,
        // this won't change for any calls further down the line
        // (unless it explicitly changes it via add_binding).  The
        // conventional thing to do here would be to pass the pat type
        // down the line in the `cmp_*` calls, but this is quite
        // cumbersome given that it appears absolutely everywhere, so
        // I'm doing this.
        self.pat_type = pat_type;
        if ctx.config().debug_print {
            println!(
                "Compare \n\t{}\n\t{}",
                ctx.print_ast(ast_id).replace("\n", " "),
                match pat_type {
                    PatType::FromAst => ctx.print_ast(pat_id).replace("\n", " "),
                    PatType::FromPat => ctx.print_pat(pat_id).replace("\n", " "),
                }
            );
        }

        match ctx.get::<N>(pat_id, pat_type) {
            Pattern::Pat(var) => {
                self.add_binding(ctx, var, ast_id);
            }
            Pattern::Real(pat) => self.cmp_syn::<N, N>(ctx.ast_ctx.get(ast_id).unwrap_real(), pat),
        }
    }

    fn fork(&mut self, fork: Fork) {
        self.forks.push(fork);
    }

    fn add_binding<N: NodeType + CmpSyn>(&mut self, ctx: &MatchCtx<N>, key: Id, ast_id: Id) {
        if ctx.config().debug_print {
            println!(
                "\tBind ${} to {}",
                &ctx.get_var(key).name(),
                ctx.print_ast(ast_id)
            );
        }
        let binding = self.bindings.get_mut(&key).unwrap();
        if let Some(ast_id_2) = binding.ast {
            self.cmp_ids(ctx, ast_id, ast_id_2, PatType::FromAst);
        } else {
            binding.ast = Some(ast_id);
            if let Some(pat_id) = binding.pat {
                self.cmp_ids(ctx, ast_id, pat_id, PatType::FromPat);
            }
        }
    }

    fn make_forks(
        mut self,
        next_id: &mut (impl FnOnce() -> MatchId + Copy),
    ) -> impl Iterator<Item = Matcher<'a>> {
        assert!(self.cmps.is_empty());
        assert!(self.valid);
        let fork = self.forks.pop().unwrap();
        fork.cmps.into_iter().map(move |cmp| Matcher {
            rules: self.rules,
            bindings: self.bindings.clone(),
            forks: self.forks.clone(),
            cmps: vec![cmp],
            valid: true,
            pat_type: self.pat_type,
            id: (*next_id)(),
            multi_match_id: if fork.make_multi_match {
                Some(self.id)
            } else {
                None
            },
        })
    }

    pub fn cmp_lists<T: CmpSyn, P>(&mut self, ts1: &NodeList<T, P>, ts2: &NodeList<T, P>) {
        match (ts1, ts2) {
            (Pattern::Real(ts1), Pattern::Real(ts2)) => {
                self.cmp_lists_real(ts1.items(), ts2.items())
            }
            (Pattern::Real(ts1), Pattern::Pat(ts2)) => match ts2 {
                PatNodeList::Single(single) => self.cmp_lists_single(ts1, single),
                PatNodeList::List(list) => self.cmp_lists_list(ts1, list),
            },
            (Pattern::Pat(_), _) => unreachable!(),
        }
    }

    fn cmp_lists_real<T: CmpSyn>(&mut self, ts1: &[NodeId<T>], ts2: &[NodeId<T>]) {
        self.eq(ts1.len(), ts2.len());
        for (item1, item2) in ts1.iter().zip(ts2.iter()) {
            self.cmp_nodes(*item1, *item2);
        }
    }

    fn cmp_lists_single<T: CmpSyn, P>(&mut self, ts1: &RealNodeList<T, P>, ts2: &Single<T, P>) {
        let cmps = ts1
            .iter()
            .map(|item1| Comparison::new(*item1, ts2.item(), self.pat_type))
            .collect();
        match ts2.mode() {
            SingleMatchingMode::Any => {
                self.fork(Fork::new(cmps, false));
            }
            SingleMatchingMode::All => {
                self.fork(Fork::new(cmps, true));
            }
        }
    }

    fn cmp_lists_list<T: CmpSyn, P>(&mut self, ts1: &RealNodeList<T, P>, ts2: &List<T, P>) {
        match ts2.mode() {
            ListMatchingMode::Exact => self.cmp_lists_real(ts1.items(), ts2.items()),
            ListMatchingMode::ContainsAll => {
                if ts2.is_empty() {
                } else if ts2.len() == 1 {
                    let item2 = ts2.get(0).unwrap();
                    let fork = Fork::new(
                        ts1.iter()
                            .map(|item1| Comparison::new(*item1, *item2, self.pat_type))
                            .collect(),
                        false,
                    );
                    self.fork(fork);
                } else {
                    todo!()
                }
            }
        }
    }

    fn check(&mut self, val: bool) {
        if !val {
            self.valid = false;
        }
    }

    pub fn eq<T: PartialEq>(&mut self, t1: T, t2: T) {
        self.check(t1 == t2)
    }

    pub fn no_match(&mut self) {
        self.check(false)
    }

    pub fn cmp_nodes<T: CmpSyn>(&mut self, ast: NodeId<T>, pat: NodeId<T>) {
        self.cmps.push(Comparison::new(ast, pat, self.pat_type));
    }

    pub fn cmp_syn<T: CmpSyn<S>, S>(&mut self, t1: &T, t2: &S) {
        t1.cmp_syn(self, t2)
    }

    pub fn should_compare(&mut self, rule: RuleKey) -> bool {
        match self.rules[rule] {
            Rule::Ignore => false,
            Rule::Strict => true,
        }
    }
}

impl Match {
    pub fn get_binding(&self, var: Id) -> &MultiBinding {
        &self.bindings[&var]
    }

    pub fn iter_vars(&self) -> impl Iterator<Item = Id> {
        self.bindings.keys().cloned()
    }
}

pub fn match_pattern<N: NodeType + CmpSyn>(
    ctx: &MatchCtx<N>,
    vars: &[VarDecl],
    var: Id,
    ast: Id,
    config: &Config,
) -> Vec<Match> {
    let mut match_ = Matcher::new_root(vars, &config.rules);
    match_.add_binding(ctx, var, ast);
    let mut current = vec![match_];
    let mut matches = vec![];
    let mut id = 0;
    let mut next_id = move || {
        id += 1;
        id
    };
    while let Some(mut match_) = current.pop() {
        while let Some(cmp) = match_.cmps.pop() {
            match_.cmp_ids(ctx, cmp.ast, cmp.pat, cmp.pat_type);
            if !match_.valid {
                break;
            }
        }
        if match_.forks.is_empty() {
            // Remember the match if it is
            // 1. Valid and not part of a multi match
            // 2. Part of a multi match
            if match_.valid || match_.multi_match_id.is_some() {
                matches.push(match_)
            }
        } else if match_.valid {
            current.extend(match_.make_forks(&mut next_id));
        }
    }
    merge_matches(matches)
}

fn merge_matches(matches: Vec<Matcher>) -> Vec<Match> {
    let mut no_parents = vec![];
    let mut by_parent: HashMap<MatchId, Vec<Matcher>> = HashMap::default();
    for m in matches.into_iter() {
        match m.multi_match_id {
            Some(parent) => match by_parent.entry(parent) {
                Entry::Occupied(mut ms) => ms.get_mut().push(m),
                Entry::Vacant(entry) => {
                    entry.insert(vec![m]);
                }
            },
            None => {
                no_parents.push(m);
            }
        }
    }
    no_parents
        .into_iter()
        .map(|match_| Match {
            bindings: match_
                .bindings
                .into_iter()
                .map(|(id, bind)| (id, make_single_binding(bind)))
                .collect(),
        })
        .chain(by_parent.into_values().filter_map(|matches| {
            if matches.iter().any(|match_| !match_.valid) {
                return None;
            }
            let mut bindings_by_id: HashMap<Id, Vec<Binding>> = HashMap::default();
            let num = matches.len();
            for m in matches.into_iter() {
                for (id, binding) in m.bindings.into_iter() {
                    match bindings_by_id.entry(id) {
                        Entry::Occupied(mut bs) => {
                            let bs = bs.get_mut();
                            if *bs.first().unwrap() != binding {
                                bs.push(binding);
                            }
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(vec![binding]);
                        }
                    }
                }
            }
            let bindings = bindings_by_id
                .into_iter()
                .map(|(id, mut bindings)| {
                    if bindings.len() == 1 {
                        (id, make_single_binding(bindings.remove(0)))
                    } else {
                        assert_eq!(bindings.len(), num);
                        (id, make_multi_binding(bindings))
                    }
                })
                .collect();
            Some(Match { bindings })
        }))
        .collect()
}

fn make_single_binding(bind: Binding) -> MultiBinding {
    MultiBinding {
        pat: bind.pat,
        ast: bind.ast.into_iter().collect(),
    }
}

fn make_multi_binding(bindings: Vec<Binding>) -> MultiBinding {
    // assert!(bindings.iter().all(|bind| bind.pat.is_none()));
    MultiBinding {
        pat: None,
        // The previous .pop() operations will reverse the order, so
        // to restore the original order, we reverse here
        ast: bindings
            .into_iter()
            .rev()
            .map(|bind| bind.ast.unwrap())
            .collect(),
    }
}
