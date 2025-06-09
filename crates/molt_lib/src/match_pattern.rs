use std::collections::HashMap;

use crate::{GetKind, Id, MatchingMode, NodeId, NodeList, Pattern, VarDecl};

use crate::cmp_syn::CmpSyn;
use crate::match_ctx::MatchCtx;

pub fn match_pattern<N: GetKind + CmpSyn>(
    ctx: &MatchCtx<N>,
    vars: &[VarDecl],
    var: Id,
    ast: Id,
) -> Vec<Match> {
    let mut match_ = Match::new(vars);
    match_.add_binding(ctx, var, ast);
    let mut current = vec![match_];
    let mut matches = vec![];
    'outer: while let Some(mut match_) = current.pop() {
        while let Some(cmp) = match_.cmps.pop() {
            match_.cmp_ids(ctx, cmp.ast, cmp.pat, cmp.pat_type);
            if !match_.valid {
                break 'outer;
            }
        }
        if match_.forks.is_empty() {
            if match_.valid {
                matches.push(match_);
            }
        } else {
            current.extend(match_.make_forks());
        }
    }
    matches
}

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
            pat_type: pat_type,
        }
    }
}

#[derive(Clone, Debug)]
struct Fork {
    cmps: Vec<Comparison>,
}

impl Fork {
    fn new(cmps: Vec<Comparison>) -> Fork {
        Self { cmps }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub pat: Option<Id>,
    pub ast: Option<Id>,
}

impl Binding {
    fn new(pat: Option<Id>) -> Self {
        Self { pat, ast: None }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum PatType {
    FromAst,
    FromPat,
}

#[derive(Debug)]
pub struct Match {
    bindings: HashMap<Id, Binding>,
    cmps: Vec<Comparison>,
    forks: Vec<Fork>,
    valid: bool,
    pat_type: PatType,
}

impl Match {
    fn new(vars: &[VarDecl]) -> Self {
        Self {
            bindings: vars
                .iter()
                .map(|var| (var.id, Binding::new(var.node)))
                .collect(),
            cmps: vec![],
            forks: vec![],
            valid: true,
            pat_type: PatType::FromPat,
        }
    }

    fn cmp_ids<N: GetKind + CmpSyn>(
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
        let ast_kind = ctx.ast_ctx.get_kind(ast_id);
        let pat_kind = ctx.get_kind(pat_id, pat_type);
        if ctx.config.debug_print && ast_kind == pat_kind {
            println!(
                "Compare ({:?} {:?})\n\t{}\n\t{}",
                ast_kind,
                pat_kind,
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
            Pattern::Real(pat) => self.cmp_syn(ctx.ast_ctx.get(ast_id).unwrap_real(), pat),
        }
    }

    fn fork(&mut self, fork: Fork) {
        self.forks.push(fork);
    }

    fn add_binding<N: GetKind + CmpSyn>(&mut self, ctx: &MatchCtx<N>, key: Id, ast_id: Id) {
        if ctx.config.debug_print {
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

    fn make_forks(mut self) -> impl Iterator<Item = Match> {
        assert!(self.cmps.is_empty());
        assert!(self.valid);
        let fork = self.forks.pop().unwrap();
        fork.cmps.into_iter().map(move |cmp| Match {
            bindings: self.bindings.clone(),
            forks: self.forks.clone(),
            cmps: vec![cmp],
            valid: true,
            pat_type: self.pat_type,
        })
    }

    pub fn iter_vars(&self) -> impl Iterator<Item = Id> {
        self.bindings.keys().cloned()
    }

    pub fn get_binding(&self, var: Id) -> &Binding {
        &self.bindings[&var]
    }

    pub fn cmp_lists<T: CmpSyn, P>(&mut self, ts1: &NodeList<T, P>, ts2: &NodeList<T, P>) {
        let matching_mode = MatchingMode::Exact;
        let NodeList::Real(ts1) = ts1 else { todo!() };
        let NodeList::Real(ts2) = ts2 else { todo!() };
        match matching_mode {
            MatchingMode::Exact => {
                self.eq(ts1.len(), ts2.len());
                for (item1, item2) in ts1.iter().zip(ts2.iter()) {
                    self.cmp_nodes(*item1, *item2);
                }
            }
            MatchingMode::ContainsAll => {
                if ts2.is_empty() {
                } else if ts2.len() == 1 {
                    let item2 = ts2.get(0).unwrap();
                    let fork = Fork::new(
                        ts1.iter()
                            .map(|item1| Comparison::new(*item1, *item2, self.pat_type))
                            .collect(),
                    );
                    self.fork(fork);
                } else {
                    todo!()
                }
            }
        }
    }

    pub fn check(&mut self, val: bool) {
        if !val {
            self.valid = false;
        }
    }

    pub fn eq<T: PartialEq>(&mut self, t1: T, t2: T) {
        self.check(t1 == t2)
    }

    pub fn same<T>(&mut self, t1: &Option<T>, t2: &Option<T>) {
        self.check(t1.is_some() == t2.is_some())
    }

    pub fn no_match(&mut self) {
        self.check(false)
    }

    pub fn cmp_nodes<T: CmpSyn>(&mut self, ast: NodeId<T>, pat: NodeId<T>) {
        self.cmps.push(Comparison::new(ast, pat, self.pat_type));
    }

    // This exists purely to make the calls look symmetrical
    pub fn cmp_syn<T: CmpSyn>(&mut self, t1: &T, t2: &T) {
        t1.cmp_syn(self, t2)
    }
}
