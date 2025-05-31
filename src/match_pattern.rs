use std::collections::HashMap;

use crate::{
    MoltFile,
    cmp_syn::CmpSyn,
    ctx::{AstCtx, Id, MatchCtx, MatchingMode, NodeId, NodeList, PatCtx},
    parser::{
        Node, Pattern, VarDecl, VarId,
        rust_grammar::{
            Expr, ExprBinary, ExprLit, ExprParen, ExprUnary, Ident, Item, ItemConst, ItemFn, Lit,
        },
    },
};

pub(crate) struct Matches {
    current: Vec<Match>,
}

impl Matches {
    fn new(ctx: &MatchCtx, vars: &[VarDecl], var: VarId, ast: Id) -> Self {
        let mut match_ = Match::new(vars);
        match_.add_binding(ctx, var, ast, false);
        Self {
            current: vec![match_],
        }
    }

    fn run(&mut self, ctx: &MatchCtx) -> Vec<Match> {
        let mut matches = vec![];
        'outer: while let Some(mut match_) = self.current.pop() {
            while let Some(cmp) = match_.cmps.pop() {
                match_.cmp_ids(ctx, cmp.ast, cmp.pat);
                if !match_.valid {
                    break 'outer;
                }
            }
            if match_.forks.is_empty() {
                if match_.valid {
                    matches.push(match_);
                }
            } else {
                self.current.extend(match_.make_forks());
            }
        }
        matches
    }
}

#[derive(Clone, Debug)]
struct Comparison {
    ast: Id,
    pat: Id,
}

impl Comparison {
    fn new<T>(ast: NodeId<T>, pat: NodeId<T>) -> Comparison {
        Comparison {
            ast: ast.untyped(),
            pat: pat.untyped(),
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
pub(crate) struct Binding {
    pub pat: Option<Id>,
    pub ast: Option<Id>,
}

impl Binding {
    fn new(pat: Option<Id>) -> Self {
        Self { pat, ast: None }
    }
}

#[derive(Debug, Default)]
pub(crate) struct Match {
    bindings: HashMap<VarId, Binding>,
    cmps: Vec<Comparison>,
    forks: Vec<Fork>,
    valid: bool,
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
        }
    }

    fn cmp_ids(&mut self, ctx: &MatchCtx, ast_id: Id, pat_id: Id) {
        #[cfg(feature = "debug-print")]
        {
            let ast_kind = ctx.get_kind(ast_id);
            let pat_kind = ctx.get_kind(pat_id);
            if ast_kind == pat_kind {
                println!(
                    "Compare ({:?} {:?})\n\t{}\n\t{}",
                    ast_kind,
                    pat_kind,
                    ctx.print(ast_id),
                    ctx.print(pat_id),
                );
            }
        }
        match ctx.get_pat_node(pat_id) {
            Pattern::Pattern(var) => {
                self.add_binding(ctx, var, ast_id, true);
            }
            Pattern::Exact(pat) => self.cmp_syn(ctx.ast_ctx.get_node(ast_id), pat),
        }
    }

    fn cmp_lists<T: CmpSyn>(&mut self, ts1: &NodeList<T>, ts2: &NodeList<T>) {
        match ts2.matching_mode {
            MatchingMode::Exact => {
                self.eq(ts1.len(), ts2.len());
                for (item1, item2) in ts1.iter().zip(ts2.iter()) {
                    self.cmp(*item1, *item2);
                }
            }
            MatchingMode::ContainsAll => {
                if ts2.is_empty() {
                } else if ts2.len() == 1 {
                    let item2 = ts2.get(0).unwrap();
                    let fork = Fork::new(
                        ts1.iter()
                            .map(|item1| Comparison::new(*item1, *item2))
                            .collect(),
                    );
                    self.fork(fork);
                } else {
                    todo!()
                }
            }
        }
    }

    fn fork(&mut self, fork: Fork) {
        self.forks.push(fork);
    }

    fn add_binding(&mut self, ctx: &MatchCtx, key: VarId, ast_id: Id, debug_print: bool) {
        if debug_print {
            #[cfg(feature = "debug-print")]
            println!("\tBind {} to {}", ctx.get_var(key), ctx.print(ast_id));
        }
        let binding = self.bindings.get_mut(&key).unwrap();
        if let Some(ast_id_2) = binding.ast {
            self.cmp_ids(ctx, ast_id, ast_id_2);
        } else {
            binding.ast = Some(ast_id);
            if let Some(pat_id) = binding.pat {
                self.cmp_ids(ctx, ast_id, pat_id);
            }
        }
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = VarId> {
        self.bindings.keys().cloned()
    }

    pub(crate) fn get_binding(&self, var: VarId) -> &Binding {
        &self.bindings[&var]
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
        })
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

    pub fn cmp<T: CmpSyn>(&mut self, ast: NodeId<T>, pat: NodeId<T>) {
        self.cmps.push(Comparison::new(ast, pat));
    }

    // This exists purely to make the calls look symmetrical
    pub fn cmp_syn<T: CmpSyn>(&mut self, t1: &T, t2: &T) {
        t1.cmp_syn(self, t2)
    }
}

pub(crate) struct MatchResult {
    pub matches: Vec<Match>,
    pub ctx: MatchCtx,
    pub var: VarId,
}

impl MoltFile {
    pub(crate) fn match_pattern(
        &self,
        ast_ctx: AstCtx,
        pat_ctx: PatCtx,
        var: VarId,
        rust_src: &str,
        molt_src: &str,
    ) -> MatchResult {
        let ctx = MatchCtx::new(pat_ctx, ast_ctx, rust_src, molt_src);
        #[cfg(feature = "debug-print")]
        ctx.dump();
        let pat_kind = ctx.get_var_kind(var);
        let matches = ctx
            .ast_ctx
            .iter()
            .flat_map(|item| {
                let kind = ctx.get_kind(item);
                if pat_kind != kind {
                    vec![]
                } else {
                    let mut matches = Matches::new(&ctx, &self.vars, var.clone(), item);
                    matches.run(&ctx)
                }
            })
            .collect();
        MatchResult {
            matches,
            ctx,
            var: var.clone(),
        }
    }
}
