use std::collections::HashMap;

use syn::Lit;

use crate::{
    MoltFile,
    ctx::{AstCtx, Id, MatchCtx, MatchingMode, NodeId, NodeList, PatCtx},
    parser::{
        Node, Pattern, VarDecl, VarId,
        rust_grammar::{
            Expr, ExprBinary, ExprLit, ExprParen, ExprUnary, Ident, Item, ItemConst, ItemFn,
        },
    },
};

#[cfg(feature = "debug-print")]
use crate::parser::CustomDebug;

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
                match_.cmp_id(ctx, cmp.ast, cmp.pat);
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

    fn check(&mut self, val: bool) {
        if !val {
            self.valid = false;
        }
    }

    fn eq<T: PartialEq>(&mut self, t1: T, t2: T) {
        self.check(t1 == t2)
    }

    fn same<T>(&mut self, t1: &Option<T>, t2: &Option<T>) {
        self.check(t1.is_some() == t2.is_some())
    }

    fn no_match(&mut self) {
        self.check(false)
    }

    fn cmp<T: CmpDirect>(&mut self, ast: NodeId<T>, pat: NodeId<T>) {
        self.cmps.push(Comparison::new(ast, pat));
    }

    // This exists purely to make the calls look symmetrical
    fn cmp_direct<T: CmpDirect>(&mut self, t1: &T, t2: &T) {
        t1.cmp_direct(self, t2)
    }

    fn cmp_nodes(&mut self, _ctx: &MatchCtx, ast: &Node, pat: &Node) {
        #[cfg(feature = "debug-print")]
        if ast.kind() == pat.kind() {
            println!(
                "Compare ({:?} {:?})\n\t{}\n\t{}",
                ast.kind(),
                pat.kind(),
                ast.deb(_ctx),
                pat.deb(_ctx),
            );
        }
        if ast.kind() == pat.kind() {
            Node::cmp_equal_kinds(self, ast, pat);
        } else {
            self.no_match()
        }
    }

    fn cmp_id(&mut self, ctx: &MatchCtx, ast_id: Id, pat_id: Id) {
        match ctx.get_pat_node(pat_id) {
            Pattern::Pattern(var) => {
                self.add_binding(ctx, var, ast_id, true);
            }
            Pattern::Exact(pat) => self.cmp_nodes(ctx, ctx.ast_ctx.get_node(ast_id), pat),
        }
    }

    fn cmp_lists<T: CmpDirect>(&mut self, ts1: &NodeList<T>, ts2: &NodeList<T>) {
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
            println!(
                "\tBind {} to {}",
                key.deb(ctx),
                ctx.ast_ctx.get_node(ast_id).deb(ctx)
            );
        }
        let binding = self.bindings.get_mut(&key).unwrap();
        if let Some(ast_id_2) = binding.ast {
            self.cmp_nodes(
                ctx,
                ctx.ast_ctx.get_node(ast_id),
                ctx.ast_ctx.get_node(ast_id_2),
            );
        } else {
            binding.ast = Some(ast_id);
            if let Some(pat_id) = binding.pat {
                self.cmp_nodes(
                    ctx,
                    ctx.ast_ctx.get_node(ast_id),
                    ctx.pat_ctx.get_node(pat_id),
                );
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
    ) -> MatchResult {
        let ctx = MatchCtx::new(pat_ctx, ast_ctx);
        #[cfg(feature = "debug-print")]
        ctx.dump();
        let matches = ctx
            .ast_ctx
            .iter()
            .flat_map(|item| {
                let mut matches = Matches::new(&ctx, &self.vars, var.clone(), item);
                matches.run(&ctx)
            })
            .collect();
        MatchResult {
            matches,
            ctx,
            var: var.clone(),
        }
    }
}

pub(crate) trait CmpDirect {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self);
}

impl CmpDirect for Item {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        match self {
            Item::Const(c_ast) => {
                if let Item::Const(c_pat) = pat {
                    ctx.cmp_direct(c_ast, c_pat);
                } else {
                    ctx.no_match()
                }
            }
            Item::Fn(fn_ast) => {
                if let Item::Fn(fn_pat) = pat {
                    ctx.cmp_direct(fn_ast, fn_pat);
                } else {
                    ctx.no_match()
                }
            }
            _ => ctx.no_match(),
        }
    }
}

// impl CmpDirect for Lit {
//     fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
//         let cmp_bool = || {
//             match self {
//                 syn::Lit::Str(s1) => {
//                     if let syn::Lit::Str(s2) = pat {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::ByteStr(s1) => {
//                     if let syn::Lit::ByteStr(s2) = pat {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::CStr(s1) => {
//                     if let syn::Lit::CStr(s2) = pat {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::Byte(s1) => {
//                     if let syn::Lit::Byte(s2) = pat {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::Char(s1) => {
//                     if let syn::Lit::Char(s2) = pat {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::Int(s1) => {
//                     if let syn::Lit::Int(s2) = pat {
//                         return s1.base10_digits() == s2.base10_digits();
//                     }
//                 }
//                 syn::Lit::Float(s1) => {
//                     if let syn::Lit::Float(s2) = pat {
//                         return s1.base10_digits() == s2.base10_digits();
//                     }
//                 }
//                 syn::Lit::Bool(s1) => {
//                     if let syn::Lit::Bool(s2) = pat {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 _ => todo!(),
//             }
//             false
//         };
//         ctx.check(cmp_bool())
//     }
// }

impl CmpDirect for Expr {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        match self {
            Expr::Binary(i1) => {
                if let Expr::Binary(i2) = pat {
                    ctx.cmp_direct(i1, i2);
                    return;
                }
            }
            Expr::Unary(i1) => {
                if let Expr::Unary(i2) = pat {
                    ctx.cmp_direct(i1, i2);
                    return;
                }
            }
            Expr::Lit(i1) => {
                if let Expr::Lit(i2) = pat {
                    ctx.cmp_direct(i1, i2);
                    return;
                }
            }
            Expr::Paren(i1) => {
                if let Expr::Paren(i2) = pat {
                    ctx.cmp_direct(i1, i2);
                    return;
                }
            }
            _ => {
                todo!()
            }
        }
        ctx.no_match()
    }
}

impl CmpDirect for ExprBinary {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_direct(&self.op, &pat.op);
        ctx.cmp_direct(&*self.left, &*pat.left);
        ctx.cmp_direct(&*self.right, &*pat.right);
    }
}

impl CmpDirect for ExprUnary {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_direct(&self.op, &pat.op);
        ctx.cmp_direct(&*self.expr, &*pat.expr);
    }
}

impl CmpDirect for ExprLit {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        todo!()
        // ctx.cmp(*self.lit, pat.lit);
    }
}

impl CmpDirect for ExprParen {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        todo!()
        // ctx.cmp(self.expr, pat.expr);
    }
}

impl CmpDirect for ItemConst {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        // self.vis.match_pattern(&item.vis)?;
        ctx.cmp(self.ident, pat.ident);
        // self.generics.match_pattern(&item.generics)?;
        // self.ty.match_pattern(&item.ty)?;
        ctx.cmp_direct(&*self.expr, &*pat.expr);
    }
}

impl CmpDirect for syn::BinOp {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        let is_match = match self {
            syn::BinOp::Add(_) => matches!(pat, syn::BinOp::Add(_)),
            syn::BinOp::Sub(_) => matches!(pat, syn::BinOp::Sub(_)),
            syn::BinOp::Mul(_) => matches!(pat, syn::BinOp::Mul(_)),
            syn::BinOp::Div(_) => matches!(pat, syn::BinOp::Div(_)),
            syn::BinOp::Rem(_) => matches!(pat, syn::BinOp::Rem(_)),
            syn::BinOp::And(_) => matches!(pat, syn::BinOp::And(_)),
            syn::BinOp::Or(_) => matches!(pat, syn::BinOp::Or(_)),
            syn::BinOp::BitXor(_) => matches!(pat, syn::BinOp::BitXor(_)),
            syn::BinOp::BitAnd(_) => matches!(pat, syn::BinOp::BitAnd(_)),
            syn::BinOp::BitOr(_) => matches!(pat, syn::BinOp::BitOr(_)),
            syn::BinOp::Shl(_) => matches!(pat, syn::BinOp::Shl(_)),
            syn::BinOp::Shr(_) => matches!(pat, syn::BinOp::Shr(_)),
            syn::BinOp::Eq(_) => matches!(pat, syn::BinOp::Eq(_)),
            syn::BinOp::Lt(_) => matches!(pat, syn::BinOp::Lt(_)),
            syn::BinOp::Le(_) => matches!(pat, syn::BinOp::Le(_)),
            syn::BinOp::Ne(_) => matches!(pat, syn::BinOp::Ne(_)),
            syn::BinOp::Ge(_) => matches!(pat, syn::BinOp::Ge(_)),
            syn::BinOp::Gt(_) => matches!(pat, syn::BinOp::Gt(_)),
            syn::BinOp::AddAssign(_) => matches!(pat, syn::BinOp::AddAssign(_)),
            syn::BinOp::SubAssign(_) => matches!(pat, syn::BinOp::SubAssign(_)),
            syn::BinOp::MulAssign(_) => matches!(pat, syn::BinOp::MulAssign(_)),
            syn::BinOp::DivAssign(_) => matches!(pat, syn::BinOp::DivAssign(_)),
            syn::BinOp::RemAssign(_) => matches!(pat, syn::BinOp::RemAssign(_)),
            syn::BinOp::BitXorAssign(_) => matches!(pat, syn::BinOp::BitXorAssign(_)),
            syn::BinOp::BitAndAssign(_) => matches!(pat, syn::BinOp::BitAndAssign(_)),
            syn::BinOp::BitOrAssign(_) => matches!(pat, syn::BinOp::BitOrAssign(_)),
            syn::BinOp::ShlAssign(_) => matches!(pat, syn::BinOp::ShlAssign(_)),
            syn::BinOp::ShrAssign(_) => matches!(pat, syn::BinOp::ShrAssign(_)),
            _ => todo!(),
        };
        ctx.check(is_match)
    }
}

impl CmpDirect for syn::UnOp {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        let is_match = match self {
            syn::UnOp::Deref(_) => matches!(pat, syn::UnOp::Deref(_)),
            syn::UnOp::Not(_) => matches!(pat, syn::UnOp::Not(_)),
            syn::UnOp::Neg(_) => matches!(pat, syn::UnOp::Neg(_)),
            _ => todo!(),
        };
        ctx.check(is_match)
    }
}

impl CmpDirect for ItemFn {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        todo!()
        // ctx.cmp(self.sig, pat.sig);
        // ctx.cmp_direct(self.expr, pat.expr);
    }
}

// impl CmpDirect for Signature {
//     fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
//         ctx.same(&self.constness, &pat.constness);
//         ctx.same(&self.asyncness, &pat.asyncness);
//         ctx.same(&self.unsafety, &pat.unsafety);
//         // ctx.cmp_direct(&self.abi, &pat.abi);
//         ctx.cmp(self.ident, pat.ident);
//         ctx.cmp_direct(&self.generics, &pat.generics);
//         ctx.cmp_lists(&self.inputs, &pat.inputs);
//         ctx.cmp_direct(&self.output, &pat.output);
//     }
// }

// impl CmpDirect for syn::Generics {
//     fn cmp_direct(&self, _: &mut Match, _: &Self) {
//         // todo
//     }
// }

// impl CmpDirect for syn::ReturnType {
//     fn cmp_direct(&self, _: &mut Match, _: &Self) {
//         // todo
//     }
// }

// impl CmpDirect for FnArg {
//     fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
//         ctx.cmp(self.ident, pat.ident);
//     }
// }

impl CmpDirect for Lit {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        todo!()
        // ctx.eq(self, pat)
    }
}

impl CmpDirect for Ident {
    fn cmp_direct(&self, ctx: &mut Match, pat: &Self) {
        ctx.eq(&self, &pat)
    }
}
