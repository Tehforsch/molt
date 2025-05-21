use std::collections::HashMap;

use crate::{
    ctx::{AstCtx, Id, NodeId, PatCtx},
    grammar::{self, Expr, ExprBinary, ExprLit, ExprUnary, Ident, ItemConst, Lit, Node},
    mangle::Pattern,
    spec::{SynVar, SynVarDecl},
    Ast, Spec,
};

struct MatchCtx {
    pat_ctx: PatCtx,
    ast_ctx: AstCtx,
}

impl MatchCtx {
    fn new(pat_ctx: PatCtx, ast_ctx: AstCtx) -> Self {
        Self { pat_ctx, ast_ctx }
    }
}

pub(crate) struct Matches {
    matches: Vec<Match>,
    current: Match,
    vars: Vec<SynVarDecl>,
    todo: Vec<Comparison>,
}

impl Matches {
    fn new(vars: &[SynVarDecl]) -> Self {
        Self {
            matches: vec![],
            current: Match::new(&vars),
            vars: vars.to_vec(),
            todo: vec![],
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Match> {
        self.matches.iter()
    }

    fn start_new_match(&mut self) {
        let mut new = Match::new(&self.vars);
        std::mem::swap(&mut new, &mut self.current);
        if new.valid {
            self.matches.push(new);
        }
    }

    fn current(&self) -> &Match {
        &self.current
    }

    fn current_mut(&mut self) -> &mut Match {
        &mut self.current
    }

    fn cmp<T: CmpDirect>(&mut self, concrete: NodeId<T>, pat: NodeId<T>) {
        self.current_mut().cmp(concrete, pat);
    }

    // This exists purely to make the calls look symmetrical
    fn cmp_direct<T: CmpDirect>(&mut self, t1: &T, t2: &T) {
        t1.cmp_direct(self, t2)
    }

    fn check(&mut self, val: bool) {
        if !val {
            self.current_mut().valid = false;
        }
    }

    fn eq<T: PartialEq>(&mut self, t1: T, t2: T) {
        self.check(t1 == t2)
    }

    fn no_match(&mut self) {
        self.check(false)
    }

    fn add_todo(&mut self, concrete: Id, pat: Id) {
        self.todo.push(Comparison { concrete, pat })
    }

    fn add_binding(&mut self, ctx: &MatchCtx, key: &SynVar, concrete_id: Id) {
        if let Some(pat_id) = self.current().bindings[&key] {
            self.cmp_nodes(
                ctx.ast_ctx.get_node(concrete_id),
                ctx.pat_ctx.get_node(pat_id),
            );
        } else {
            self.current_mut()
                .bindings
                .insert(key.clone(), Some(concrete_id));
        }
    }

    fn run(&mut self, match_ctx: &MatchCtx) {
        while let Some(todo) = self.todo.pop() {
            self.cmp_id(match_ctx, todo.concrete, todo.pat);
            while let Some(cmp) = self.current.cmps.pop() {
                self.cmp_id(match_ctx, cmp.concrete, cmp.pat);
            }
            self.start_new_match();
        }
    }

    fn cmp_id(&mut self, ctx: &MatchCtx, concrete_id: Id, pat_id: Id) {
        match ctx.pat_ctx.get_pattern(pat_id) {
            Pattern::Pattern(var) => {
                self.add_binding(ctx, &var, concrete_id);
            }
            Pattern::Exact(pat) => self.cmp_nodes(ctx.ast_ctx.get_node(concrete_id), pat),
        }
    }

    fn cmp_nodes(&mut self, concrete: &Node, pat: &Node) {
        if concrete.kind() == pat.kind() {
            Node::cmp_equal_kinds(self, concrete, pat);
        } else {
            self.no_match()
        }
    }
}

#[derive(Debug)]
struct Comparison {
    concrete: Id,
    pat: Id,
}

#[derive(Debug, Default)]
pub(crate) struct Match {
    bindings: HashMap<SynVar, Option<Id>>,
    cmps: Vec<Comparison>,
    valid: bool,
}

impl Match {
    fn new(vars: &[SynVarDecl]) -> Self {
        Self {
            bindings: vars
                .iter()
                .map(|var| {
                    (
                        SynVar {
                            name: var.name.clone(),
                        },
                        var.node,
                    )
                })
                .collect(),
            cmps: vec![],
            valid: true,
        }
    }

    fn cmp<T: CmpDirect>(&mut self, concrete: NodeId<T>, pat: NodeId<T>) {
        self.cmps.push(Comparison {
            concrete: concrete.untyped(),
            pat: pat.untyped(),
        });
    }
}

pub(crate) struct MatchResult {
    pub matches: Matches,
    pub _ctx: AstCtx,
}

impl Spec {
    pub(crate) fn find_node_for_var(&self, var: &SynVar) -> Id {
        self.vars
            .iter()
            .find(|v| v.name.to_string() == var.name)
            .unwrap()
            .node
            .unwrap()
    }

    pub(crate) fn match_pattern(&self, ast: Ast, pat_ctx: PatCtx, pat_id: Id) -> MatchResult {
        let ctx = MatchCtx::new(pat_ctx, ast.ctx);
        let mut matches = Matches::new(&self.vars);
        for item in ctx.ast_ctx.iter() {
            matches.add_todo(item, pat_id);
        }
        matches.run(&ctx);
        let result = MatchResult {
            matches,
            _ctx: ctx.ast_ctx,
        };
        for match_ in result.matches.iter() {
            for (key, value) in match_.bindings.iter() {
                println!("{:?} {:?}", key, value);
            }
        }
        result
    }
}

pub(crate) trait CmpDirect {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self);
}

impl CmpDirect for grammar::Item {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
        match self {
            grammar::Item::Const(node_id) => {
                if let grammar::Item::Const(pat) = pat {
                    matches.cmp(*node_id, *pat);
                } else {
                    matches.no_match()
                }
            }
            _ => todo!(), // grammar::Item::Enum(item_enum) => todo!(),
                          // grammar::Item::ExternCrate(item_extern_crate) => todo!(),
                          // grammar::Item::Fn(item_fn) => todo!(),
                          // grammar::Item::ForeignMod(item_foreign_mod) => todo!(),
                          // grammar::Item::Impl(item_impl) => todo!(),
                          // grammar::Item::Macro(item_macro) => todo!(),
                          // grammar::Item::Mod(item_mod) => todo!(),
                          // grammar::Item::Static(item_static) => todo!(),
                          // grammar::Item::Struct(item_struct) => todo!(),
                          // grammar::Item::Trait(item_trait) => todo!(),
                          // grammar::Item::TraitAlias(item_trait_alias) => todo!(),
                          // grammar::Item::Type(item_type) => todo!(),
                          // grammar::Item::Union(item_union) => todo!(),
                          // grammar::Item::Use(item_use) => todo!(),
        }
        todo!()
    }
}

impl CmpDirect for Ident {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
        matches.eq(&self.to_string(), &pat.to_string())
    }
}

impl CmpDirect for Lit {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
        let cmp_bool = || {
            match self {
                syn::Lit::Str(s1) => {
                    if let syn::Lit::Str(s2) = pat {
                        return s1.value() == s2.value();
                    }
                }
                syn::Lit::ByteStr(s1) => {
                    if let syn::Lit::ByteStr(s2) = pat {
                        return s1.value() == s2.value();
                    }
                }
                syn::Lit::CStr(s1) => {
                    if let syn::Lit::CStr(s2) = pat {
                        return s1.value() == s2.value();
                    }
                }
                syn::Lit::Byte(s1) => {
                    if let syn::Lit::Byte(s2) = pat {
                        return s1.value() == s2.value();
                    }
                }
                syn::Lit::Char(s1) => {
                    if let syn::Lit::Char(s2) = pat {
                        return s1.value() == s2.value();
                    }
                }
                syn::Lit::Int(s1) => {
                    if let syn::Lit::Int(s2) = pat {
                        return s1.base10_digits() == s2.base10_digits();
                    }
                }
                syn::Lit::Float(s1) => {
                    if let syn::Lit::Float(s2) = pat {
                        return s1.base10_digits() == s2.base10_digits();
                    }
                }
                syn::Lit::Bool(s1) => {
                    if let syn::Lit::Bool(s2) = pat {
                        return s1.value() == s2.value();
                    }
                }
                _ => todo!(),
            }
            false
        };
        matches.check(cmp_bool())
    }
}

impl CmpDirect for Expr {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
        match self {
            Expr::Binary(i1) => {
                if let grammar::Expr::Binary(i2) = pat {
                    matches.cmp(*i1, *i2);
                    return;
                }
            }
            Expr::Unary(i1) => {
                if let grammar::Expr::Unary(i2) = pat {
                    matches.cmp(*i1, *i2);
                    return;
                }
            }
            Expr::Lit(i1) => {
                if let grammar::Expr::Lit(i2) = pat {
                    matches.cmp(*i1, *i2);
                    return;
                }
            }
            _ => todo!(),
        }
        matches.no_match()
    }
}

impl CmpDirect for ExprBinary {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
        let Self {
            _attrs: _,
            left,
            op,
            right,
        } = self;
        matches.cmp_direct(op, &pat.op);
        op.cmp_direct(matches, &pat.op);
        matches.cmp(*left, pat.left);
        matches.cmp(*right, pat.right);
    }
}

impl CmpDirect for ExprUnary {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
        let Self {
            _attrs: _,
            expr,
            op,
        } = self;
        matches.cmp_direct(op, &pat.op);
        matches.cmp(*expr, pat.expr);
    }
}

impl CmpDirect for ExprLit {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
        let Self { _attrs: _, lit } = self;
        matches.cmp(*lit, pat.lit);
    }
}

impl CmpDirect for ItemConst {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
        // self.vis.match_pattern(&item.vis)?;
        matches.cmp(self.ident, pat.ident);
        // self.generics.match_pattern(&item.generics)?;
        // self.ty.match_pattern(&item.ty)?;
        matches.cmp(self.expr, pat.expr);
    }
}

impl CmpDirect for syn::BinOp {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
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
        matches.check(is_match)
    }
}

impl CmpDirect for syn::UnOp {
    fn cmp_direct(&self, matches: &mut Matches, pat: &Self) {
        let is_match = match self {
            syn::UnOp::Deref(_) => matches!(pat, syn::UnOp::Deref(_)),
            syn::UnOp::Not(_) => matches!(pat, syn::UnOp::Not(_)),
            syn::UnOp::Neg(_) => matches!(pat, syn::UnOp::Neg(_)),
            _ => todo!(),
        };
        matches.check(is_match)
    }
}
