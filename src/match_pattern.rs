use crate::{
    ctx::{AstCtx, Ctx, Id, NodeId, PatCtx},
    grammar::{
        self, AsNode, Expr, ExprBinary, ExprLit, ExprUnary, GetKind, Ident, Item, ItemConst, Lit,
        Node,
    },
    spec::{SynVar, SynVarDecl},
    Ast, Spec,
};

struct MatchCtx {
    pat_ctx: PatCtx,
    ast_ctx: AstCtx,
    // matches: Vec<MatchData>,
    // current_match: MatchData,
    // vars: Vec<SynVarDecl>,
}

impl MatchCtx {
    fn new(_vars: Vec<SynVarDecl>, pat_ctx: PatCtx, ast_ctx: AstCtx) -> Self {
        Self {
            pat_ctx,
            ast_ctx,
            // matches: vec![],
            // current_match: MatchData::default(),
            // vars,
        }
    }
}

//     fn add_binding<T>(&mut self, arg: &T, var: &SynVar) {
//         // Somehow record the fact that arg needs to match the pattern
//         // contained in var. If var has a node, they need to be compared.
//         // Afterwards, even if it does not have a node, record the fact that
//         // `var` is now bound to `arg`, so that all further comparisons with
//         // var match against arg.
//         todo!()
//     }
// }

// #[derive(Debug, Default)]
// pub(crate) struct MatchData {
//     bindings: Vec<Binding>,
// }

// #[derive(Debug)]
// struct Binding {}

#[derive(Debug)]
pub(crate) struct Match;

#[derive(Debug)]
pub(crate) struct NoMatch;

pub(crate) type MatchResult = Result<Match, NoMatch>;

impl Match {
    fn is_eq<T: PartialEq<T>>(t1: &T, t2: &T) -> MatchResult {
        Match::from_bool(t1 == t2)
    }

    fn from_bool(b: bool) -> MatchResult {
        if b {
            Ok(Match)
        } else {
            Err(NoMatch)
        }
    }
}

impl Spec {
    pub(crate) fn find_var(&self, var: &SynVar) -> Id {
        self.vars
            .iter()
            .find(|v| v.name.to_string() == var.name)
            .unwrap()
            .node
            .unwrap()
    }

    pub(crate) fn match_pattern(&self, ast: Ast, pat_ctx: Ctx, id: Id) -> MatchResult {
        let item = *ast.items.first().unwrap();
        let ctx = MatchCtx::new(self.vars.clone(), pat_ctx, ast.ctx);
        let pat = ctx.pat_ctx.get_node(id).unwrap();
        let item = ctx.ast_ctx.get(item);
        if let Node::Const(ref pat) = pat {
            if let Item::Const(ref item) = item {
                let item = ctx.ast_ctx.get(*item);
                return item.cmp_direct(&ctx, pat);
            }
        }
        todo!()
        // match pat {
        // todo
        // Node::Item(s) => item.cmp_indirect(&ctx, &ctx.pat_ctx.typed(id)),
        // Node::Const(s) => item.cmp_indirect(&ctx, ctx.pat_ctx.get_node(id)),
        // Node::Ident(s) => cmp_direct_node_id::<grammar::Ident>(&ctx, item, ctx.typed(id)),
        // Node::Expr(s) => cmp_direct_node_id::<grammar::Expr>(&ctx, item, ctx.typed(id)),
        // Node::Lit(s) => cmp_direct_node_id::<grammar::Lit>(&ctx, item, ctx.typed(id)),
        // Node::ExprBinary(s) => cmp_direct_node_id::<grammar::ExprBinary>(&ctx, item, ctx.typed(id)),
        // Node::ExprUnary(s) => cmp_direct_node_id::<grammar::ExprUnary>(&ctx, item, ctx.typed(id)),
        // Node::ExprLit(s) => cmp_direct_node_id::<grammar::ExprLit>(&ctx, item, ctx.typed(id)),
        // }
    }
}

impl<T: CmpDirect + AsNode> CmpDirect for NodeId<T> {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &NodeId<T>) -> MatchResult {
        let concrete = ctx.ast_ctx.get(*self);
        let pat = ctx.pat_ctx.get(*pat);
        match pat {
            None => Ok(Match), // TODO: take care of syn vars
            Some(pat) => concrete.cmp_direct(ctx, pat),
        }
    }
}

trait CmpDirect {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &Self) -> MatchResult;
}

// trait CmpSub {
// fn cmp_sub(&self, ctx: &MatchCtx, pat: &Node) -> MatchResult;
// }

impl CmpDirect for grammar::Item {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &Self) -> MatchResult {
        match self {
            grammar::Item::Const(node_id) => {
                if let grammar::Item::Const(pat) = pat {
                    return node_id.cmp_direct(ctx, pat);
                }
            }
            grammar::Item::Enum(item_enum) => todo!(),
            grammar::Item::ExternCrate(item_extern_crate) => todo!(),
            grammar::Item::Fn(item_fn) => todo!(),
            grammar::Item::ForeignMod(item_foreign_mod) => todo!(),
            grammar::Item::Impl(item_impl) => todo!(),
            grammar::Item::Macro(item_macro) => todo!(),
            grammar::Item::Mod(item_mod) => todo!(),
            grammar::Item::Static(item_static) => todo!(),
            grammar::Item::Struct(item_struct) => todo!(),
            grammar::Item::Trait(item_trait) => todo!(),
            grammar::Item::TraitAlias(item_trait_alias) => todo!(),
            grammar::Item::Type(item_type) => todo!(),
            grammar::Item::Union(item_union) => todo!(),
            grammar::Item::Use(item_use) => todo!(),
        }
        todo!()
    }
}

impl CmpDirect for Ident {
    fn cmp_direct(&self, _: &MatchCtx, pat: &Self) -> MatchResult {
        Match::is_eq(&self.to_string(), &pat.to_string())
    }
}

impl CmpDirect for Lit {
    fn cmp_direct(&self, _: &MatchCtx, pat: &Self) -> MatchResult {
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
        Match::from_bool(cmp_bool())
    }
}

impl CmpDirect for Expr {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &Self) -> MatchResult {
        match self {
            Expr::Binary(i1) => {
                if let grammar::Expr::Binary(i2) = pat {
                    i1.cmp_direct(ctx, i2)
                } else {
                    Err(NoMatch)
                }
            }
            Expr::Unary(i1) => {
                if let grammar::Expr::Unary(i2) = pat {
                    i1.cmp_direct(ctx, i2)
                } else {
                    Err(NoMatch)
                }
            }
            Expr::Lit(i1) => {
                if let grammar::Expr::Lit(i2) = pat {
                    i1.cmp_direct(ctx, i2)
                } else {
                    Err(NoMatch)
                }
            }
            _ => todo!(),
        }
    }
}

impl CmpDirect for ExprBinary {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &Self) -> MatchResult {
        let Self {
            _attrs: _,
            left,
            op,
            right,
        } = self;
        op.cmp_direct(ctx, &pat.op)?;
        left.cmp_direct(ctx, &pat.left)?;
        right.cmp_direct(ctx, &pat.right)?;
        Ok(Match)
    }
}

impl CmpDirect for ExprUnary {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &Self) -> MatchResult {
        let Self {
            _attrs: _,
            expr,
            op,
        } = self;
        op.cmp_direct(ctx, &pat.op)?;
        expr.cmp_direct(ctx, &pat.expr)?;
        Ok(Match)
    }
}

impl CmpDirect for ExprLit {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &Self) -> MatchResult {
        let Self { _attrs: _, lit } = self;
        lit.cmp_direct(ctx, &pat.lit)?;
        Ok(Match)
    }
}

impl CmpDirect for ItemConst {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &Self) -> MatchResult {
        // self.vis.match_pattern(&item.vis)?;
        self.ident.cmp_direct(ctx, &pat.ident)?;
        // self.generics.match_pattern(&item.generics)?;
        // self.ty.match_pattern(&item.ty)?;
        self.expr.cmp_direct(ctx, &pat.expr)?;
        Ok(Match)
    }
}

impl CmpDirect for syn::BinOp {
    fn cmp_direct(&self, _: &MatchCtx, pat: &Self) -> MatchResult {
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
        Match::from_bool(is_match)
    }
}

impl CmpDirect for syn::UnOp {
    fn cmp_direct(&self, _: &MatchCtx, pat: &Self) -> MatchResult {
        let is_match = match self {
            syn::UnOp::Deref(_) => matches!(pat, syn::UnOp::Deref(_)),
            syn::UnOp::Not(_) => matches!(pat, syn::UnOp::Not(_)),
            syn::UnOp::Neg(_) => matches!(pat, syn::UnOp::Neg(_)),
            _ => todo!(),
        };
        Match::from_bool(is_match)
    }
}
