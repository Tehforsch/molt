use crate::{
    ctx::{AstCtx, Ctx, Id, NodeId, PatCtx},
    grammar::{
        self, AsNode, Expr, ExprBinary, ExprLit, ExprUnary, Ident, Item, ItemConst, Lit, Node,
    },
    mangle::Pattern,
    spec::{SynVar, SynVarDecl},
    Ast, Spec,
};

struct MatchCtx {
    pat_ctx: PatCtx,
    ast_ctx: AstCtx,
}

struct Matches {
    match_: MatchData,
}

impl MatchCtx {
    fn new(pat_ctx: PatCtx, ast_ctx: AstCtx) -> Self {
        Self { pat_ctx, ast_ctx }
    }
}

impl Matches {
    fn new(vars: &[SynVarDecl]) -> Self {
        Self {
            match_: MatchData::new(&vars),
        }
    }

    fn add_binding(&mut self, key: &SynVar, value: Id) {
        if self
            .match_
            .bindings
            .iter()
            .any(|binding| &binding.key == key)
        {
            todo!()
        }
        self.match_.bindings.push(Binding {
            value,
            key: key.clone(),
        });
    }
}

#[derive(Debug, Default)]
pub(crate) struct MatchData {
    bindings: Vec<Binding>,
}
impl MatchData {
    fn new(vars: &[SynVarDecl]) -> Self {
        Self {
            bindings: vars
                .iter()
                .filter_map(|var| {
                    var.node.map(|node| Binding {
                        key: SynVar {
                            name: var.name.clone(),
                        },
                        value: node,
                    })
                })
                .collect(),
        }
    }
}

#[derive(Debug)]
struct Binding {
    key: SynVar,
    value: Id,
}

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
        let ctx = MatchCtx::new(pat_ctx, ast.ctx);
        let mut matches = Matches::new(&self.vars);
        let pat = ctx.pat_ctx.get_node(id).unwrap();
        let item = ctx.ast_ctx.get(item);
        if let Item::Const(ref item) = item {
            let item = ctx.ast_ctx.get(*item);
            if let Node::Const(ref pat) = pat {
                item.cmp_direct(&ctx, &mut matches, pat)
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
    fn cmp_direct(&self, ctx: &MatchCtx, matches: &mut Matches, pat: &NodeId<T>) -> MatchResult {
        let concrete = ctx.ast_ctx.get(*self);
        let pat = ctx.pat_ctx.get_pattern(*pat);
        match pat {
            Pattern::Pattern(var) => {
                matches.add_binding(&var, self.untyped());
                Ok(Match)
            }
            Pattern::Exact(pat) => concrete.cmp_direct(ctx, matches, pat),
        }
    }
}

trait CmpDirect {
    fn cmp_direct(&self, ctx: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult;
}

impl CmpDirect for grammar::Item {
    fn cmp_direct(&self, ctx: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
        match self {
            grammar::Item::Const(node_id) => {
                if let grammar::Item::Const(pat) = pat {
                    return node_id.cmp_direct(ctx, matches, pat);
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
    fn cmp_direct(&self, _: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
        Match::is_eq(&self.to_string(), &pat.to_string())
    }
}

impl CmpDirect for Lit {
    fn cmp_direct(&self, _: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
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
    fn cmp_direct(&self, ctx: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
        match self {
            Expr::Binary(i1) => {
                if let grammar::Expr::Binary(i2) = pat {
                    i1.cmp_direct(ctx, matches, i2)
                } else {
                    Err(NoMatch)
                }
            }
            Expr::Unary(i1) => {
                if let grammar::Expr::Unary(i2) = pat {
                    i1.cmp_direct(ctx, matches, i2)
                } else {
                    Err(NoMatch)
                }
            }
            Expr::Lit(i1) => {
                if let grammar::Expr::Lit(i2) = pat {
                    i1.cmp_direct(ctx, matches, i2)
                } else {
                    Err(NoMatch)
                }
            }
            _ => todo!(),
        }
    }
}

impl CmpDirect for ExprBinary {
    fn cmp_direct(&self, ctx: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
        let Self {
            _attrs: _,
            left,
            op,
            right,
        } = self;
        op.cmp_direct(ctx, matches, &pat.op)?;
        left.cmp_direct(ctx, matches, &pat.left)?;
        right.cmp_direct(ctx, matches, &pat.right)?;
        Ok(Match)
    }
}

impl CmpDirect for ExprUnary {
    fn cmp_direct(&self, ctx: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
        let Self {
            _attrs: _,
            expr,
            op,
        } = self;
        op.cmp_direct(ctx, matches, &pat.op)?;
        expr.cmp_direct(ctx, matches, &pat.expr)?;
        Ok(Match)
    }
}

impl CmpDirect for ExprLit {
    fn cmp_direct(&self, ctx: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
        let Self { _attrs: _, lit } = self;
        lit.cmp_direct(ctx, matches, &pat.lit)?;
        Ok(Match)
    }
}

impl CmpDirect for ItemConst {
    fn cmp_direct(&self, ctx: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
        // self.vis.match_pattern(&item.vis)?;
        self.ident.cmp_direct(ctx, matches, &pat.ident)?;
        // self.generics.match_pattern(&item.generics)?;
        // self.ty.match_pattern(&item.ty)?;
        self.expr.cmp_direct(ctx, matches, &pat.expr)?;
        Ok(Match)
    }
}

impl CmpDirect for syn::BinOp {
    fn cmp_direct(&self, _: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
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
    fn cmp_direct(&self, _: &MatchCtx, matches: &mut Matches, pat: &Self) -> MatchResult {
        let is_match = match self {
            syn::UnOp::Deref(_) => matches!(pat, syn::UnOp::Deref(_)),
            syn::UnOp::Not(_) => matches!(pat, syn::UnOp::Not(_)),
            syn::UnOp::Neg(_) => matches!(pat, syn::UnOp::Neg(_)),
            _ => todo!(),
        };
        Match::from_bool(is_match)
    }
}
