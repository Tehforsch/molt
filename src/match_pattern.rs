use crate::{
    ctx::{AstCtx, Ctx, Id, NodeId, PatCtx},
    grammar::{self, AsNode, GetKind, Node},
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

    pub(crate) fn transform(&self, _ast: Ast, _input: &Node, _output: &Node) -> Ast {
        todo!()
    }

    pub(crate) fn match_pattern(&self, ast: Ast, pat_ctx: Ctx, id: Id) -> MatchResult {
        let item = *ast.items.first().unwrap();
        let ctx = MatchCtx::new(self.vars.clone(), pat_ctx, ast.ctx);
        let pat = ctx.pat_ctx.get_node(id).unwrap();
        match pat {
            Node::Item(s) => cmp_direct_node_id::<grammar::Item>(&ctx, item, ctx.pat_ctx.typed(id)),
            _ => todo!(), // Node::Const(s) => cmp_direct_node_id::<grammar::ItemConst>(&ctx, item, ctx.typed(id)),
                          // Node::Ident(s) => cmp_direct_node_id::<grammar::Ident>(&ctx, item, ctx.typed(id)),
                          // Node::Expr(s) => cmp_direct_node_id::<grammar::Expr>(&ctx, item, ctx.typed(id)),
                          // Node::Lit(s) => cmp_direct_node_id::<grammar::Lit>(&ctx, item, ctx.typed(id)),
                          // Node::ExprBinary(s) => cmp_direct_node_id::<grammar::ExprBinary>(&ctx, item, ctx.typed(id)),
                          // Node::ExprUnary(s) => cmp_direct_node_id::<grammar::ExprUnary>(&ctx, item, ctx.typed(id)),
                          // Node::ExprLit(s) => cmp_direct_node_id::<grammar::ExprLit>(&ctx, item, ctx.typed(id)),
        }
    }
}

fn cmp_direct_node_id<T: MatchP>(
    ctx: &MatchCtx,
    concrete: NodeId<T>,
    pat: NodeId<T>,
) -> MatchResult {
    let concrete = ctx.ast_ctx.get(concrete);
    let pat = ctx.pat_ctx.get(pat);
    match pat {
        None => todo!(), // take care of syn vars
        Some(pat) => concrete.cmp_direct(ctx, pat),
    }
}

pub(crate) trait MatchP: GetKind + AsNode {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &Self) -> MatchResult;
}

impl MatchP for grammar::Item {
    fn cmp_direct(&self, ctx: &MatchCtx, pat: &Self) -> MatchResult {
        todo!()
        // match self {
        //     grammar::Item::Const(node_id) => todo!(),
        //     grammar::Item::Enum(item_enum) => todo!(),
        //     grammar::Item::ExternCrate(item_extern_crate) => todo!(),
        //     grammar::Item::Fn(item_fn) => todo!(),
        //     grammar::Item::ForeignMod(item_foreign_mod) => todo!(),
        //     grammar::Item::Impl(item_impl) => todo!(),
        //     grammar::Item::Macro(item_macro) => todo!(),
        //     grammar::Item::Mod(item_mod) => todo!(),
        //     grammar::Item::Static(item_static) => todo!(),
        //     grammar::Item::Struct(item_struct) => todo!(),
        //     grammar::Item::Trait(item_trait) => todo!(),
        //     grammar::Item::TraitAlias(item_trait_alias) => todo!(),
        //     grammar::Item::Type(item_type) => todo!(),
        //     grammar::Item::Union(item_union) => todo!(),
        //     grammar::Item::Use(item_use) => todo!(),
        // }
    }
}

// trait MatchPattern<Pat>: Sized {
//     fn match_pattern(&self, ctx: &mut MatchCtx, t: &Pat) -> MatchResult;

//     fn cmp_pat(&self, ctx: &mut MatchCtx, pat: &Pattern<Pat>) -> MatchResult {
//         match pat {
//             Pattern::Exact(t) => self.match_pattern(ctx, t),
//             Pattern::Pattern(var) => {
//                 ctx.add_binding(self, var);
//                 Ok(Match)
//             }
//         }
//     }
// }

// impl MatchPattern<Node> for grammar::Item {
//     fn match_pattern(&self, ctx: &mut MatchCtx, node: &Node) -> MatchResult {
//         match node {
//             Node::Const(item_const) => self.match_pattern(ctx, item_const),
//             Node::Expr(expr) => self.match_pattern(ctx, expr),
//             Node::Ident(_) => todo!(),
//             Node::Lit(_) => todo!(),
//         }
//     }
// }

// impl MatchPattern<grammar::ItemConst> for syn::Item {
//     fn match_pattern(&self, ctx: &mut MatchCtx, item: &grammar::ItemConst) -> MatchResult {
//         match self {
//             syn::Item::Const(item_const) => item_const.match_pattern(ctx, item),
//             _ => Err(NoMatch),
//         }
//     }
// }

// impl MatchPattern<grammar::Expr> for syn::Item {
//     fn match_pattern(&self, ctx: &mut MatchCtx, item: &grammar::Expr) -> MatchResult {
//         match self {
//             syn::Item::Const(const_) => const_.expr.match_pattern(ctx, item),
//             _ => Err(NoMatch),
//         }
//     }
// }

// impl MatchPattern<syn::Ident> for grammar::Ident {
//     fn match_pattern(&self, _: &mut MatchCtx, t: &grammar::Ident) -> MatchResult {
//         Match::is_eq(&self.to_string(), &t.to_string())
//     }
// }

// impl MatchPattern<syn::Lit> for syn::Lit {
//     fn match_pattern(&self, _: &mut MatchCtx, t: &syn::Lit) -> MatchResult {
//         let cmp_bool = || {
//             match self {
//                 syn::Lit::Str(s1) => {
//                     if let syn::Lit::Str(s2) = t {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::ByteStr(s1) => {
//                     if let syn::Lit::ByteStr(s2) = t {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::CStr(s1) => {
//                     if let syn::Lit::CStr(s2) = t {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::Byte(s1) => {
//                     if let syn::Lit::Byte(s2) = t {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::Char(s1) => {
//                     if let syn::Lit::Char(s2) = t {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 syn::Lit::Int(s1) => {
//                     if let syn::Lit::Int(s2) = t {
//                         return s1.base10_digits() == s2.base10_digits();
//                     }
//                 }
//                 syn::Lit::Float(s1) => {
//                     if let syn::Lit::Float(s2) = t {
//                         return s1.base10_digits() == s2.base10_digits();
//                     }
//                 }
//                 syn::Lit::Bool(s1) => {
//                     if let syn::Lit::Bool(s2) = t {
//                         return s1.value() == s2.value();
//                     }
//                 }
//                 _ => todo!(),
//             }
//             false
//         };
//         Match::from_bool(cmp_bool())
//     }
// }

// impl MatchPattern<grammar::Expr> for syn::Expr {
//     fn match_pattern(&self, ctx: &mut MatchCtx, t: &grammar::Expr) -> MatchResult {
//         match self {
//             syn::Expr::Binary(i1) => {
//                 if let grammar::Expr::Binary(i2) = t {
//                     i1.match_pattern(ctx, i2)
//                 } else {
//                     Err(NoMatch)
//                 }
//             }
//             syn::Expr::Unary(i1) => {
//                 if let grammar::Expr::Unary(i2) = t {
//                     i1.match_pattern(ctx, i2)
//                 } else {
//                     Err(NoMatch)
//                 }
//             }
//             syn::Expr::Lit(i1) => {
//                 if let grammar::Expr::Lit(i2) = t {
//                     i1.match_pattern(ctx, i2)
//                 } else {
//                     Err(NoMatch)
//                 }
//             }
//             _ => todo!(),
//         }
//     }
// }

// impl MatchPattern<grammar::ExprBinary> for syn::ExprBinary {
//     fn match_pattern(&self, ctx: &mut MatchCtx, t: &grammar::ExprBinary) -> MatchResult {
//         let Self {
//             attrs: _,
//             left,
//             op,
//             right,
//         } = self;
//         op.match_pattern(ctx, &t.op)?;
//         left.cmp_pat(ctx, &t.left)?;
//         right.cmp_pat(ctx, &t.right)?;
//         Ok(Match)
//     }
// }

// impl MatchPattern<grammar::ExprUnary> for syn::ExprUnary {
//     fn match_pattern(&self, ctx: &mut MatchCtx, t: &grammar::ExprUnary) -> MatchResult {
//         let Self { attrs: _, expr, op } = self;
//         op.match_pattern(ctx, &t.op)?;
//         expr.cmp_pat(ctx, &t.expr)?;
//         Ok(Match)
//     }
// }

// impl MatchPattern<grammar::ExprLit> for syn::ExprLit {
//     fn match_pattern(&self, ctx: &mut MatchCtx, t: &grammar::ExprLit) -> MatchResult {
//         let Self { attrs: _, lit } = self;
//         lit.cmp_pat(ctx, &t.lit)?;
//         Ok(Match)
//     }
// }

// impl MatchPattern<syn::BinOp> for syn::BinOp {
//     fn match_pattern(&self, _: &mut MatchCtx, t: &syn::BinOp) -> MatchResult {
//         let is_match = match self {
//             syn::BinOp::Add(_) => matches!(t, syn::BinOp::Add(_)),
//             syn::BinOp::Sub(_) => matches!(t, syn::BinOp::Sub(_)),
//             syn::BinOp::Mul(_) => matches!(t, syn::BinOp::Mul(_)),
//             syn::BinOp::Div(_) => matches!(t, syn::BinOp::Div(_)),
//             syn::BinOp::Rem(_) => matches!(t, syn::BinOp::Rem(_)),
//             syn::BinOp::And(_) => matches!(t, syn::BinOp::And(_)),
//             syn::BinOp::Or(_) => matches!(t, syn::BinOp::Or(_)),
//             syn::BinOp::BitXor(_) => matches!(t, syn::BinOp::BitXor(_)),
//             syn::BinOp::BitAnd(_) => matches!(t, syn::BinOp::BitAnd(_)),
//             syn::BinOp::BitOr(_) => matches!(t, syn::BinOp::BitOr(_)),
//             syn::BinOp::Shl(_) => matches!(t, syn::BinOp::Shl(_)),
//             syn::BinOp::Shr(_) => matches!(t, syn::BinOp::Shr(_)),
//             syn::BinOp::Eq(_) => matches!(t, syn::BinOp::Eq(_)),
//             syn::BinOp::Lt(_) => matches!(t, syn::BinOp::Lt(_)),
//             syn::BinOp::Le(_) => matches!(t, syn::BinOp::Le(_)),
//             syn::BinOp::Ne(_) => matches!(t, syn::BinOp::Ne(_)),
//             syn::BinOp::Ge(_) => matches!(t, syn::BinOp::Ge(_)),
//             syn::BinOp::Gt(_) => matches!(t, syn::BinOp::Gt(_)),
//             syn::BinOp::AddAssign(_) => matches!(t, syn::BinOp::AddAssign(_)),
//             syn::BinOp::SubAssign(_) => matches!(t, syn::BinOp::SubAssign(_)),
//             syn::BinOp::MulAssign(_) => matches!(t, syn::BinOp::MulAssign(_)),
//             syn::BinOp::DivAssign(_) => matches!(t, syn::BinOp::DivAssign(_)),
//             syn::BinOp::RemAssign(_) => matches!(t, syn::BinOp::RemAssign(_)),
//             syn::BinOp::BitXorAssign(_) => matches!(t, syn::BinOp::BitXorAssign(_)),
//             syn::BinOp::BitAndAssign(_) => matches!(t, syn::BinOp::BitAndAssign(_)),
//             syn::BinOp::BitOrAssign(_) => matches!(t, syn::BinOp::BitOrAssign(_)),
//             syn::BinOp::ShlAssign(_) => matches!(t, syn::BinOp::ShlAssign(_)),
//             syn::BinOp::ShrAssign(_) => matches!(t, syn::BinOp::ShrAssign(_)),
//             _ => todo!(),
//         };
//         Match::from_bool(is_match)
//     }
// }

// impl MatchPattern<syn::UnOp> for syn::UnOp {
//     fn match_pattern(&self, _: &mut MatchCtx, t: &syn::UnOp) -> MatchResult {
//         let is_match = match self {
//             syn::UnOp::Deref(_) => matches!(t, syn::UnOp::Deref(_)),
//             syn::UnOp::Not(_) => matches!(t, syn::UnOp::Not(_)),
//             syn::UnOp::Neg(_) => matches!(t, syn::UnOp::Neg(_)),
//             _ => todo!(),
//         };
//         Match::from_bool(is_match)
//     }
// }

// impl MatchPattern<grammar::ItemConst> for syn::ItemConst {
//     fn match_pattern(&self, ctx: &mut MatchCtx, item: &grammar::ItemConst) -> MatchResult {
//         // self.vis.match_pattern(&item.vis)?;
//         self.ident.cmp_pat(ctx, &item.ident)?;
//         // self.generics.match_pattern(&item.generics)?;
//         // self.ty.match_pattern(&item.ty)?;
//         (*self.expr).cmp_pat(ctx, &item.expr)?;
//         Ok(Match)
//     }
// }
