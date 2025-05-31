use crate::{
    MoltFile,
    ctx::{AstCtx, Id, MatchCtx, MatchingMode, NodeId, NodeList, PatCtx},
    parser::{
        Node, Pattern, VarDecl, VarId,
        rust_grammar::{
            Attribute, Expr, ExprBinary, ExprLit, ExprParen, ExprUnary, Ident, Item, ItemConst,
            ItemFn, Lit,
        },
    },
};

use crate::match_pattern::Match;

pub(crate) trait CmpSyn {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self);
}

impl CmpSyn for Expr {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        match (self, pat) {
            (Expr::Binary(i1), Expr::Binary(i2)) => ctx.cmp_syn(i1, i2),
            (Expr::Unary(i1), Expr::Unary(i2)) => ctx.cmp_syn(i1, i2),
            (Expr::Lit(i1), Expr::Lit(i2)) => ctx.cmp_syn(i1, i2),
            (Expr::Paren(i1), Expr::Paren(i2)) => ctx.cmp_syn(i1, i2),
            (Expr::Binary(_), _)
            | (Expr::Unary(_), _)
            | (Expr::Lit(_), _)
            | (Expr::Paren(_), _) => ctx.no_match(),
            _ => {
                todo!()
            }
        }
    }
}

impl CmpSyn for ExprBinary {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_syn(&self.op, &pat.op);
        ctx.cmp_syn(&*self.left, &*pat.left);
        ctx.cmp_syn(&*self.right, &*pat.right);
    }
}

impl CmpSyn for ExprUnary {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_syn(&self.op, &pat.op);
        ctx.cmp_syn(&*self.expr, &*pat.expr);
    }
}

impl CmpSyn for ExprLit {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_syn(&self.lit, &pat.lit);
    }
}

impl CmpSyn for ExprParen {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_syn(&*self.expr, &*pat.expr);
    }
}

impl CmpSyn for syn::BinOp {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
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

impl CmpSyn for syn::UnOp {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        let is_match = match self {
            syn::UnOp::Deref(_) => matches!(pat, syn::UnOp::Deref(_)),
            syn::UnOp::Not(_) => matches!(pat, syn::UnOp::Not(_)),
            syn::UnOp::Neg(_) => matches!(pat, syn::UnOp::Neg(_)),
            _ => todo!(),
        };
        ctx.check(is_match)
    }
}

impl CmpSyn for ItemFn {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        todo!()
    }
}

impl CmpSyn for syn::Lit {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        let b = match (self, pat) {
            (syn::Lit::Str(s1), syn::Lit::Str(s2)) => s1.value() == s2.value(),
            (syn::Lit::ByteStr(s1), syn::Lit::ByteStr(s2)) => s1.value() == s2.value(),
            (syn::Lit::CStr(s1), syn::Lit::CStr(s2)) => s1.value() == s2.value(),
            (syn::Lit::Byte(s1), syn::Lit::Byte(s2)) => s1.value() == s2.value(),
            (syn::Lit::Char(s1), syn::Lit::Char(s2)) => s1.value() == s2.value(),
            (syn::Lit::Int(s1), syn::Lit::Int(s2)) => s1.base10_digits() == s2.base10_digits(),
            (syn::Lit::Float(s1), syn::Lit::Float(s2)) => s1.base10_digits() == s2.base10_digits(),
            (syn::Lit::Bool(s1), syn::Lit::Bool(s2)) => s1.value() == s2.value(),
            (syn::Lit::Verbatim(_), syn::Lit::Verbatim(_)) => todo!(),
            _ => false,
        };
        ctx.check(b)
    }
}

impl CmpSyn for Ident {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.eq(&self, &pat)
    }
}

impl CmpSyn for Lit {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_syn(self.inner(), pat.inner())
    }
}

impl CmpSyn for Attribute {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {}
}
