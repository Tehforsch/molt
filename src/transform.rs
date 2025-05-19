use crate::{
    grammar::{self, Node, Pattern},
    rust_ast::RustAst,
    Transformation,
};

#[derive(Debug)]
struct Match;

#[derive(Debug)]
struct NoMatch;

type MatchResult = Result<Match, NoMatch>;

impl Match {
    fn from_bool(b: bool) -> Result<Match, NoMatch> {
        if b {
            Ok(Match)
        } else {
            Err(NoMatch)
        }
    }
}

impl Transformation {
    pub(crate) fn transform(&self, ast: RustAst) -> RustAst {
        let node = self.top_level_node();
        match ast.match_pattern(node) {
            Ok(_) => println!("Match"),
            Err(_) => println!("No match!"),
        }
        ast
    }
}

trait MatchPattern<Pat> {
    fn match_pattern(&self, t: &Pat) -> MatchResult;

    fn cmp_pat(&self, pat: &Pattern<Pat>) -> MatchResult {
        match pat {
            Pattern::Exact(t) => self.match_pattern(t),
            Pattern::Pattern(_) => {
                // TODO record constraint
                Ok(Match)
            }
        }
    }
}

impl MatchPattern<Node> for RustAst {
    fn match_pattern(&self, node: &Node) -> MatchResult {
        // todo multiple items
        let item = self.file.items.first().unwrap();
        match node {
            Node::Const(item_const) => item.match_pattern(item_const),
            Node::Ident(_) => todo!(),
            Node::Expr(expr) => item.match_pattern(expr),
        }
    }
}

impl MatchPattern<grammar::ItemConst> for syn::Item {
    fn match_pattern(&self, item: &grammar::ItemConst) -> MatchResult {
        match self {
            syn::Item::Const(item_const) => item_const.match_pattern(item),
            _ => Err(NoMatch),
        }
    }
}

impl MatchPattern<grammar::Expr> for syn::Item {
    fn match_pattern(&self, item: &grammar::Expr) -> MatchResult {
        match self {
            syn::Item::Const(const_) => const_.expr.match_pattern(item),
            _ => Err(NoMatch),
        }
    }
}

impl MatchPattern<syn::Ident> for grammar::Ident {
    fn match_pattern(&self, t: &grammar::Ident) -> MatchResult {
        Match::from_bool(self.to_string() == t.to_string())
    }
}

impl MatchPattern<grammar::Expr> for syn::Expr {
    fn match_pattern(&self, t: &grammar::Expr) -> MatchResult {
        match self {
            syn::Expr::Binary(i1) => {
                if let grammar::Expr::Binary(i2) = t {
                    i1.match_pattern(i2)
                } else {
                    Err(NoMatch)
                }
            }
            syn::Expr::Unary(i1) => {
                if let grammar::Expr::Unary(i2) = t {
                    i1.match_pattern(i2)
                } else {
                    Err(NoMatch)
                }
            }
            _ => todo!(),
        }
    }
}

impl MatchPattern<grammar::ExprBinary> for syn::ExprBinary {
    fn match_pattern(&self, t: &grammar::ExprBinary) -> MatchResult {
        let Self {
            attrs: _,
            left,
            op,
            right,
        } = self;
        // attrs.match_pattern(&t.attrs)?;
        op.match_pattern(&t.op)?;
        left.cmp_pat(&t.left)?;
        right.cmp_pat(&t.right)?;
        Ok(Match)
    }
}

impl MatchPattern<syn::ExprUnary> for grammar::ExprUnary {
    fn match_pattern(&self, t: &syn::ExprUnary) -> MatchResult {
        todo!()
    }
}

impl MatchPattern<syn::BinOp> for syn::BinOp {
    fn match_pattern(&self, t: &syn::BinOp) -> MatchResult {
        let is_match = match self {
            syn::BinOp::Add(_) => matches!(t, syn::BinOp::Add(_)),
            syn::BinOp::Sub(_) => matches!(t, syn::BinOp::Sub(_)),
            syn::BinOp::Mul(_) => matches!(t, syn::BinOp::Mul(_)),
            syn::BinOp::Div(_) => matches!(t, syn::BinOp::Div(_)),
            syn::BinOp::Rem(_) => matches!(t, syn::BinOp::Rem(_)),
            syn::BinOp::And(_) => matches!(t, syn::BinOp::And(_)),
            syn::BinOp::Or(_) => matches!(t, syn::BinOp::Or(_)),
            syn::BinOp::BitXor(_) => matches!(t, syn::BinOp::BitXor(_)),
            syn::BinOp::BitAnd(_) => matches!(t, syn::BinOp::BitAnd(_)),
            syn::BinOp::BitOr(_) => matches!(t, syn::BinOp::BitOr(_)),
            syn::BinOp::Shl(_) => matches!(t, syn::BinOp::Shl(_)),
            syn::BinOp::Shr(_) => matches!(t, syn::BinOp::Shr(_)),
            syn::BinOp::Eq(_) => matches!(t, syn::BinOp::Eq(_)),
            syn::BinOp::Lt(_) => matches!(t, syn::BinOp::Lt(_)),
            syn::BinOp::Le(_) => matches!(t, syn::BinOp::Le(_)),
            syn::BinOp::Ne(_) => matches!(t, syn::BinOp::Ne(_)),
            syn::BinOp::Ge(_) => matches!(t, syn::BinOp::Ge(_)),
            syn::BinOp::Gt(_) => matches!(t, syn::BinOp::Gt(_)),
            syn::BinOp::AddAssign(_) => matches!(t, syn::BinOp::AddAssign(_)),
            syn::BinOp::SubAssign(_) => matches!(t, syn::BinOp::SubAssign(_)),
            syn::BinOp::MulAssign(_) => matches!(t, syn::BinOp::MulAssign(_)),
            syn::BinOp::DivAssign(_) => matches!(t, syn::BinOp::DivAssign(_)),
            syn::BinOp::RemAssign(_) => matches!(t, syn::BinOp::RemAssign(_)),
            syn::BinOp::BitXorAssign(_) => matches!(t, syn::BinOp::BitXorAssign(_)),
            syn::BinOp::BitAndAssign(_) => matches!(t, syn::BinOp::BitAndAssign(_)),
            syn::BinOp::BitOrAssign(_) => matches!(t, syn::BinOp::BitOrAssign(_)),
            syn::BinOp::ShlAssign(_) => matches!(t, syn::BinOp::ShlAssign(_)),
            syn::BinOp::ShrAssign(_) => matches!(t, syn::BinOp::ShrAssign(_)),
            _ => todo!(),
        };
        Match::from_bool(is_match)
    }
}

impl MatchPattern<grammar::ItemConst> for syn::ItemConst {
    fn match_pattern(&self, item: &grammar::ItemConst) -> MatchResult {
        // self.vis.match_pattern(&item.vis)?;
        self.ident.cmp_pat(&item.ident)?;
        // self.generics.match_pattern(&item.generics)?;
        // self.ty.match_pattern(&item.ty)?;
        (*self.expr).cmp_pat(&item.expr)?;
        Ok(Match)
    }
}
