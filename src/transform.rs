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

impl MatchPattern<syn::ExprBinary> for grammar::ExprBinary {
    fn match_pattern(&self, t: &syn::ExprBinary) -> MatchResult {
        todo!()
    }
}

impl MatchPattern<syn::ExprUnary> for grammar::ExprUnary {
    fn match_pattern(&self, t: &syn::ExprUnary) -> MatchResult {
        todo!()
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
