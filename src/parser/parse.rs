use crate::ctx::NodeId;

use super::node::ToNode;
use super::tokenizer::Ident;
use super::tokenizer::Keyword::Let;
use super::tokenizer::TokenKind::{Colon, Eq, Keyword};
use super::{MoltFile, Node, RustFile, Var, VarId};
use super::{Parse, Parser, Result, molt_grammar::VarDecl};

impl Parse for VarDecl {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.consume(Keyword(Let))?;
        let var: Var = parser.parse()?;
        let name: VarId = VarId::new(parser.ctx.add_var(var));
        parser.consume(Colon)?;
        let kind = parser.parse()?;
        let node = if parser.consume_if_matches(Eq) {
            Some(parser.parse::<NodeId<Node>>()?.untyped())
        } else {
            None
        };
        Ok(Self {
            id: name,
            kind,
            node,
        })
    }
}

impl<T: Parse + ToNode> Parse for NodeId<T> {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.push_node_pos();
        let t = parser.parse()?;
        let id = parser.ctx.add(t, parser.mode);
        parser.pop_node_pos();
        Ok(id)
    }
}

impl Parse for Ident {
    fn parse(parser: &mut Parser) -> Result<Self> {
        todo!()
    }
}

impl Parse for Var {
    fn parse(parser: &mut Parser) -> Result<Self> {
        todo!()
    }
}

impl Parse for Node {
    fn parse(parser: &mut Parser) -> Result<Self> {
        todo!()
    }
}

impl Parse for RustFile {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Self)
    }
}

impl Parse for MoltFile {
    fn parse(parser: &mut Parser) -> Result<Self> {
        todo!()
    }
}
