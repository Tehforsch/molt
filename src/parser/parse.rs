use crate::ctx::NodeId;

use super::node::ToNode;
use super::tokenizer::Ident;
use super::tokenizer::Keyword::Let;
use super::tokenizer::TokenKind::{Colon, Eq, Keyword};
use super::{Kind, Node, RustFile};
use super::{Parse, Parser, Result, molt_grammar::VarDecl};

impl Parse for VarDecl {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.consume(Keyword(Let))?;
        let name = parser.parse()?;
        parser.consume(Colon)?;
        let kind = parser.parse()?;
        parser.consume(Eq)?;
        let node: NodeId<Node> = parser.parse()?;
        Ok(Self {
            name,
            kind,
            node: node.untyped(),
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
