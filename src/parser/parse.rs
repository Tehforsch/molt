use crate::ctx::NodeId;

use super::node::ToNode;
use super::tokenizer::Ident;
use super::tokenizer::Keyword::Let;
use super::tokenizer::TokenKind::{Colon, Eq, Keyword};
use super::{Kind, MoltFile, Node, RustFile, VarId};
use super::{Parse, Parser, Result, molt_grammar::VarDecl};

impl Parse for MoltFile {
    fn parse(parser: &mut Parser) -> Result<Self> {
        todo!()
    }
}

impl Parse for VarDecl {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.consume(Keyword(Let))?;
        let name: Ident = parser.parse()?;
        // TODO add this to vars. Need to change ctx for that.
        let name: VarId = todo!();
        parser.consume(Colon)?;
        let kind = parser.parse()?;
        let node = if parser.consume_if_matches(Eq) {
            Some(parser.parse::<NodeId<Node>>()?.untyped())
        } else {
            None
        };
        Ok(Self { name, kind, node })
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
