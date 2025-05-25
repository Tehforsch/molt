use crate::ctx::NodeId;

use super::node::ToNode;
use super::tokenizer::Keyword::{Let, Match, Transform};
#[allow(unused)]
use super::tokenizer::TokenKind::{CloseBrace, Colon, Eq, Gt, Keyword, Lt, Minus, OpenBrace, Semi};
use super::tokenizer::{Ident, TokenKind};
use super::{Command, Decl, Lit, MoltFile, Node, Peek, RustFile, Span, Var, VarId};
use super::{Parse, Parser, Result, molt_grammar::VarDecl};

impl Parse for MoltFile {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let mut commands = vec![];
        let mut vars = vec![];
        while !parser.is_at_end() {
            match parser.parse_spanned()? {
                Decl::VarDecl(var) => vars.push(var),
                Decl::Command(command) => commands.push(command),
            }
        }
        Ok(MoltFile {
            vars,
            commands,
            sorted: false,
        })
    }
}

impl Parse for Decl {
    fn parse(parser: &mut Parser) -> Result<Self> {
        if parser.token_matches(Keyword(Match)) || parser.token_matches(Keyword(Transform)) {
            Ok(Self::Command(parser.parse()?))
        } else {
            Ok(Self::VarDecl(parser.parse()?))
        }
    }
}

impl Parse for VarDecl {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.consume(Keyword(Let))?;
        let var: Var = parser.parse()?;
        let name: VarId = parser.ctx.add_var(var);
        parser.consume(Colon)?;
        let kind = parser.parse()?;
        let node = if parser.consume_if_matches(Eq) {
            parser.consume(OpenBrace)?;
            let node = Node::parse_with_kind(parser, kind)?;
            parser.consume(CloseBrace)?;
            let id = parser.ctx.add_node(node, parser.mode);
            Some(id)
        } else {
            None
        };
        parser.consume(Semi)?;
        Ok(Self { id: name, node })
    }
}

impl<T: Parse + ToNode> Parse for NodeId<T> {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let t = parser.parse_spanned()?;
        let id = parser.ctx.add(t, parser.mode);
        Ok(id)
    }
}

impl Parse for Lit {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let span = parser.cursor.current_token_span();
        let lit = parser.consume_pat(|kind| {
            if let TokenKind::Literal { kind, .. } = kind {
                Some(kind)
            } else {
                None
            }
        })?;
        Ok(Lit::new(lit, span))
    }
}

impl Parse for Ident {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let span = parser.cursor.current_token_span();
        parser.consume(TokenKind::Ident)?;
        Ok(Ident::new(span))
    }
}

impl Parse for Var {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Var::new(parser.parse()?))
    }
}

impl Parse for Command {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let command = if parser.consume_if_matches(Keyword(Transform)) {
            todo!("enable variant")
            // let input_var: Var = parser.parse()?;
            // parser.consume(Minus);
            // parser.consume(Gt);
            // let output_var: Var = parser.parse()?;
            // Command::Transform(input_var, output_var)
        } else {
            parser.consume(Keyword(Match))?;
            let match_var: Var = parser.parse()?;
            Command::Match(parser.ctx.add_var(match_var))
        };
        parser.consume(Semi)?;
        Ok(command)
    }
}

impl Parse for RustFile {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Self)
    }
}
