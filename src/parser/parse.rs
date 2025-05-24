use crate::ctx::NodeId;

use super::node::ToNode;
use super::tokenizer::Keyword::{Let, Match, Transform};
#[allow(unused)]
use super::tokenizer::TokenKind::{Colon, Eq, Gt, Keyword, Lt, Minus, Semi};
use super::tokenizer::{Ident, TokenKind};
use super::{Command, Decl, Lit, MoltFile, Node, Peek, RustFile, Span, Var, VarId};
use super::{Parse, Parser, Result, molt_grammar::VarDecl};

impl Parse for VarDecl {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.consume(Keyword(Let))?;
        let var: Var = parser.parse()?;
        let name: VarId = parser.ctx.add_var(var);
        parser.consume(Colon)?;
        let kind = parser.parse()?;
        let node = if parser.consume_if_matches(Eq) {
            let node = Node::parse_with_kind(parser, kind)?;
            let id = parser.ctx.add_node(node, parser.mode);
            Some(id)
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
        let t = parser.parse_spanned()?;
        let id = parser.ctx.add(t, parser.mode);
        Ok(id)
    }
}

impl Parse for Lit {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let start = parser.cursor.pos();
        let lit = parser.consume_pat(|kind| {
            if let TokenKind::Literal { kind, .. } = kind {
                Some(kind)
            } else {
                None
            }
        })?;
        let end = parser.cursor.pos();
        let span = Span::new(start, end);
        Ok(Lit::new(lit, span))
    }
}

impl Parse for Ident {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let start = parser.cursor.pos();
        parser.consume(TokenKind::Ident)?;
        let end = parser.cursor.pos();
        let span = Span::new(start, end);
        Ok(Ident::new(span))
    }
}

impl Parse for Var {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Var::new(parser.parse()?))
    }
}

impl Parse for RustFile {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Self)
    }
}

impl Parse for MoltFile {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let mut decls = vec![];
        while !parser.is_at_end() {
            decls.push(parser.parse()?);
        }
        let mut commands = vec![];
        let mut vars = vec![];
        for decl in decls.into_iter() {
            match decl {
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
