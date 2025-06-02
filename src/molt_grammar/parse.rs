use molt_lib::VarDecl;
use rust_grammar::{Ident, Node, Token};
use rust_grammar::{Result, braced, parse::Parse, parse::ParseStream};

use crate::molt_grammar::parse_node_with_kind;

use super::{Command, Decl, MoltFile, UserKind, Var, VarId};

mod kw {
    rust_grammar::custom_keyword!(transform);
}

impl Parse for MoltFile {
    fn parse(parser: ParseStream) -> Result<Self> {
        let mut commands = vec![];
        let mut vars = vec![];
        while !parser.is_empty() {
            match parser.parse()? {
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
    fn parse(parser: ParseStream) -> Result<Self> {
        if parser.peek(Token![match]) || parser.peek(kw::transform) {
            Ok(Self::Command(parser.parse()?))
        } else {
            let var_decl: ParseVarDecl = parser.parse()?;
            Ok(Self::VarDecl(var_decl.0))
        }
    }
}

struct ParseVarDecl(VarDecl);

impl Parse for ParseVarDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![let] = input.parse()?;
        let var: Ident = input.parse()?;
        let _: Token![:] = input.parse()?;
        let kind: UserKind = input.parse()?;
        let var: Var<Node> = Var::new(var.to_string(), kind.into());
        let name: VarId = input.add_var::<Node>(var).untyped();
        let node = if input.peek(Token![=]) {
            let _: Token![=] = input.parse()?;
            let content;
            braced!(content in input);
            let node = parse_node_with_kind(&content, kind)?;
            let id = input.add_node(node);
            Some(id)
        } else {
            None
        };
        let _: Token![;] = input.parse()?;
        Ok(Self(VarDecl { id: name, node }))
    }
}

impl Parse for Command {
    fn parse(parser: ParseStream) -> Result<Self> {
        let command = if parser.peek(kw::transform) {
            todo!("enable variant")
            // let input_var: Var = parser.parse()?;
            // parser.consume(Minus);
            // parser.consume(Gt);
            // let output_var: Var = parser.parse()?;
            // Command::Transform(input_var, output_var)
        } else {
            let _: Token![match] = parser.parse()?;
            let match_var: Ident = parser.parse()?;
            // obtain the kind by using the ctx
            let id = parser
                .add_existing_var(&match_var.to_string())
                // TODO: Fix this unwrap here and replace it with a proper
                // error message if the var does not exist. Problem: This
                // requires Spans which I don#t have because I dont have "printing"
                // enabled
                .unwrap();
            Command::Match(id)
        };
        let _: Token![;] = parser.parse()?;
        Ok(command)
    }
}
