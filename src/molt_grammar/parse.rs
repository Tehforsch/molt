use rust_grammar::ext::IdentExt;
use rust_grammar::{Ident, Token, TokenStream};
use rust_grammar::{Result, braced, parse::Parse, parse::ParseStream};

use super::{Command, Decl, UnresolvedMoltFile, UnresolvedVarDecl, UserKind};

mod kw {
    rust_grammar::custom_keyword!(transform);
}

impl Parse for UnresolvedMoltFile {
    fn parse(parser: ParseStream) -> Result<Self> {
        let mut commands = vec![];
        let mut vars = vec![];
        while !parser.is_empty() {
            match parser.parse()? {
                Decl::VarDecl(var) => vars.push(var),
                Decl::Command(command) => commands.push(command),
            }
        }
        Ok(UnresolvedMoltFile { vars, commands })
    }
}

impl Parse for Decl {
    fn parse(parser: ParseStream) -> Result<Self> {
        if parser.peek(Token![match]) || parser.peek(kw::transform) {
            Ok(Self::Command(parser.parse()?))
        } else {
            Ok(Self::VarDecl(parser.parse()?))
        }
    }
}

impl Parse for UnresolvedVarDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![let] = input.parse()?;
        let var: Ident = input.call(Ident::parse_any)?;
        let _: Token![:] = input.parse()?;
        let kind: UserKind = input.parse()?;
        let tokens = if input.peek(Token![=]) {
            let _: Token![=] = input.parse()?;
            let content;
            braced!(content in input);
            let tokens = content.parse::<TokenStream>()?;
            Some(tokens)
        } else {
            None
        };
        let _: Token![;] = input.parse()?;
        Ok(Self {
            name: var.to_string(),
            kind: kind.into(),
            tokens,
        })
    }
}

impl Parse for Command<String> {
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
            Command::Match(match_var.to_string())
        };
        let _: Token![;] = parser.parse()?;
        Ok(command)
    }
}
