use rust_grammar::ext::IdentExt;
use rust_grammar::{Ident, Token, TokenStream};
use rust_grammar::{Result, braced, parse::Parse, parse::ParseStream};

use super::{
    Command, Decl, MatchCommand, TransformCommand, UnresolvedMoltFile, UnresolvedVarDecl, UserKind,
};

mod kw {
    rust_grammar::custom_keyword!(transform);
}

impl Parse for UnresolvedMoltFile {
    fn parse(parser: ParseStream) -> Result<Self> {
        let mut commands = vec![];
        let mut vars = vec![];
        while !parser.is_empty() {
            match parser.parse()? {
                Decl::VarDecl(new_vars) => vars.extend(new_vars.0.into_iter()),
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

pub struct UnresolvedVarDecls(Vec<UnresolvedVarDecl>);

impl Parse for UnresolvedVarDecls {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![let] = input.parse()?;
        let var: Ident = input.call(Ident::parse_any)?;
        let mut vars = vec![var];
        while input.peek(Token![,]) {
            let _: Token![,] = input.parse()?;
            vars.push(input.call(Ident::parse_any)?);
        }
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
        Ok(Self(
            vars.into_iter()
                .map(|var| UnresolvedVarDecl {
                    name: var.to_string(),
                    kind: kind.into(),
                    tokens: tokens.clone(),
                })
                .collect(),
        ))
    }
}

impl Parse for Command<String> {
    fn parse(parser: ParseStream) -> Result<Self> {
        let command = if parser.peek(kw::transform) {
            let _: kw::transform = parser.parse()?;
            let input: Ident = parser.parse()?;
            let _: Token![->] = parser.parse()?;
            let output: Ident = parser.parse()?;
            Command::Transform(TransformCommand {
                input: input.to_string(),
                output: output.to_string(),
                match_: None,
            })
        } else {
            let _: Token![match] = parser.parse()?;
            let print: Ident = parser.parse()?;
            Command::Match(MatchCommand {
                match_: None,
                print: Some(print.to_string()),
            })
        };
        let _: Token![;] = parser.parse()?;
        Ok(command)
    }
}
