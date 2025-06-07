use rust_grammar::ext::IdentExt;
use rust_grammar::{Ident, Token, TokenStream};
use rust_grammar::{Result, braced, parse::Parse, parse::ParseStream};

use super::{
    Command, Decl, MatchCommand, TokenVar, TransformCommand, UnresolvedMoltFile, UnresolvedVarDecl,
    UserKind,
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
        let var: TokenVar = input.parse()?;
        let mut vars = vec![var];
        while input.peek(Token![,]) {
            let _: Token![,] = input.parse()?;
            vars.push(input.parse()?);
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
        if input.peek(Token![;]) {
            let _: Token![;] = input.parse()?;
        }
        Ok(Self(
            vars.into_iter()
                .map(|var| UnresolvedVarDecl {
                    var,
                    kind: kind.into(),
                    tokens: tokens.clone(),
                })
                .collect(),
        ))
    }
}

impl Parse for TokenVar {
    fn parse(input: ParseStream) -> Result<Self> {
        let (span, name) = input
            .call_spanned(Ident::parse_any)?
            .unwrap_real()
            .decompose();
        Ok(TokenVar {
            span,
            name: name.to_string(),
        })
    }
}

impl Parse for Command<TokenVar> {
    fn parse(parser: ParseStream) -> Result<Self> {
        let command = if parser.peek(kw::transform) {
            let _: kw::transform = parser.parse()?;
            let input: TokenVar = parser.parse()?;
            let _: Token![->] = parser.parse()?;
            let output: TokenVar = parser.parse()?;
            Command::Transform(TransformCommand {
                input,
                output,
                match_: None,
            })
        } else {
            let _: Token![match] = parser.parse()?;
            let print: TokenVar = parser.parse()?;
            Command::Match(MatchCommand {
                match_: None,
                print: Some(print),
            })
        };
        if parser.peek(Token![;]) {
            let _: Token![;] = parser.parse()?;
        }
        Ok(command)
    }
}
