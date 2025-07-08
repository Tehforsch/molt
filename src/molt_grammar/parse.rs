use rust_grammar::ext::IdentExt;
use rust_grammar::parse::{Parse, ParseStream};
use rust_grammar::token::Brace;
use rust_grammar::{Ident, Result, Token, TokenStream, TokenTree, braced};

use super::{
    Command, Decl, MatchCommand, TokenVar, TransformCommand, UnresolvedMoltFile,
    UnresolvedTypeAnnotation, UnresolvedVarDecl, UserKind,
};

mod kw {
    rust_grammar::custom_keyword!(transform);
}

impl Parse for UnresolvedMoltFile {
    fn parse(parser: ParseStream) -> Result<Self> {
        let mut commands = vec![];
        let mut vars = vec![];
        let mut type_annotations = vec![];
        while !parser.is_empty() {
            match parser.parse()? {
                Decl::Var(new_vars) => vars.extend(new_vars.0.into_iter()),
                Decl::Command(command) => commands.push(command),
                Decl::TypeAnnotation(type_annotation) => type_annotations.push(type_annotation),
            }
        }
        Ok(UnresolvedMoltFile {
            vars,
            commands,
            type_annotations,
        })
    }
}

impl Parse for Decl {
    fn parse(parser: ParseStream) -> Result<Self> {
        if parser.peek(Token![match]) || parser.peek(kw::transform) {
            Ok(Self::Command(parser.parse()?))
        } else if parser.peek(Token![type]) {
            Ok(Self::TypeAnnotation(parser.parse()?))
        } else {
            Ok(Self::Var(parser.parse()?))
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
            if input.peek(Brace) {
                let content;
                braced!(content in input);
                let tokens = content.parse::<TokenStream>()?;
                Some(tokens)
            } else {
                Some(parse_until_semicolon(input)?)
            }
        } else {
            None
        };
        let _: Token![;] = input.parse()?;
        Ok(Self(
            vars.into_iter()
                .map(|var| UnresolvedVarDecl {
                    var,
                    kind,
                    tokens: tokens.clone(),
                })
                .collect(),
        ))
    }
}

impl Parse for TokenVar {
    fn parse(input: ParseStream) -> Result<Self> {
        let (span, name) = input.call_spanned(Ident::parse_any)?.decompose();
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
                transforms: vec![(input, output)],
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
        let _: Token![;] = parser.parse()?;
        Ok(command)
    }
}

impl Parse for UnresolvedTypeAnnotation {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![type] = input.parse()?;
        let var_name: Ident = input.parse()?;
        let _: Token![=] = input.parse()?;
        let type_ = if input.peek(Brace) {
            let content;
            braced!(content in input);
            content.parse()?
        } else {
            parse_until_semicolon(input)?
        };
        let _: Token![;] = input.parse()?;
        Ok(UnresolvedTypeAnnotation {
            var_name: var_name.to_string(),
            type_,
        })
    }
}

fn parse_until_semicolon(input: ParseStream) -> Result<TokenStream> {
    let mut collected = TokenStream::new();
    while !input.is_empty() {
        let tt: TokenTree = input.parse()?;
        collected.extend(std::iter::once(tt));

        if input.peek(Token![;]) || input.is_empty() {
            break;
        }
    }

    Ok(collected)
}
