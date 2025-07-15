use molt_lib::rule::Rule;
use rust_grammar::ext::IdentExt;
use rust_grammar::parse::{Parse, ParseStream};
use rust_grammar::token::Brace;
use rust_grammar::{Error, Ident, Kind, Result, Token, TokenStream, TokenTree, braced};
use rust_grammar::{parenthesized, token::Paren};

use super::{
    Command, Decl, MatchCommand, Ruleset, TokenVar, TransformCommand, UnresolvedMoltFile,
    UnresolvedTypeAnnotation, UnresolvedVarDecl,
};

mod kw {
    rust_grammar::custom_keyword!(transform);
    rust_grammar::custom_keyword!(strict);
    rust_grammar::custom_keyword!(ignore);
}

impl Parse for UnresolvedMoltFile {
    fn parse(parser: ParseStream) -> Result<Self> {
        let mut commands = vec![];
        let mut vars = vec![];
        let mut type_annotations = vec![];
        let mut rules = vec![];
        while !parser.is_empty() {
            match parser.parse()? {
                Decl::Var(new_vars) => vars.extend(new_vars.0.into_iter()),
                Decl::Command(command) => commands.push(command),
                Decl::TypeAnnotation(type_annotation) => type_annotations.push(type_annotation),
                Decl::Ruleset(rule) => rules.push(rule),
            }
        }
        Ok(UnresolvedMoltFile {
            vars,
            commands,
            type_annotations,
            rules,
        })
    }
}

impl Parse for Decl {
    fn parse(parser: ParseStream) -> Result<Self> {
        let lookahead = parser.lookahead1();
        if lookahead.peek(Token![match]) || lookahead.peek(kw::transform) {
            Ok(Self::Command(parser.parse()?))
        } else if lookahead.peek(Token![type]) {
            Ok(Self::TypeAnnotation(parser.parse()?))
        } else if lookahead.peek(Token![let]) {
            Ok(Self::Var(parser.parse()?))
        } else if lookahead.peek(kw::strict) || lookahead.peek(kw::ignore) {
            Ok(Self::Ruleset(parser.parse()?))
        } else {
            Err(lookahead.error())
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
        let kind: Kind = input.parse()?;
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
    // Check if input is empty (like in "let x: Type = ;")
    // This is necessary because an empty TokenStream will produce
    // strange spans when an error occurs.
    if input.peek(Token![;]) {
        return Err(input.error("expected expression"));
    }

    let mut collected = TokenStream::new();
    while !input.is_empty() {
        let tt: TokenTree = input.parse()?;
        collected.extend(std::iter::once(tt));

        if input.is_empty() {
            return Err(Error::new(input.cursor().prev_span(), "Expected `;`."));
        }
        if input.peek(Token![;]) || input.is_empty() {
            break;
        }
    }

    Ok(collected)
}

impl Parse for Ruleset {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Ruleset {
            rule: input.parse::<RuleWrap>()?.0,
            keys: {
                let mut keys = vec![];
                while !input.peek(Token![;]) {
                    let parse_key: ParseRuleKey = input.parse()?;
                    keys.extend(parse_key.into_keys());
                    if input.peek(Token![,]) {
                        input.parse::<Token![,]>()?;
                    }
                }
                input.parse::<Token![;]>()?;
                keys
            },
        })
    }
}

struct RuleWrap(Rule);

impl Parse for RuleWrap {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::ignore) {
            let _: kw::ignore = input.parse()?;
            Ok(RuleWrap(Rule::Ignore))
        } else if input.peek(kw::strict) {
            let _: kw::strict = input.parse()?;
            Ok(RuleWrap(Rule::Strict))
        } else {
            unreachable!()
        }
    }
}

macro_rules! rules {
    ($(($name: ident, $(($item: ident, $rule: ident)),* $(,)?),)* $(,)?) => {
        pub enum ParseRuleKey {
            Blanket(molt_lib::rule::RuleKeyKind),
            Concrete(Vec<molt_lib::rule::RuleKey>),
        }

        impl ParseRuleKey {
            fn into_keys(self) -> Vec<molt_lib::rule::RuleKey> {
                match self {
                    Self::Blanket(key) => {
                        match key {
                            $(
                                molt_lib::rule::RuleKeyKind::$name => {
                                    vec![
                                        $(
                                            molt_lib::rule::RuleKey::$name(molt_lib::rule::$name::$item),
                                        )*
                                    ]
                                }
                            )*
                        }
                    }
                    Self::Concrete(keys) => {
                        keys
                    }
                }
            }
        }

        mod key_kws {
            $(
                rust_grammar::custom_keyword!($name);
            )*
            pub mod inner {
                $(
                    #[allow(non_snake_case)]
                    pub mod $name {
                        $(
                            rust_grammar::custom_keyword!($item);
                        )*
                    }
                )*
            }
        }

        struct RuleKeyKindWrapper(molt_lib::rule::RuleKeyKind);

        impl Parse for RuleKeyKindWrapper {
            fn parse(input: ParseStream) -> Result<RuleKeyKindWrapper> {
                let lookahead = input.lookahead1();
                $(
                    if lookahead.peek(key_kws::$name) {
                        let _ = input.parse::<key_kws::$name>()?;
                        return Ok(RuleKeyKindWrapper(molt_lib::rule::RuleKeyKind::$name))
                    }
                )*
                return Err(lookahead.error());
            }
        }

        impl Parse for ParseRuleKey {
            fn parse(input: ParseStream) -> Result<Self> {
                let key = input.parse::<RuleKeyKindWrapper>()?.0;
                if input.peek(Paren) {
                    let content;
                    parenthesized!(content in input);
                    let mut types = vec![];
                    while !content.is_empty() {
                        match key {
                            $(
                                molt_lib::rule::RuleKeyKind::$name => {
                                    let lookahead = content.lookahead1();
                                    $(
                                        if lookahead.peek(key_kws::inner::$name::$item) {
                                            let _ = content.parse::<key_kws::inner::$name::$item>();
                                            let inner = molt_lib::rule::$name::$item;
                                            types.push(molt_lib::rule::RuleKey::$name(inner));
                                            if content.peek(Token![,]) {
                                                let _ = content.parse::<Token![,]>()?;
                                                break
                                            }
                                            else {
                                                continue
                                            }
                                        }
                                    )*
                                    return Err(lookahead.error());
                                }
                            )*
                        }
                    }
                    Ok(ParseRuleKey::Concrete(types))
                }
                else {
                    Ok(ParseRuleKey::Blanket(key))
                }
            }
        }
    }
}

// Unfortunately, this is split over crates for now.
// Remember to update the corresponding macro call in
// crates/molt_lib/src/rule.rs
rules! {
    (Vis,
        (Const, Strict),
        (Enum, Strict),
        (ExternCrate, Strict),
        (Fn, Strict),
        (Mod, Strict),
        (Static, Strict),
        (Struct, Strict),
        (Trait, Strict),
        (TraitAlias, Strict),
        (Type, Strict),
        (Union, Strict),
        (Use, Strict),
        (Field, Strict),
    ),
    (Unsafe,
        (Mod, Strict),
        (Impl, Strict),
        (Trait, Strict),
        (Fn, Strict),
    ),
}
