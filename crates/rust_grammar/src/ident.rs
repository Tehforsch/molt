#[cfg(feature = "parsing")]
use crate::lookahead;

pub use proc_macro2::Ident;

pub struct AnyIdent;
pub struct TokenIdent<T>(T);

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Ident(marker: lookahead::TokenMarker) -> Ident {
    match marker {}
}

macro_rules! ident_from_token {
    ($token:ident) => {
        impl From<Token![$token]> for Ident {
            fn from(token: Token![$token]) -> Ident {
                Ident::new(stringify!($token), token.span)
            }
        }
    };
}

ident_from_token!(self);
ident_from_token!(Self);
ident_from_token!(super);
ident_from_token!(crate);
ident_from_token!(extern);
ident_from_token!(unsafe);

impl From<Token![_]> for Ident {
    fn from(token: Token![_]) -> Ident {
        Ident::new("_", token.span)
    }
}

pub(crate) fn xid_ok(symbol: &str) -> bool {
    let mut chars = symbol.chars();
    let first = chars.next().unwrap();
    if !(first == '_' || unicode_ident::is_xid_start(first)) {
        return false;
    }
    for ch in chars {
        if !unicode_ident::is_xid_continue(ch) {
            return false;
        }
    }
    true
}

#[cfg(feature = "parsing")]
mod parsing {
    use crate::buffer::Cursor;
    use crate::error::Result;
    use crate::ext::IdentExt;
    use crate::parse::{Parse, ParsePat, ParseStream, PeekPat};
    use crate::token::Token;
    use molt_lib::{Pattern, Spanned, SpannedPat};
    use proc_macro2::Ident;

    use super::AnyIdent;
    use super::TokenIdent;

    fn accept_as_ident(ident: &Ident) -> bool {
        match ident.to_string().as_str() {
            "_" |
            // Based on https://doc.rust-lang.org/1.65.0/reference/keywords.html
            "abstract" | "as" | "async" | "await" | "become" | "box" | "break" |
            "const" | "continue" | "crate" | "do" | "dyn" | "else" | "enum" |
            "extern" | "false" | "final" | "fn" | "for" | "if" | "impl" | "in" |
            "let" | "loop" | "macro" | "match" | "mod" | "move" | "mut" |
            "override" | "priv" | "pub" | "ref" | "return" | "Self" | "self" |
            "static" | "struct" | "super" | "trait" | "true" | "try" | "type" |
            "typeof" | "unsafe" | "unsized" | "use" | "virtual" | "where" |
            "while" | "yield" => false,
            _ => true,
        }
    }

    fn parse_ident(input: ParseStream) -> Result<Ident> {
        input.step(|cursor| {
            if let Some((ident, rest)) = cursor.ident() {
                if accept_as_ident(&ident) {
                    Ok((ident, rest))
                } else {
                    Err(cursor.error(format_args!(
                        "expected identifier, found keyword `{}`",
                        ident,
                    )))
                }
            } else {
                Err(cursor.error("expected identifier"))
            }
        })
    }

    impl Parse for Ident {
        fn parse(input: ParseStream) -> Result<Self> {
            parse_ident(input)
        }
    }

    impl ParsePat for Ident {
        type Target = Ident;

        fn parse_pat(input: ParseStream) -> Result<SpannedPat<Ident>> {
            input.call_spanned(parse_ident)
        }
    }

    impl PeekPat for Ident {
        type Target = Ident;

        fn peek(cursor: Cursor) -> bool {
            if let Some((ident, _rest)) = cursor.ident() {
                accept_as_ident(&ident)
            } else {
                false
            }
        }
    }

    impl ParsePat for AnyIdent {
        type Target = Ident;

        fn parse_pat(input: ParseStream) -> Result<SpannedPat<Ident>> {
            input.call_spanned(Ident::parse_any)
        }
    }

    impl PeekPat for AnyIdent {
        type Target = Ident;

        fn peek(cursor: Cursor) -> bool {
            cursor.ident().is_some()
        }
    }

    impl<T: Token + Parse> ParsePat for TokenIdent<T>
    where
        Ident: From<T>,
    {
        type Target = Ident;

        fn parse_pat(input: ParseStream) -> Result<SpannedPat<Ident>> {
            // We only enter this function if we already
            // peeked at this token, so there is no need to
            // check for variables.
            let token: Spanned<T> = input.parse_spanned()?;
            Ok(token.map(|token| Pattern::Real(Ident::from(token))))
        }
    }

    impl Token for Ident {
        fn peek(cursor: Cursor) -> bool {
            if let Some((ident, _rest)) = cursor.ident() {
                accept_as_ident(&ident)
            } else {
                false
            }
        }

        fn display() -> &'static str {
            "identifier"
        }
    }
}
