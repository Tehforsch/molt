use derive_macro::CmpSyn;

#[cfg(feature = "parsing")]
use crate::error::Result;
#[cfg(feature = "parsing")]
use crate::parse::ParseStream;
use crate::path::Path;
use crate::token::{Brace, Bracket, Paren};
use proc_macro2::extra::DelimSpan;
#[cfg(feature = "parsing")]
use proc_macro2::Delimiter;
use proc_macro2::TokenStream;
#[cfg(feature = "parsing")]
use proc_macro2::TokenTree;

#[derive(Debug, CmpSyn)]
/// A macro invocation: `println!("{}", mac)`.
pub struct Macro {
    pub path: Path,
    pub bang_token: Token![!],
    pub delimiter: MacroDelimiter,
    pub tokens: TokenStream,
}

#[derive(Debug, CmpSyn)]
/// A grouping token that surrounds a macro body: `m!(...)` or `m!{...}` or `m![...]`.
pub enum MacroDelimiter {
    Paren(Paren),
    Brace(Brace),
    Bracket(Bracket),
}

impl MacroDelimiter {
    pub fn span(&self) -> &DelimSpan {
        match self {
            MacroDelimiter::Paren(token) => &token.span,
            MacroDelimiter::Brace(token) => &token.span,
            MacroDelimiter::Bracket(token) => &token.span,
        }
    }

    pub(crate) fn is_brace(&self) -> bool {
        match self {
            MacroDelimiter::Brace(_) => true,
            MacroDelimiter::Paren(_) | MacroDelimiter::Bracket(_) => false,
        }
    }
}

#[cfg(feature = "parsing")]
pub(crate) fn parse_delimiter(input: ParseStream) -> Result<(MacroDelimiter, TokenStream)> {
    input.step(|cursor| {
        if let Some((TokenTree::Group(g), rest)) = cursor.token_tree() {
            let span = g.delim_span();
            let delimiter = match g.delimiter() {
                Delimiter::Parenthesis => MacroDelimiter::Paren(Paren(span)),
                Delimiter::Brace => MacroDelimiter::Brace(Brace(span)),
                Delimiter::Bracket => MacroDelimiter::Bracket(Bracket(span)),
                Delimiter::None => {
                    return Err(cursor.error("expected delimiter"));
                }
            };
            Ok(((delimiter, g.stream()), rest))
        } else {
            Err(cursor.error("expected delimiter"))
        }
    })
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    use crate::error::Result;
    use crate::mac::{parse_delimiter, Macro};
    use crate::parse::{Parse, ParseStream};
    use crate::path::Path;

    impl Parse for Macro {
        fn parse(input: ParseStream) -> Result<Self> {
            let tokens;
            Ok(Macro {
                path: input.call(Path::parse_mod_style)?,
                bang_token: input.parse()?,
                delimiter: {
                    let (delimiter, content) = parse_delimiter(input)?;
                    tokens = content;
                    delimiter
                },
                tokens,
            })
        }
    }
}
