#[doc(hidden)]
pub use std::concat;

#[doc(hidden)]
pub type Span = proc_macro2::Span;

#[doc(hidden)]
pub use crate::rust_grammar::group::{parse_braces, parse_brackets, parse_parens};
#[doc(hidden)]
pub use crate::rust_grammar::span::IntoSpans;
#[doc(hidden)]
pub use crate::rust_grammar::token::private::CustomToken;
