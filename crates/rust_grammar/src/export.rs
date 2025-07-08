#[doc(hidden)]
pub use std::concat;

#[doc(hidden)]
pub type Span = proc_macro2::Span;

#[doc(hidden)]
pub use crate::group::{parse_braces, parse_brackets, parse_parens};
#[doc(hidden)]
pub use crate::span::IntoSpans;
#[doc(hidden)]
pub use crate::token::private::CustomToken;
