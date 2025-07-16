//! Extension traits to provide parsing methods on foreign types.

use proc_macro2::Ident;

use crate::parser::buffer::Cursor;
use crate::parser::error::Result;
use crate::parser::parse::{ParseStream, Peek};
use crate::parser::token::CustomToken;

/// Additional methods for `Ident` not provided by proc-macro2 or libproc_macro.
pub trait IdentExt: Sized {
    fn parse_any(input: ParseStream) -> Result<Self>;

    #[allow(non_upper_case_globals)]
    const peek_any: private::PeekFn = private::PeekFn;
}

impl IdentExt for Ident {
    fn parse_any(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.ident() {
            Some((ident, rest)) => Ok((ident, rest)),
            None => Err(cursor.error("expected ident")),
        })
    }
}

impl Peek for private::PeekFn {
    type Token = private::IdentAny;
}

impl CustomToken for private::IdentAny {
    fn peek(cursor: Cursor) -> bool {
        cursor.ident().is_some()
    }

    fn display() -> &'static str {
        "identifier"
    }
}

mod private {
    pub struct PeekFn;
    pub struct IdentAny;

    impl Copy for PeekFn {}
    impl Clone for PeekFn {
        fn clone(&self) -> Self {
            *self
        }
    }
}
