use crate::{CmpSyn, Matcher};
use proc_macro2::{Ident, Span};

use crate::parser::error::Result;
use crate::parser::lookahead;
use crate::parser::parse::{Parse, ParseStream};

/// A Rust lifetime: `'a`.
///
/// Lifetime names must conform to the following rules:
///
/// - Must start with an apostrophe.
/// - Must not consist of just an apostrophe: `'`.
/// - Character after the apostrophe must be `_` or a Unicode code point with
///   the XID_Start property.
/// - All following characters must be Unicode code points with the XID_Continue
///   property.
#[derive(Debug)]
pub struct Lifetime {
    pub apostrophe: Span,
    pub ident: Ident,
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Lifetime(marker: lookahead::TokenMarker) -> Lifetime {
    match marker {}
}

impl Parse for Lifetime {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| {
            cursor
                .lifetime()
                .ok_or_else(|| cursor.error("expected lifetime"))
        })
    }
}

impl CmpSyn for Lifetime {
    fn cmp_syn(&self, ctx: &mut Matcher, pat: &Self) {
        ctx.cmp_syn(&self.ident, &pat.ident);
    }
}
