use proc_macro2::Span;

use crate::rust_grammar::Ident;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BuiltinFn {
    Assert,
    AssertEq,
    Print,
    Dbg,
}

pub fn builtins_def() -> Vec<(Ident, BuiltinFn)> {
    vec![
        (Ident::new("assert", Span::call_site()), BuiltinFn::Assert),
        (
            Ident::new("assert_eq", Span::call_site()),
            BuiltinFn::AssertEq,
        ),
        (Ident::new("print", Span::call_site()), BuiltinFn::Print),
        (Ident::new("dbg", Span::call_site()), BuiltinFn::Dbg),
    ]
}
