use std::collections::HashMap;

use crate::molt_lang::interpreter::RuntimeFn;

#[derive(Clone, Copy)]
pub enum BuiltinFn {
    Assert,
    AssertEq,
    Print,
    Dbg,
}

pub fn builtins<'a>() -> HashMap<String, RuntimeFn<'a>> {
    [
        ("assert", RuntimeFn::Builtin(BuiltinFn::Assert)),
        ("assert_eq", RuntimeFn::Builtin(BuiltinFn::AssertEq)),
        ("print", RuntimeFn::Builtin(BuiltinFn::Print)),
        ("dbg", RuntimeFn::Builtin(BuiltinFn::Dbg)),
    ]
    .into_iter()
    .map(|(name, f)| (name.to_string(), f))
    .collect()
}

pub fn builtins_def<'a>() -> &'a [(&'a str, BuiltinFn)] {
    &[
        ("assert", BuiltinFn::Assert),
        ("assert_eq", BuiltinFn::AssertEq),
        ("print", BuiltinFn::Print),
        ("dbg", BuiltinFn::Dbg),
    ]
}
