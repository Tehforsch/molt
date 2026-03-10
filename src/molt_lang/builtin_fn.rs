#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BuiltinFn {
    Assert,
    AssertEq,
    Print,
    Dbg,
}

pub fn builtins_def<'a>() -> &'a [(&'a str, BuiltinFn)] {
    &[
        ("assert", BuiltinFn::Assert),
        ("assert_eq", BuiltinFn::AssertEq),
        ("print", BuiltinFn::Print),
        ("dbg", BuiltinFn::Dbg),
    ]
}
