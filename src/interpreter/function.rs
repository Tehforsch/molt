use std::collections::HashMap;

use crate::molt_lang::MoltFn;

pub enum RuntimeFn<'a> {
    UserDefined(UserFn<'a>),
    Builtin(BuiltinFn),
}

pub struct UserFn<'a> {
    pub inner: &'a MoltFn,
}

pub enum BuiltinFn {
    Print,
}

pub fn builtins<'a>() -> HashMap<String, RuntimeFn<'a>> {
    [("print", RuntimeFn::Builtin(BuiltinFn::Print))]
        .into_iter()
        .map(|(name, f)| (name.to_string(), f))
        .collect()
}
