use std::collections::HashMap;

use crate::molt_lang::{Interpreter, MoltFn, interpreter::value::Value};

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

impl<'src> Interpreter<'src> {
    pub(super) fn eval_print(&self, args: &[Value]) {
        for val in args.iter() {
            match val {
                Value::String(_) => todo!(),
                Value::Node(id) => {
                    self.src.print(*id);
                }
                Value::Null => {
                    println!("Null");
                }
            }
        }
    }
}
