use std::collections::HashMap;

use crate::molt_lang::{Interpreter, MoltFn, interpreter::value::Value};

#[derive(Clone, Copy)]
pub enum RuntimeFn<'a> {
    UserDefined(&'a MoltFn),
    Builtin(BuiltinFn),
}

#[derive(Clone, Copy)]
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
