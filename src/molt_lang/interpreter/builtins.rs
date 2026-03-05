use std::collections::HashMap;

use codespan_reporting::diagnostic::Label;

use crate::{
    Diagnostic, FileId, Id,
    molt_lang::{
        Interpreter,
        interpreter::{RuntimeFn, value::Value},
    },
};

#[derive(Clone, Copy)]
pub enum BuiltinFn {
    Print,
    Dbg,
}

pub fn builtins<'a>() -> HashMap<String, RuntimeFn<'a>> {
    [
        ("print", RuntimeFn::Builtin(BuiltinFn::Print)),
        ("dbg", RuntimeFn::Builtin(BuiltinFn::Dbg)),
    ]
    .into_iter()
    .map(|(name, f)| (name.to_string(), f))
    .collect()
}

impl<'src> Interpreter<'src> {
    pub fn eval_builtin(&mut self, args: &[Value], builtin_fn: BuiltinFn) {
        match builtin_fn {
            BuiltinFn::Print => self.eval_print(args),
            BuiltinFn::Dbg => self.eval_dbg(args),
        }
    }

    fn eval_print(&self, args: &[Value]) {
        for val in args.iter() {
            self.print_val(val);
        }
    }

    fn eval_dbg(&self, args: &[Value]) {
        for val in args.iter() {
            match val {
                Value::Node(id) => {
                    self.emit_diagnostic(self.src.file_id, *id);
                }
                val => self.print_val(val),
            }
        }
    }

    fn print_val(&self, val: &Value) {
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

    fn emit_diagnostic(&self, file_id: FileId, real_node_to_print: Id) {
        let ctx = self.src.ctx;
        let span = ctx.get_span(real_node_to_print);
        let diagnostic =
            Diagnostic::note().with_labels(vec![Label::primary(file_id, span.byte_range())]);
        self.writer.emit_diagnostic(diagnostic)
    }
}
