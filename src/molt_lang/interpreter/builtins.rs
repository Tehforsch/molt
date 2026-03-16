use codespan_reporting::diagnostic::Label;

use crate::{
    Diagnostic, FileId, Id,
    modify::NodeSpec,
    molt_lang::{Interpreter, InterpreterError, builtin_fn::BuiltinFn, interpreter::value::Value},
};

use super::Result;

impl<'src> Interpreter<'src> {
    pub fn eval_builtin(&mut self, args: &[Value], builtin_fn: BuiltinFn) -> Result<Value> {
        match builtin_fn {
            BuiltinFn::Assert => self.eval_assert(args)?,
            BuiltinFn::AssertEq => self.eval_assert_eq(args)?,
            BuiltinFn::Print => {
                self.eval_print(args);
            }
            BuiltinFn::Dbg => {
                self.eval_dbg(args);
            }
        };
        Ok(Value::Unit)
    }

    fn eval_assert(&self, args: &[Value]) -> Result<()> {
        assert_eq!(args.len(), 1); // Resolver makes sure
        if let Value::Bool(b) = args[0] {
            if !b {
                Err(InterpreterError::Assertion)
            } else {
                Ok(())
            }
        } else {
            unreachable!() // TODO: make sure in type checker
        }
    }

    fn eval_assert_eq(&self, args: &[Value]) -> Result<()> {
        assert_eq!(args.len(), 2); // Resolver makes sure
        assert!(args[0].is_comparable_to(&args[1])); // TODO: make sure in type checker
        if args[0].eq(&args[1]) {
            Ok(())
        } else {
            Err(InterpreterError::Assertion)
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
                Value::Node(NodeSpec::Real(id)) => {
                    self.emit_diagnostic(self.context.real_id, *id);
                }
                Value::Node(_) => {
                    todo!();
                }
                val => self.print_val(val),
            }
        }
    }

    fn print_val(&self, val: &Value) {
        let content = match val {
            Value::String(s) => s,
            Value::Int(x) => &format!("{}", x),
            Value::Bool(b) => &format!("{}", b),
            Value::Node(NodeSpec::Real(id)) => {
                self.context.real_ctx().print(*id, self.context.real_src())
            }
            Value::Node(_) => {
                todo!()
            }
            Value::Unit => "()",
            Value::UserFn(f) => &format!("{:?}", f),
            Value::BuiltinFn(f) => &format!("{:?}", f),
        };
        self.context.writer().write_line(content);
    }

    fn emit_diagnostic(&self, file_id: FileId, real_node_to_print: Id) {
        let ctx = self.context.real_ctx();
        let span = ctx.get_span(real_node_to_print);
        let diagnostic =
            Diagnostic::note().with_labels(vec![Label::primary(file_id, span.byte_range())]);
        self.context
            .writer()
            .emit_diagnostic(self.context.input(), diagnostic)
    }
}
