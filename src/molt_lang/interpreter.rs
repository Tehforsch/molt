mod error;
mod function;
mod value;

use std::collections::HashMap;

use crate::{
    Ctx, Diagnostic, Id,
    molt_lang::{Expr, MAIN_FN_NAME, MoltFile, MoltFn, Stmt},
    rust_grammar::{Ident, Node},
};
use {
    function::{BuiltinFn, RuntimeFn, UserFn, builtins},
    value::Value,
};

use error::Result;

pub(crate) use error::Error;

type ScopeIndex = usize;

#[derive(Default)]
struct Scope {
    parent: Option<ScopeIndex>,
    variables: HashMap<Ident, Value>,
    index: ScopeIndex,
}

impl Scope {
    fn child_of(active_scope: &Scope, index: usize) -> Scope {
        Self {
            parent: Some(active_scope.index),
            variables: HashMap::default(),
            index,
        }
    }

    fn insert(&mut self, var_name: Ident, val: &Value) {
        let existing = self.variables.insert(var_name, val.clone());
        assert!(existing.is_none());
    }
}

pub(crate) struct Interpreter<'src> {
    diagnostics: Vec<Diagnostic>,
    scopes: Vec<Scope>,
    fns: HashMap<String, RuntimeFn<'src>>,
    src: SrcData<'src>,
}

struct SrcData<'src> {
    ctx: &'src Ctx<Node>,
    src: &'src str,
}

impl<'src> SrcData<'src> {
    fn print(&self, id: Id) {
        println!("{}", self.ctx.print(id, self.src))
    }
}

impl<'src> Interpreter<'src> {
    pub(crate) fn run(
        file: &MoltFile,
        src: &'src str,
        ctx: &'src Ctx<Node>,
    ) -> Result<Vec<Diagnostic>> {
        let mut interpreter = Self {
            diagnostics: vec![],
            scopes: vec![Scope::default()],
            fns: builtins(),
            src: SrcData { ctx, src },
        };
        for f in file.fns.iter() {
            interpreter.eval_fn_def(f)?;
        }
        for node in ctx.iter() {
            let value = Value::Node(node);
            interpreter.eval_fn_call(MAIN_FN_NAME, &[value])?;
        }
        Ok(interpreter.diagnostics)
    }

    fn eval_fn_def(&mut self, f: &'src MoltFn) -> Result<()> {
        self.fns.insert(
            f.name.to_string(),
            RuntimeFn::UserDefined(UserFn { inner: f }),
        );
        Ok(())
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Assignment(_) => todo!(),
            Stmt::Let(_) => todo!(),
            Stmt::FnCall(fn_call) => {
                let args = self.make_fn_args(&fn_call.args)?;
                self.eval_fn_call(&fn_call.fn_name.to_string(), &args)
            }
        }
    }

    fn eval_fn_call(&mut self, fn_name: &str, args: &[Value]) -> Result<()> {
        match self.fns.get(fn_name).ok_or(Error::UndefinedFn)? {
            // Inlining to make borrow checker happy. A simple alternative
            // would be to clone the fn definitions.
            RuntimeFn::UserDefined(user_fn) => {
                let mut scope = Scope::child_of(self.active_scope(), self.next_scope_index());
                assert_eq!(user_fn.inner.args.len(), args.len()); // TODO check
                for (arg, val) in user_fn.inner.args.iter().zip(args.iter()) {
                    scope.insert(arg.var_name.clone(), val);
                }
                self.scopes.push(scope);
                for stmt in user_fn.inner.stmts.iter() {
                    self.eval_stmt(stmt)?;
                }
                self.scopes.pop();
                Ok(())
            }
            RuntimeFn::Builtin(builtin_fn) => {
                match builtin_fn {
                    BuiltinFn::Print => self.eval_print(args),
                }
                Ok(())
            }
        }
    }

    fn make_fn_args(&mut self, args: &[Expr]) -> Result<Vec<Value>> {
        args.iter().map(|expr| self.eval_expr(expr)).collect()
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Pattern(_) => todo!(),
            Expr::Atom(name) => Ok(self.lookup_var(name)?.clone()),
        }
    }

    fn active_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn next_scope_index(&self) -> usize {
        self.scopes.len() - 1
    }

    fn lookup_var(&self, name: &Ident) -> Result<&Value> {
        let mut scope_idx = Some(self.scopes.len() - 1);
        while let Some(idx) = scope_idx {
            let scope = &self.scopes[idx];
            if let Some(val) = scope.variables.get(name) {
                return Ok(val);
            }
            scope_idx = scope.parent;
        }
        Err(Error::UndefinedVar)
    }

    fn eval_print(&self, args: &[Value]) {
        for val in args.iter() {
            match val {
                Value::String(_) => todo!(),
                Value::Node(id) => {
                    self.src.print(*id);
                }
            }
        }
    }
}
