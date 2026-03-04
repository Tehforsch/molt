mod error;
mod function;
mod value;

use std::collections::HashMap;

use super::{LetLhs, LetStmt};
use crate::{
    Ctx, Diagnostic, Id, NodeType,
    molt_lang::{Expr, MAIN_FN_NAME, MoltFile, MoltFn, Stmt, Type, interpreter::value::StmtValue},
    rust_grammar::{Ident, Node},
};

use {
    function::{BuiltinFn, builtins},
    value::Value,
};

use error::Result;

pub(crate) use error::Error;
pub(crate) use function::RuntimeFn;

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
            interpreter.eval_main_fn_on_node(node)?;
        }
        Ok(interpreter.diagnostics)
    }

    fn eval_main_fn_on_node(&mut self, node: Id) -> Result<()> {
        let value = Value::Node(node);
        // Special handling to filter out non-matching node types
        // in the main function
        let main_fn = self.lookup_fn(MAIN_FN_NAME)?;
        let RuntimeFn::UserDefined(f) = main_fn else {
            unreachable!()
        };
        if f.args.len() != 1 {
            return Err(Error::InvalidMainFn);
        } else {
            let Type::Kind(kind) = &f.args[0].type_;
            if !self
                .src
                .ctx
                .get::<Node>(node)
                .unwrap_item()
                .is_of_kind(*kind)
            {
                return Ok(());
            }
        }
        self.eval_fn_call(MAIN_FN_NAME, &[value])?;
        Ok(())
    }

    fn eval_fn_def(&mut self, f: &'src MoltFn) -> Result<()> {
        self.fns
            .insert(f.name.to_string(), RuntimeFn::UserDefined(f));
        Ok(())
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<StmtValue> {
        match stmt {
            Stmt::Assignment(_) => todo!(),
            Stmt::Let(let_stmt) => self.eval_let(let_stmt),
            Stmt::FnCall(fn_call) => {
                let args = self.make_fn_args(&fn_call.args)?;
                self.eval_fn_call(&fn_call.fn_name.to_string(), &args)
            }
        }
    }

    fn eval_fn_call(&mut self, fn_name: &str, args: &[Value]) -> Result<StmtValue> {
        match self.lookup_fn(fn_name)? {
            // Inlining to make borrow checker happy. A simple alternative
            // would be to clone the fn definitions.
            RuntimeFn::UserDefined(user_fn) => {
                self.eval_user_defined_fn(args, user_fn)?;
            }
            RuntimeFn::Builtin(builtin_fn) => match builtin_fn {
                BuiltinFn::Print => self.eval_print(args),
            },
        }
        Ok(StmtValue::Value(Value::Null))
    }

    fn eval_user_defined_fn(&mut self, args: &[Value], user_fn: &MoltFn) -> Result<(), Error> {
        let mut scope = Scope::child_of(self.active_scope(), self.next_scope_index());
        assert_eq!(user_fn.args.len(), args.len());
        for (arg, val) in user_fn.args.iter().zip(args.iter()) {
            scope.insert(arg.var_name.clone(), val);
        }
        self.scopes.push(scope);
        self.eval_block(user_fn)?;
        self.scopes.pop();
        Ok(())
    }

    fn eval_block(&mut self, user_fn: &MoltFn) -> Result<(), Error> {
        for stmt in user_fn.stmts.iter() {
            match self.eval_stmt(stmt)? {
                StmtValue::Return => break,
                StmtValue::Value(_) => {}
            }
        }
        Ok(())
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

    fn eval_let(&mut self, let_stmt: &LetStmt) -> Result<StmtValue> {
        match &let_stmt.lhs {
            LetLhs::Var(var_name) => {
                if self.lookup_var(var_name).is_ok() {
                    todo!()
                }
                if let Some(rhs) = &let_stmt.rhs {
                    let val = self.eval_expr(rhs)?;
                    self.active_scope_mut().insert(var_name.clone(), &val);
                }
                Ok(StmtValue::Value(Value::Null))
            }
            LetLhs::Pat(_) => todo!(),
        }
    }

    fn active_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn active_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn next_scope_index(&self) -> usize {
        self.scopes.len() - 1
    }

    fn lookup_fn(&mut self, fn_name: &str) -> std::result::Result<RuntimeFn<'src>, Error> {
        self.fns
            .get(fn_name)
            .cloned()
            .ok_or(Error::undefined_fn(fn_name))
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
}
