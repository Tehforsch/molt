mod builtins;
mod error;
mod value;

use std::collections::HashMap;

use super::{LetLhs, LetStmt};
use crate::{
    Id, Matcher, NodeType,
    molt_lang::{
        Expr, MAIN_FN_NAME, MoltFile, MoltFn, Stmt, Type,
        context::Context,
        interpreter::{builtins::BuiltinFn, value::StmtValue},
    },
    rust_grammar::{Ident, Node},
};

use {builtins::builtins, value::Value};

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
        self.variables.insert(var_name, val.clone());
    }
}

#[derive(Clone, Copy)]
pub(crate) enum RuntimeFn<'a> {
    UserDefined(&'a MoltFn),
    Builtin(BuiltinFn),
}

pub(crate) struct Interpreter<'a> {
    scopes: Vec<Scope>,
    fns: HashMap<String, RuntimeFn<'a>>,

    context: Context<'a>,
}

impl<'a> Interpreter<'a> {
    pub(crate) fn run(file: &MoltFile, context: Context<'a>) -> Result<()> {
        let mut interpreter = Self {
            scopes: vec![Scope::default()],
            fns: builtins(),
            context,
        };
        for f in file.fns.iter() {
            interpreter.eval_fn_def(f)?;
        }
        for node in interpreter.context.real_ctx.iter() {
            // TODO reset interpreter here
            interpreter.eval_main_fn_on_node(node)?;
        }
        Ok(())
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
                .context
                .real_ctx
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

    fn eval_fn_def(&mut self, f: &'a MoltFn) -> Result<()> {
        self.fns
            .insert(f.name.to_string(), RuntimeFn::UserDefined(f));
        Ok(())
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<StmtValue> {
        match stmt {
            Stmt::Let(let_stmt) => self.eval_let(let_stmt),
            Stmt::ExprStmt(expr) => {
                self.eval_expr(expr)?;
                Ok(StmtValue::Value(Value::Unit))
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
            RuntimeFn::Builtin(builtin_fn) => self.eval_builtin(args, builtin_fn),
        }
        Ok(StmtValue::Value(Value::Unit))
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
                StmtValue::NoMatch => break,
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
            Expr::FnCall(fn_call) => {
                let args = self.make_fn_args(&fn_call.args)?;
                match self.eval_fn_call(&fn_call.fn_name.to_string(), &args)? {
                    StmtValue::Value(v) => Ok(v),
                    StmtValue::NoMatch => Ok(Value::Unit),
                }
            }
            Expr::Atom(name) => Ok(self.lookup_var(name)?.clone()),
        }
    }

    fn eval_let(&mut self, let_stmt: &LetStmt) -> Result<StmtValue> {
        match &let_stmt.lhs {
            LetLhs::Var(var_name) => {
                if let Some(rhs) = &let_stmt.rhs {
                    let val = self.eval_expr(rhs)?;
                    self.active_scope_mut().insert(var_name.clone(), &val);
                }
                Ok(StmtValue::Value(Value::Unit))
            }
            LetLhs::Pat(pat) => {
                let real_id = if let Some(expr) = &let_stmt.rhs {
                    self.eval_expr(expr)?
                } else {
                    // error handling
                    todo!()
                };
                let rules = crate::rule::Rules::default();
                let mut matcher = Matcher::from_interpreter_ctx(&self.context, &pat.ctx, &rules);
                for var in pat.vars.iter() {
                    // Look up if this variable was previously bound to
                    // something.
                    let bound_to = self.lookup_var(&var.ident).ok().map(|val| {
                        let Value::Node(bound_to) = val else {
                            todo!()
                            // error handling
                        };
                        *bound_to
                    });
                    matcher.add_var(var.id, bound_to);
                }
                let new_bindings: Vec<_> = if let Value::Node(real) = real_id {
                    let match_ = matcher.get_matches(pat.node, real);
                    if let Some(match_) = match_ {
                        match_
                            .iter_vars()
                            .map(|var| {
                                let bound_to = match_.get_binding(var).id.unwrap();
                                (pat.ctx.get_var(var).ident().clone(), Value::Node(bound_to))
                            })
                            .collect()
                    } else {
                        return Ok(StmtValue::NoMatch);
                    }
                } else {
                    // error handling
                    todo!()
                };

                for (ident, val) in new_bindings {
                    self.active_scope_mut().insert(ident, &val);
                }
                Ok(StmtValue::Value(Value::Unit))
            }
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

    fn lookup_fn(&mut self, fn_name: &str) -> std::result::Result<RuntimeFn<'a>, Error> {
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
        Err(Error::undefined_var(&name.to_string()))
    }
}
