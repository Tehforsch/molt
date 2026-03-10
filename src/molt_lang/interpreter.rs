mod builtins;
mod error;
mod value;

use std::collections::HashMap;

use super::{LetLhs, LetStmt, Lit};
use crate::{
    Id, Matcher, NodeType,
    molt_lang::{
        Expr, MAIN_FN_NAME, MoltFile, MoltFn, Stmt, Type,
        builtin_fn::{BuiltinFn, builtins},
        context::Context,
        interpreter::value::StmtValue,
    },
    rust_grammar::Node,
};

use value::Value;

use error::Result;

pub(crate) use error::Error;

#[derive(Clone, Copy)]
pub(crate) enum RuntimeFn<'a> {
    UserDefined(&'a MoltFn),
    Builtin(BuiltinFn),
}

#[derive(Default)]
struct VarStack {
    values: Vec<Value>,
}

impl VarStack {
    // TODO document why this is push/pop and not set/get (see recursion)
    fn push(&mut self, val: Value) {
        self.values.push(val)
    }

    fn pop(&mut self) -> Value {
        self.values.pop().unwrap()
    }

    fn get(&self) -> Value {
        // We unwrap here since the resolver will have reported
        // an undefined variable if the stack is empty at run time
        self.values.last().unwrap().clone()
    }

    fn try_get(&self) -> Option<Value> {
        self.values.last().cloned()
    }
}

pub(crate) struct Interpreter<'a> {
    vars: Vec<VarStack>,
    fns: HashMap<String, RuntimeFn<'a>>,
    context: Context<'a>,
}

impl<'a> Interpreter<'a> {
    pub(crate) fn run(file: &MoltFile, context: Context<'a>) -> Result<()> {
        let mut interpreter = Self {
            vars: (0..file.num_vars).map(|_| VarStack::default()).collect(),
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

    pub(crate) fn run_dry(file: &MoltFile, context: Context<'a>) -> Result<()> {
        let mut interpreter = Self {
            vars: (0..file.num_vars).map(|_| VarStack::default()).collect(),
            fns: builtins(),
            context,
        };
        for f in file.fns.iter() {
            interpreter.eval_fn_def(f)?;
        }
        interpreter.eval_main_fn_dry()?;
        Ok(())
    }

    fn eval_main_fn_dry(&mut self) -> Result<()> {
        let main_fn = self.lookup_fn(MAIN_FN_NAME)?;
        let RuntimeFn::UserDefined(f) = main_fn else {
            unreachable!()
        };
        if !f.args.is_empty() {
            return Err(Error::InvalidMainFn);
        }
        self.eval_fn_call(MAIN_FN_NAME, &[])?;
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
            RuntimeFn::Builtin(builtin_fn) => self.eval_builtin(args, builtin_fn)?,
        }
        Ok(StmtValue::Value(Value::Unit))
    }

    fn eval_user_defined_fn(&mut self, args: &[Value], user_fn: &MoltFn) -> Result<(), Error> {
        assert_eq!(user_fn.args.len(), args.len()); // todo verify this at resolution time
        for (arg, val) in user_fn.args.iter().zip(args.iter()) {
            self.vars[arg.var_id.0].push(val.clone());
        }
        self.eval_block(user_fn)?;
        for arg in user_fn.args.iter() {
            self.vars[arg.var_id.0].pop();
        }
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
            Expr::Atom(atom) => self.eval_atom(atom),
        }
    }

    fn eval_atom(&self, atom: &super::Atom) -> Result<Value> {
        match atom {
            super::Atom::Var(var_id) => Ok(self.vars[var_id.0].get().clone()),
            super::Atom::Lit(lit) => self.eval_lit(lit),
        }
    }

    fn eval_lit(&self, lit: &Lit) -> Result<Value> {
        match lit {
            Lit::Str(s) => Ok(Value::String(s.clone())),
            Lit::Int(x) => Ok(Value::Int(*x)),
            Lit::Bool(b) => Ok(Value::Bool(*b)),
        }
    }

    fn eval_let(&mut self, let_stmt: &LetStmt) -> Result<StmtValue> {
        match &let_stmt.lhs {
            LetLhs::Var(var_id) => {
                if let Some(rhs) = &let_stmt.rhs {
                    let val = self.eval_expr(rhs)?;
                    self.vars[var_id.0].push(val);
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
                    let bound_to = self.vars[var.var_id.0].try_get().map(|val| {
                        let Value::Node(bound_to) = val else {
                            todo!()
                            // error handling
                        };
                        bound_to
                    });
                    matcher.add_var(var.ctx_id, bound_to);
                }
                let new_bindings: Vec<_> = if let Value::Node(real) = real_id {
                    let match_ = matcher.get_matches(pat.node, real);
                    if let Some(match_) = match_ {
                        match_
                            .iter_vars()
                            .map(|var| {
                                let bound_to = match_.get_binding(var).id.unwrap();
                                let var_id = pat.get_var_id(var);
                                (var_id, Value::Node(bound_to))
                            })
                            .collect()
                    } else {
                        return Ok(StmtValue::NoMatch);
                    }
                } else {
                    // error handling
                    todo!()
                };

                for (id, val) in new_bindings {
                    self.vars[id.0].push(val);
                }
                Ok(StmtValue::Value(Value::Unit))
            }
        }
    }

    fn lookup_fn(&mut self, fn_name: &str) -> std::result::Result<RuntimeFn<'a>, Error> {
        self.fns
            .get(fn_name)
            .cloned()
            .ok_or(Error::undefined_fn(fn_name))
    }
}
