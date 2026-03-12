mod builtins;
mod error;
mod value;

use super::{LetLhs, LetStmt, Lit};
use crate::{
    Id, Matcher, Node,
    modify::Modification,
    molt_lang::{
        Assignment, Expr, FnId, MoltFile, MoltFn, Stmt, Type, VarId, context::Context,
        interpreter::value::StmtValue,
    },
    node::NodeType,
    storage::Storage,
};

use value::Value;

use error::Result;

pub(crate) use error::Error;

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
    vars: Storage<VarId, VarStack>,
    fns: Storage<FnId, &'a MoltFn>,
    context: Context<'a>,
    modifications: Vec<Modification>,
}

impl<'a> Interpreter<'a> {
    fn new(file: &'a MoltFile, context: Context<'a>) -> Interpreter<'a> {
        let mut interpreter = Self {
            vars: file.var_names.iter().map(|_| VarStack::default()).collect(),
            fns: file.fns.iter().collect(),
            context,
            modifications: vec![],
        };
        for (id, f) in file.fns.enumerate() {
            interpreter.eval_fn_def(f, id);
        }
        for (id, f) in file.iter_builtins() {
            interpreter.vars[id].push(Value::BuiltinFn(f));
        }
        interpreter
    }

    pub(crate) fn run(file: &'a MoltFile, context: Context<'a>) -> Result<()> {
        let mut interpreter = Interpreter::new(file, context);
        for node in interpreter.context.real_ctx.iter() {
            // TODO reset interpreter here
            interpreter.eval_main_fn_on_node(node, file.main_fn_id)?;
        }
        Ok(())
    }

    pub(crate) fn run_dry(file: &MoltFile, context: Context<'a>) -> Result<()> {
        let mut interpreter = Interpreter::new(file, context);
        interpreter.eval_main_fn_dry(file.main_fn_id)?;
        Ok(())
    }

    fn eval_main_fn_dry(&mut self, main_fn_id: FnId) -> Result<()> {
        let f = self.fns[main_fn_id];
        if !f.args.is_empty() {
            return Err(Error::InvalidMainFn);
        }
        self.eval_user_defined_fn(&[], self.fns[main_fn_id])?;
        Ok(())
    }

    fn eval_main_fn_on_node(&mut self, node: Id, main_fn_id: FnId) -> Result<()> {
        let value = Value::Node(node);
        // Special handling to filter out non-matching node types
        // in the main function
        let f = self.fns[main_fn_id];
        if f.args.len() != 1 {
            return Err(Error::InvalidMainFn);
        } else {
            let Type::Kind(kind) = &f.args[0].type_ else {
                unreachable!() // TODO: Ensure type checker makes sure this never happens.
            };
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
        self.eval_user_defined_fn(&[value], self.fns[main_fn_id])?;
        Ok(())
    }

    fn eval_fn_def(&mut self, f: &'a MoltFn, id: FnId) {
        self.vars[f.id].push(Value::UserFn(id));
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<StmtValue> {
        match stmt {
            Stmt::Let(let_stmt) => self.eval_let(let_stmt),
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(StmtValue::Value(Value::Unit))
            }
            Stmt::Return(ret) => {
                let val = if let Some(expr) = &ret.expr {
                    self.eval_expr(expr)?
                } else {
                    Value::Unit
                };
                Ok(StmtValue::Return(val))
            }
            Stmt::Assignment(assignment) => self.eval_assignment(assignment),
        }
    }

    fn eval_fn_call(&mut self, fn_var_id: VarId, args: &[Value]) -> Result<StmtValue> {
        match self.vars[fn_var_id].get() {
            Value::UserFn(fn_id) => self.eval_user_defined_fn(args, self.fns[fn_id]),
            Value::BuiltinFn(builtin_fn) => {
                self.eval_builtin(args, builtin_fn).map(StmtValue::Value)
            }
            _ => unreachable!(), // TODO: verify in type checker.
        }
    }

    fn eval_user_defined_fn(
        &mut self,
        args: &[Value],
        user_fn: &MoltFn,
    ) -> Result<StmtValue, Error> {
        assert_eq!(user_fn.args.len(), args.len()); // todo verify this at resolution time
        for (arg, val) in user_fn.args.iter().zip(args.iter()) {
            self.vars[arg.var_id].push(val.clone());
        }
        let val = self.eval_block(user_fn)?;
        let val = match val {
            StmtValue::NoMatch => Value::Unit,
            StmtValue::Value(_) => Value::Unit,
            StmtValue::Return(value) => value,
        };
        for arg in user_fn.args.iter() {
            self.vars[arg.var_id].pop();
        }
        Ok(StmtValue::Value(val))
    }

    fn eval_block(&mut self, user_fn: &MoltFn) -> Result<StmtValue, Error> {
        for stmt in user_fn.stmts.iter() {
            match self.eval_stmt(stmt)? {
                StmtValue::NoMatch => return Ok(StmtValue::NoMatch),
                StmtValue::Return(val) => return Ok(StmtValue::Return(val)),
                StmtValue::Value(_) => {}
            }
        }
        Ok(StmtValue::Value(Value::Unit))
    }

    fn make_fn_args(&mut self, args: &[Expr]) -> Result<Vec<Value>> {
        args.iter().map(|expr| self.eval_expr(expr)).collect()
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::FnCall(fn_call) => {
                let args = self.make_fn_args(&fn_call.args)?;
                match self.eval_fn_call(fn_call.id, &args)? {
                    StmtValue::Value(v) => Ok(v),
                    StmtValue::NoMatch => Ok(Value::Unit),
                    StmtValue::Return(_) => unreachable!(),
                }
            }
            Expr::Atom(atom) => self.eval_atom(atom),
            Expr::Pat(pat) => self.eval_pat(pat),
        }
    }

    fn eval_atom(&self, atom: &super::Atom) -> Result<Value> {
        match atom {
            super::Atom::Var(var_id) => Ok(self.vars[*var_id].get().clone()),
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

    fn eval_pat(&self, pat: &super::PatId) -> Result<Value> {
        let pat = self.context.get_pat(*pat);
        Ok(Value::Node(pat.node))
    }

    fn eval_assignment(&mut self, assignment: &Assignment) -> Result<StmtValue> {
        let val = self.vars[assignment.lhs].pop();
        let new_val = match val {
            Value::String(_)
            | Value::Int(_)
            | Value::Bool(_)
            | Value::Unit
            | Value::UserFn(_)
            | Value::BuiltinFn(_) => self.eval_expr(&assignment.rhs)?,
            Value::Node(id) => {
                let Value::Node(new_val) = self.eval_expr(&assignment.rhs)? else {
                    unreachable!() // type checker ensures
                };
                self.modifications.push(Modification {
                    old: id,
                    new: new_val,
                });
                Value::Node(new_val)
            }
        };
        self.vars[assignment.lhs].push(new_val);
        Ok(StmtValue::Value(Value::Unit))
    }

    fn eval_let(&mut self, let_stmt: &LetStmt) -> Result<StmtValue> {
        match &let_stmt.lhs {
            LetLhs::Var(var_id) => {
                if let Some(rhs) = &let_stmt.rhs {
                    let val = self.eval_expr(rhs)?;
                    self.vars[*var_id].push(val);
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
                let pat = self.context.get_pat(*pat);
                let rules = crate::rule::Rules::default();
                let mut matcher = Matcher::from_interpreter_ctx(&self.context, &pat.ctx, &rules);
                for var in pat.vars.iter() {
                    // Look up if this variable was previously bound to
                    // something.
                    let bound_to = self.vars[var.var_id].try_get().map(|val| {
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
                    self.vars[id].push(val);
                }
                Ok(StmtValue::Value(Value::Unit))
            }
        }
    }
}
