use super::*;

pub(crate) trait Visitor {
    fn visit_file(&mut self, _file: &MoltFile) {}
    fn visit_fn(&mut self, _fn: &MoltFn) {}
    fn visit_fn_arg(&mut self, _arg: &FnArg) {}
    fn visit_stmt(&mut self, _stmt: &Stmt) {}
    fn visit_expr(&mut self, _expr: &Expr) {}
    fn visit_let_stmt(&mut self, _let_stmt: &LetStmt) {}
    fn visit_fn_call(&mut self, _fn_call: &FnCall) {}
    fn visit_pat(&mut self, _pat: &Pat) {}
    fn visit_type(&mut self, _type: &Type) {}
    fn visit_ident(&mut self, _ident: &Ident) {}
}

pub(crate) fn walk_file<V: Visitor + ?Sized>(v: &mut V, file: &MoltFile) {
    v.visit_file(file);
    for fn_ in &file.fns {
        walk_fn(v, fn_);
    }
}

fn walk_fn<V: Visitor + ?Sized>(v: &mut V, fn_: &MoltFn) {
    v.visit_fn(fn_);
    for arg in &fn_.args {
        walk_fn_arg(v, arg);
    }
    for stmt in &fn_.stmts {
        walk_stmt(v, stmt);
    }
}

fn walk_fn_arg<V: Visitor + ?Sized>(v: &mut V, arg: &FnArg) {
    v.visit_fn_arg(arg);
    v.visit_ident(&arg.var_name);
    v.visit_type(&arg.type_);
}

fn walk_stmt<V: Visitor + ?Sized>(v: &mut V, stmt: &Stmt) {
    v.visit_stmt(stmt);
    match stmt {
        Stmt::ExprStmt(expr) => walk_expr(v, expr),
        Stmt::Let(let_stmt) => walk_let_stmt(v, let_stmt),
    }
}

fn walk_expr<V: Visitor + ?Sized>(v: &mut V, expr: &Expr) {
    v.visit_expr(expr);
    match expr {
        Expr::FnCall(fn_call) => walk_fn_call(v, fn_call),
        Expr::Atom(ident) => v.visit_ident(ident),
    }
}

fn walk_let_stmt<V: Visitor + ?Sized>(v: &mut V, let_stmt: &LetStmt) {
    v.visit_let_stmt(let_stmt);
    match &let_stmt.lhs {
        LetLhs::Var(ident) => v.visit_ident(ident),
        LetLhs::Pat(pat) => v.visit_pat(pat),
    }
    v.visit_type(&let_stmt._type_);
    if let Some(rhs) = &let_stmt.rhs {
        walk_expr(v, rhs);
    }
}

fn walk_fn_call<V: Visitor + ?Sized>(v: &mut V, fn_call: &FnCall) {
    v.visit_fn_call(fn_call);
    v.visit_ident(&fn_call.fn_name);
    for arg in &fn_call.args {
        walk_expr(v, arg);
    }
}
