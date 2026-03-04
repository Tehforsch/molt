mod grammar;
mod interpreter;

pub(crate) use interpreter::Error as InterpreterError;
pub(crate) use interpreter::Interpreter;

use codespan_reporting::files::Files;
use proc_macro2::TokenStream;

use crate::rust_grammar::{Ident, Type};
use crate::{Error, Input, Mode};

pub struct MoltFile {
    pub fns: Vec<MoltFn>,
    pub stmts: Vec<Stmt>,
}

pub struct MoltFn {
    pub name: Ident,
    pub args: Vec<FnArg>,
    pub stmts: Vec<Stmt>,
}

pub struct FnArg {
    pub var_name: Ident,
    pub _type_: Type,
}

pub enum Stmt {
    Assignment(Assignment),
    FnCall(FnCall),
    Let(LetStmt),
}

pub struct LetStmt {
    pub lhs: Ident,
    pub type_: Type,
    pub rhs: Option<Expr>,
}

pub struct Assignment {
    pub _lhs: Ident,
    pub _rhs: Expr,
}

pub struct FnCall {
    pub fn_name: Ident,
    pub args: Vec<Expr>,
}

pub enum Expr {
    Pattern(Pat),
    Atom(Ident),
}

pub struct Pat {
    pub _pat: TokenStream,
}

fn resolve_file(file: grammar::MoltFile) -> MoltFile {
    MoltFile {
        fns: file.fns.into_iter().map(resolve_fn).collect(),
        stmts: file.stmts.into_iter().map(resolve_stmt).collect(),
    }
}

fn resolve_fn(f: grammar::MoltFn) -> MoltFn {
    MoltFn {
        name: f.name,
        args: f.args.into_iter().map(resolve_fn_arg).collect(),
        stmts: f.stmts.into_iter().map(resolve_stmt).collect(),
    }
}

fn resolve_fn_arg(arg: grammar::FnArg) -> FnArg {
    FnArg {
        var_name: arg.var_name,
        _type_: arg.type_,
    }
}

fn resolve_stmt(stmt: grammar::Stmt) -> Stmt {
    match stmt {
        grammar::Stmt::Assignment(a) => Stmt::Assignment(resolve_assignment(a)),
        grammar::Stmt::FnCall(f) => Stmt::FnCall(resolve_fn_call(f)),
        grammar::Stmt::Let(l) => Stmt::Let(resolve_let_stmt(l)),
    }
}

fn resolve_let_stmt(l: grammar::LetStmt) -> LetStmt {
    LetStmt {
        lhs: l.lhs,
        type_: l.type_,
        rhs: l.rhs.map(resolve_expr),
    }
}

fn resolve_assignment(a: grammar::Assignment) -> Assignment {
    Assignment {
        _lhs: a.lhs,
        _rhs: resolve_expr(a.rhs),
    }
}

fn resolve_fn_call(f: grammar::FnCall) -> FnCall {
    FnCall {
        fn_name: f.fn_name,
        args: f.args.into_iter().map(resolve_expr).collect(),
    }
}

fn resolve_expr(expr: grammar::Expr) -> Expr {
    match expr {
        grammar::Expr::Pattern(p) => Expr::Pattern(resolve_pat(p)),
        grammar::Expr::Atom(name) => Expr::Atom(name),
    }
}

fn resolve_pat(p: grammar::Pat) -> Pat {
    Pat { _pat: p.pat }
}

impl MoltFile {
    pub fn new(input: &Input) -> Result<Self, Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let file: grammar::MoltFile =
            crate::parser::parse_str(source, Mode::Molt).map_err(|e| Error::parse(e, file_id))?;
        Ok(resolve_file(file))
    }
}
