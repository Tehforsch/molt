mod grammar;
mod interpreter;

pub(crate) use interpreter::Error as InterpreterError;
pub(crate) use interpreter::Interpreter;

use codespan_reporting::files::Files;
use proc_macro2::TokenStream;

use crate::rust_grammar::{Ident, Type};
use crate::{Error, Input, Mode};

const MAIN_FN_NAME: &str = "main";

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

#[derive(Debug)]
pub enum ResolveError {
    MainFnAndTopLevelStmtExist,
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::MainFnAndTopLevelStmtExist => {
                write!(f, "Both a main function and top level statements exist")
            }
        }
    }
}

type ResolveResult<T> = Result<T, ResolveError>;

fn resolve_file(file: grammar::MoltFile) -> ResolveResult<MoltFile> {
    if file.fns.iter().any(|f| f.name == MAIN_FN_NAME) && !file.stmts.is_empty() {
        return Err(ResolveError::MainFnAndTopLevelStmtExist);
    }
    Ok(MoltFile {
        fns: file
            .fns
            .into_iter()
            .map(resolve_fn)
            .collect::<ResolveResult<_>>()?,
        stmts: file
            .stmts
            .into_iter()
            .map(resolve_stmt)
            .collect::<ResolveResult<_>>()?,
    })
}

fn resolve_fn(f: grammar::MoltFn) -> ResolveResult<MoltFn> {
    Ok(MoltFn {
        name: f.name,
        args: f
            .args
            .into_iter()
            .map(resolve_fn_arg)
            .collect::<ResolveResult<_>>()?,
        stmts: f
            .stmts
            .into_iter()
            .map(resolve_stmt)
            .collect::<ResolveResult<_>>()?,
    })
}

fn resolve_fn_arg(arg: grammar::FnArg) -> ResolveResult<FnArg> {
    Ok(FnArg {
        var_name: arg.var_name,
        _type_: arg.type_,
    })
}

fn resolve_stmt(stmt: grammar::Stmt) -> ResolveResult<Stmt> {
    match stmt {
        grammar::Stmt::Assignment(a) => Ok(Stmt::Assignment(resolve_assignment(a)?)),
        grammar::Stmt::FnCall(f) => Ok(Stmt::FnCall(resolve_fn_call(f)?)),
        grammar::Stmt::Let(l) => Ok(Stmt::Let(resolve_let_stmt(l)?)),
    }
}

fn resolve_let_stmt(l: grammar::LetStmt) -> ResolveResult<LetStmt> {
    Ok(LetStmt {
        lhs: l.lhs,
        type_: l.type_,
        rhs: l.rhs.map(resolve_expr).transpose()?,
    })
}

fn resolve_assignment(a: grammar::Assignment) -> ResolveResult<Assignment> {
    Ok(Assignment {
        _lhs: a.lhs,
        _rhs: resolve_expr(a.rhs)?,
    })
}

fn resolve_fn_call(f: grammar::FnCall) -> ResolveResult<FnCall> {
    Ok(FnCall {
        fn_name: f.fn_name,
        args: f
            .args
            .into_iter()
            .map(resolve_expr)
            .collect::<ResolveResult<_>>()?,
    })
}

fn resolve_expr(expr: grammar::Expr) -> ResolveResult<Expr> {
    match expr {
        grammar::Expr::Pattern(p) => Ok(Expr::Pattern(resolve_pat(p)?)),
        grammar::Expr::Atom(name) => Ok(Expr::Atom(name)),
    }
}

fn resolve_pat(p: grammar::Pat) -> ResolveResult<Pat> {
    Ok(Pat { _pat: p.pat })
}

impl MoltFile {
    pub fn new(input: &Input) -> Result<Self, Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let file: grammar::MoltFile =
            crate::parser::parse_str(source, Mode::Molt).map_err(|e| Error::parse(e, file_id))?;
        resolve_file(file).map_err(|e| Error::Misc(e.to_string()))
    }
}
