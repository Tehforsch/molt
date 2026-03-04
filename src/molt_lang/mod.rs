mod grammar;
mod interpreter;

pub(crate) use interpreter::Error as InterpreterError;
pub(crate) use interpreter::Interpreter;

use codespan_reporting::files::Files;
use proc_macro2::TokenStream;

use crate::rust_grammar::{Ident, Type};
use crate::{Error, Input, Mode};

const MAIN_FN_NAME: &str = "main";
const INPUT_VAR_NAME: &str = "input";

#[derive(Debug)]
pub struct MoltFile {
    pub fns: Vec<MoltFn>,
}

#[derive(Debug)]
pub enum FnName {
    Ident(Ident),
    ImplicitMain,
}

impl std::fmt::Display for FnName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FnName::Ident(ident) => write!(f, "{}", ident),
            FnName::ImplicitMain => write!(f, "{MAIN_FN_NAME}"),
        }
    }
}

#[derive(Debug)]
pub struct MoltFn {
    pub name: FnName,
    pub args: Vec<FnArg>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct FnArg {
    pub var_name: Ident,
    pub _type_: Type,
}

#[derive(Debug)]
pub enum Stmt {
    Assignment(Assignment),
    FnCall(FnCall),
    Let(LetStmt),
}

#[derive(Debug)]
pub struct LetStmt {
    pub lhs: Ident,
    pub type_: Type,
    pub rhs: Option<Expr>,
}

#[derive(Debug)]
pub struct Assignment {
    pub _lhs: Ident,
    pub _rhs: Expr,
}

#[derive(Debug)]
pub struct FnCall {
    pub fn_name: Ident,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Pattern(Pat),
    Atom(Ident),
}

#[derive(Debug)]
pub struct Pat {
    pub _pat: TokenStream,
}

#[derive(Debug)]
pub enum ResolveError {
    MainFnAndTopLevelStmtExist,
    InvalidInputVarName,
    NoInputVarName,
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::MainFnAndTopLevelStmtExist => {
                write!(f, "Both a main function and top level statements exist")
            }
            ResolveError::InvalidInputVarName => {
                write!(f, "The first variable needs to have name `input`.")
            }
            ResolveError::NoInputVarName => {
                write!(f, "No `input` variable declared.")
            }
        }
    }
}

type Result<T, E = ResolveError> = std::result::Result<T, E>;

fn resolve_file(file: grammar::MoltFile) -> Result<MoltFile> {
    let grammar::MoltFile { fns, mut stmts } = file;
    let mut fns: Vec<_> = fns.into_iter().map(resolve_fn).collect::<Result<_>>()?;
    // Clearly very ugly, but we need to get started somehow.
    if !stmts.is_empty() {
        if fns.iter().any(|f| {
            if let FnName::Ident(ident) = &f.name {
                ident == MAIN_FN_NAME
            } else {
                false
            }
        }) {
            return Err(ResolveError::MainFnAndTopLevelStmtExist);
        }
        if let grammar::Stmt::Let(l) = stmts.remove(0) {
            if l.lhs != INPUT_VAR_NAME {
                return Err(ResolveError::InvalidInputVarName);
            }
            fns.push(MoltFn {
                name: FnName::ImplicitMain,
                args: vec![FnArg {
                    var_name: l.lhs.clone(),
                    _type_: l.type_,
                }],
                stmts: stmts
                    .into_iter()
                    .map(resolve_stmt)
                    .collect::<Result<Vec<_>>>()?,
            });
        } else {
            return Err(ResolveError::NoInputVarName);
        }
    }
    Ok(MoltFile { fns })
}

fn resolve_fn(f: grammar::MoltFn) -> Result<MoltFn> {
    Ok(MoltFn {
        name: FnName::Ident(f.name),
        args: f
            .args
            .into_iter()
            .map(resolve_fn_arg)
            .collect::<Result<_>>()?,
        stmts: f
            .stmts
            .into_iter()
            .map(resolve_stmt)
            .collect::<Result<_>>()?,
    })
}

fn resolve_fn_arg(arg: grammar::FnArg) -> Result<FnArg> {
    Ok(FnArg {
        var_name: arg.var_name,
        _type_: arg.type_,
    })
}

fn resolve_stmt(stmt: grammar::Stmt) -> Result<Stmt> {
    match stmt {
        grammar::Stmt::Assignment(a) => Ok(Stmt::Assignment(resolve_assignment(a)?)),
        grammar::Stmt::FnCall(f) => Ok(Stmt::FnCall(resolve_fn_call(f)?)),
        grammar::Stmt::Let(l) => Ok(Stmt::Let(resolve_let_stmt(l)?)),
    }
}

fn resolve_let_stmt(l: grammar::LetStmt) -> Result<LetStmt> {
    Ok(LetStmt {
        lhs: l.lhs,
        type_: l.type_,
        rhs: l.rhs.map(resolve_expr).transpose()?,
    })
}

fn resolve_assignment(a: grammar::Assignment) -> Result<Assignment> {
    Ok(Assignment {
        _lhs: a.lhs,
        _rhs: resolve_expr(a.rhs)?,
    })
}

fn resolve_fn_call(f: grammar::FnCall) -> Result<FnCall> {
    Ok(FnCall {
        fn_name: f.fn_name,
        args: f
            .args
            .into_iter()
            .map(resolve_expr)
            .collect::<Result<_>>()?,
    })
}

fn resolve_expr(expr: grammar::Expr) -> Result<Expr> {
    match expr {
        grammar::Expr::Pattern(p) => Ok(Expr::Pattern(resolve_pat(p)?)),
        grammar::Expr::Atom(name) => Ok(Expr::Atom(name)),
    }
}

fn resolve_pat(p: grammar::Pat) -> Result<Pat> {
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
