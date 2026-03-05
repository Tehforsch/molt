mod context;
mod grammar;
mod interpreter;

use std::cell::RefCell;
use std::rc::Rc;

pub(crate) use context::Context;
pub(crate) use interpreter::Error as InterpreterError;
pub(crate) use interpreter::Interpreter;

use codespan_reporting::files::Files;

use crate::Ctx;
use crate::Id;
use crate::Var;
use crate::parser;
use crate::rust_grammar::Ident;
use crate::rust_grammar::Kind;
use crate::rust_grammar::Node;
use crate::rust_grammar::parse_node_with_kind;
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
    pub type_: Type,
}

#[derive(Debug)]
pub enum Type {
    Kind(Kind),
}

#[derive(Debug)]
pub enum Stmt {
    FnCall(FnCall),
    Let(LetStmt),
}

#[derive(Debug)]
pub struct LetStmt {
    pub lhs: LetLhs,
    pub _type_: Type,
    pub rhs: Option<Expr>,
}

#[derive(Debug)]
pub enum LetLhs {
    Var(Ident),
    Pat(Pat),
}

#[derive(Debug)]
pub struct FnCall {
    pub fn_name: Ident,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Atom(Ident),
}

pub struct Pat {
    pub vars: Vec<TokenVar>,
    pub ctx: Ctx<Node>,
    pub node: Id,
}

impl std::fmt::Debug for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Pat").field("vars", &self.vars).finish()
    }
}

#[derive(Debug)]
pub enum ResolveError {
    MainFnAndTopLevelStmtExist,
    InvalidInputVarName,
    NoInputVarName,
    PatternParse(parser::Error),
}

impl ResolveError {
    pub(crate) fn parse_error(&self) -> Option<&parser::Error> {
        match self {
            ResolveError::PatternParse(error) => Some(error),
            _ => None,
        }
    }
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
            ResolveError::PatternParse(_) => {
                write!(f, "Error while parsing pattern.")
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
            let grammar::LetLhs::Var(ref var_name) = l.lhs else {
                return Err(ResolveError::InvalidInputVarName);
            };
            if *var_name != INPUT_VAR_NAME {
                return Err(ResolveError::InvalidInputVarName);
            }
            fns.push(MoltFn {
                name: FnName::ImplicitMain,
                args: vec![FnArg {
                    var_name: var_name.clone(),
                    type_: resolve_type(l.type_)?,
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
        type_: resolve_type(arg.type_)?,
    })
}

fn resolve_type(arg: grammar::Type) -> Result<Type> {
    Ok(match arg {
        grammar::Type::Kind(ident) => Type::Kind(ident),
    })
}

fn resolve_stmt(stmt: grammar::Stmt) -> Result<Stmt> {
    match stmt {
        grammar::Stmt::FnCall(f) => Ok(Stmt::FnCall(resolve_fn_call(f)?)),
        grammar::Stmt::Let(l) => Ok(Stmt::Let(resolve_let_stmt(l)?)),
    }
}

fn resolve_let_stmt(l: grammar::LetStmt) -> Result<LetStmt> {
    let type_ = resolve_type(l.type_)?;
    Ok(LetStmt {
        lhs: resolve_let_lhs(l.lhs, &type_)?,
        _type_: type_,
        rhs: l.rhs.map(resolve_expr).transpose()?,
    })
}

fn resolve_let_lhs(lhs: grammar::LetLhs, type_: &Type) -> Result<LetLhs> {
    Ok(match lhs {
        grammar::LetLhs::Var(ident) => LetLhs::Var(ident),
        grammar::LetLhs::Pat(pat) => LetLhs::Pat(resolve_pat(pat, type_)?),
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
        grammar::Expr::Atom(name) => Ok(Expr::Atom(name)),
    }
}

fn resolve_pat(p: grammar::Pat, type_: &Type) -> Result<Pat> {
    // Add all vars to ctx with their kind.
    let mut pat_ctx = Ctx::<Node>::new(Mode::Molt);
    for var in p.vars.into_iter() {
        let kind = Kind::infer_from_name(&var.name.to_string())
            .unwrap_or_else(|| panic!("For now, names need to be called like their kind. Identifier {:?} does not follow this rule.", var.name.to_string()));
        pat_ctx.add_var::<Node>(Var::new(var.name, kind));
    }
    let ctx = Rc::new(RefCell::new(pat_ctx));
    // use the type annotation to tell us what to parse.
    let Type::Kind(kind) = type_;
    let node = crate::parser::parse_with_ctx(
        ctx.clone(),
        |stream| parse_node_with_kind(stream, *kind),
        p.tokens,
        Mode::Molt,
    )
    .map_err(ResolveError::PatternParse)?;
    let ctx = ctx.replace(Ctx::new(Mode::Molt));
    Ok(Pat {
        vars: ctx
            .iter_vars_ids()
            .map(|(id, var)| TokenVar {
                id,
                _kind: var.kind(),
                ident: var.ident().clone(),
            })
            .collect(),
        ctx,
        node,
    })
}

#[derive(Debug, Clone)]
pub struct TokenVar {
    pub id: Id,
    pub _kind: Kind,
    pub ident: Ident,
}

impl MoltFile {
    pub fn new(input: &Input) -> Result<Self, Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let file: grammar::MoltFile =
            crate::parser::parse_str(source, Mode::Molt).map_err(|e| Error::parse(e, file_id))?;
        resolve_file(file).map_err(|e| Error::Resolve2(e, file_id))
    }
}
