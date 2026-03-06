mod context;
mod grammar;
mod interpreter;

use std::cell::RefCell;
use std::rc::Rc;

pub(crate) use context::Context;
pub(crate) use grammar::FileStructureError;
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
    ExprStmt(Expr),
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
    FnCall(FnCall),
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

type Result<T, E = parser::Error> = std::result::Result<T, E>;

struct Resolver;

impl Resolver {
    fn resolve_file(&self, file: grammar::MoltFile) -> Result<MoltFile> {
        let grammar::MoltFile { fns, stmts: _ } = file;
        let fns: Vec<_> = fns
            .into_iter()
            .map(|f| self.resolve_fn(f))
            .collect::<Result<_>>()?;
        Ok(MoltFile { fns })
    }

    fn resolve_fn(&self, f: grammar::MoltFn) -> Result<MoltFn> {
        Ok(MoltFn {
            name: f.name,
            args: f
                .args
                .into_iter()
                .map(|a| self.resolve_fn_arg(a))
                .collect::<Result<_>>()?,
            stmts: f
                .stmts
                .into_iter()
                .map(|s| self.resolve_stmt(s))
                .collect::<Result<_>>()?,
        })
    }

    fn resolve_fn_arg(&self, arg: grammar::FnArg) -> Result<FnArg> {
        Ok(FnArg {
            var_name: arg.var_name,
            type_: self.resolve_type(arg.type_)?,
        })
    }

    fn resolve_type(&self, arg: grammar::Type) -> Result<Type> {
        Ok(match arg {
            grammar::Type::Kind(ident) => Type::Kind(ident),
        })
    }

    fn resolve_stmt(&self, stmt: grammar::Stmt) -> Result<Stmt> {
        match stmt {
            grammar::Stmt::ExprStmt(e) => Ok(Stmt::ExprStmt(self.resolve_expr(e)?)),
            grammar::Stmt::Let(l) => Ok(Stmt::Let(self.resolve_let_stmt(l)?)),
        }
    }

    fn resolve_let_stmt(&self, l: grammar::LetStmt) -> Result<LetStmt> {
        let type_ = self.resolve_type(l.type_)?;
        Ok(LetStmt {
            lhs: self.resolve_let_lhs(l.lhs, &type_)?,
            _type_: type_,
            rhs: l.rhs.map(|e| self.resolve_expr(e)).transpose()?,
        })
    }

    fn resolve_let_lhs(&self, lhs: grammar::LetLhs, type_: &Type) -> Result<LetLhs> {
        Ok(match lhs {
            grammar::LetLhs::Var(ident) => LetLhs::Var(ident),
            grammar::LetLhs::Pat(pat) => LetLhs::Pat(self.resolve_pat(pat, type_)?),
        })
    }

    fn resolve_fn_call(&self, f: grammar::FnCall) -> Result<FnCall> {
        Ok(FnCall {
            fn_name: f.fn_name,
            args: f
                .args
                .into_iter()
                .map(|e| self.resolve_expr(e))
                .collect::<Result<_>>()?,
        })
    }

    fn resolve_expr(&self, expr: grammar::Expr) -> Result<Expr> {
        match expr {
            grammar::Expr::FnCall(f) => Ok(Expr::FnCall(self.resolve_fn_call(f)?)),
            grammar::Expr::Atom(name) => Ok(Expr::Atom(name)),
        }
    }

    fn resolve_pat(&self, p: grammar::Pat, type_: &Type) -> Result<Pat> {
        let mut pat_ctx = Ctx::<Node>::new(Mode::Molt);
        for var in p.vars.into_iter() {
            let kind = Kind::infer_from_name(&var.name.to_string())
                .unwrap_or_else(|| panic!("For now, names need to be called like their kind. Identifier {:?} does not follow this rule.", var.name.to_string()));
            pat_ctx.add_var::<Node>(Var::new(var.name, kind));
        }
        let ctx = Rc::new(RefCell::new(pat_ctx));
        let Type::Kind(kind) = type_;
        let node = crate::parser::parse_with_ctx(
            ctx.clone(),
            |stream| parse_node_with_kind(stream, *kind),
            p.tokens,
            Mode::Molt,
        )?;
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
        let file = file.add_implicit_main().map_err(Error::FileStructure)?;
        Resolver
            .resolve_file(file)
            .map_err(|e| Error::Parse(e, file_id))
    }
}
