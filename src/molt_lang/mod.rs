mod context;
mod grammar;
mod interpreter;
mod resolve;

pub(crate) use context::Context;
pub(crate) use grammar::FileStructureError;
pub(crate) use interpreter::Error as InterpreterError;
pub(crate) use interpreter::Interpreter;

use codespan_reporting::files::Files;

use crate::Ctx;
use crate::Id;
use crate::molt_lang::resolve::Resolver;
use crate::rust_grammar::Ident;
use crate::rust_grammar::Kind;
use crate::rust_grammar::Node;
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
