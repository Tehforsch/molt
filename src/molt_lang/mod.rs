mod context;
mod grammar;
mod interpreter;
mod resolver;

pub(crate) use context::Context;
pub(crate) use grammar::FileStructureError;
pub(crate) use interpreter::Error as InterpreterError;
pub(crate) use interpreter::Interpreter;
pub(crate) use resolver::Error as ResolverError;

use codespan_reporting::files::Files;

use crate::Ctx;
use crate::Id;
use crate::molt_lang::resolver::Resolver;
use crate::rust_grammar::Ident;
use crate::rust_grammar::Kind;
use crate::rust_grammar::Node;
use crate::{Error, Input, Mode};

const MAIN_FN_NAME: &str = "main";
const INPUT_VAR_NAME: &str = "input";

#[derive(Debug)]
pub struct MoltFile {
    pub fns: Vec<MoltFn>,
    pub num_vars: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct VarId(usize);

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
    pub var_id: VarId,
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
    Var(VarId),
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
    Atom(Atom),
}

#[derive(Debug)]
pub enum Atom {
    Var(VarId),
    Lit(Lit),
}

#[derive(Debug)]
pub enum Lit {
    Str(String),
    Int(i64),
}

pub struct Pat {
    pub vars: Vec<TokenVar>,
    pub ctx: Ctx<Node>,
    pub node: Id,
}

impl Pat {
    fn get_var_id(&self, id: Id) -> VarId {
        // TODO this is O(n^2) given the surrounding context.
        self.vars
            .iter()
            .find(|var| var.ctx_id == id)
            .map(|var| var.var_id)
            .unwrap()
    }
}

impl std::fmt::Debug for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Pat").field("vars", &self.vars).finish()
    }
}

#[derive(Debug, Clone)]
pub struct TokenVar {
    pub ctx_id: Id,
    pub var_id: VarId,
}

impl MoltFile {
    pub fn new(input: &Input) -> Result<Self, Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let file: grammar::MoltFile =
            crate::parser::parse_str(source, Mode::Molt).map_err(|e| Error::parse(e, file_id))?;
        let file = file.add_implicit_main().map_err(Error::FileStructure)?;
        let mut resolver = Resolver::default();
        resolver
            .resolve_file(file)
            .map_err(|e| Error::Resolver(e, file_id))
    }
}
