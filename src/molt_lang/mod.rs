mod builtin_fn;
mod grammar;
mod index_types;
mod interpreter;
mod parse_pats;
mod resolver;
mod runtime_ctx;
mod typechecker;

use std::collections::HashMap;

pub(crate) use builtin_fn::BuiltinFn;
pub(crate) use grammar::FileStructureError;
pub(crate) use index_types::PatId;
pub(crate) use interpreter::Error as InterpreterError;
pub(crate) use interpreter::Interpreter;
use proc_macro2::TokenStream;
pub(crate) use resolver::Error as ResolverError;
pub(crate) use runtime_ctx::RuntimeCtx;
pub(crate) use typechecker::Error as TypeError;

use codespan_reporting::files::Files;

use crate::Ctx;
use crate::RawNodeId;
use crate::Span;
use crate::molt_lang::resolver::Resolver;
use crate::molt_lang::typechecker::Typechecker;
use crate::rust_grammar::Ident;
use crate::rust_grammar::Kind;
use crate::rust_grammar::Node;
use crate::storage::Storage;
use crate::{Error, Input, Mode};
use index_types::{FnId, VarId};

const MAIN_FN_NAME: &str = "main";
const INPUT_VAR_NAME: &str = "input";

/// Represents a molt file in which all variable names have
/// been resolved (i.e. assigned a unique id). In this
/// representation, the patterns have not been fully parsed.
#[derive(Debug)]
struct ResolvedMoltFile {
    pub fns: Storage<FnId, MoltFn>,
    pub var_names: Storage<VarId, Ident>,
    pub builtin_map: HashMap<VarId, BuiltinFn>,
    pub pats: Storage<PatId, UnparsedPat>,
}

/// Represents a molt file in which all variable names have
/// been resolved and all patterns have been fully parsed.
pub struct MoltFile {
    pub fns: Storage<FnId, MoltFn>,
    pub var_names: Storage<VarId, Ident>,
    pub builtin_map: HashMap<VarId, BuiltinFn>,
    pub pats: Storage<PatId, ParsedPat>,
}

impl MoltFile {
    fn main_fn_id(&self) -> FnId {
        self.fns.find_id(|f| f.name == MAIN_FN_NAME)
    }
}

#[derive(Debug)]
pub struct MoltFn {
    pub name: Ident,
    pub id: VarId,
    pub args: Vec<FnArg>,
    pub stmts: Vec<Stmt>,
    pub return_type: Type,
}

#[derive(Debug)]
pub struct FnArg {
    pub var_id: VarId,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Kind(Kind),
    Int,
    Bool,
    Str,
    List(Box<Type>),
    Unit,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Let(LetStmt),
    Return(ReturnStmt),
    Assignment(Assignment),
    If(If),
    For(For),
}

#[derive(Debug)]
pub struct If {
    pub if_branches: Vec<(Expr, Vec<Stmt>)>,
    pub else_branch: Option<Vec<Stmt>>,
}

#[derive(Debug)]
pub struct For {
    pub lhs: LetLhs,
    pub iterable: Expr,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Assignment {
    pub lhs: VarId,
    pub rhs: Expr,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct LetStmt {
    pub lhs: LetLhs,
    pub type_: Option<Type>,
    pub rhs: Option<Expr>,
}

#[derive(Debug)]
pub enum LetLhs {
    Var(VarId),
    Pat(PatId),
}

#[derive(Debug)]
pub struct FnCall {
    pub id: VarId,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    FnCall(FnCall),
    Atom(Atom),
    Pat(PatId),
}

#[derive(Debug)]
pub enum Atom {
    Var(VarId),
    Lit(Lit),
    List(List),
}

#[derive(Debug)]
pub struct List {
    items: Vec<Expr>,
}

#[derive(Debug)]
pub enum Lit {
    Str(String),
    Int(i64),
    Bool(bool),
}

impl Lit {
    fn type_(&self) -> typechecker::Type {
        match self {
            Lit::Str(_) => typechecker::Type::Str,
            Lit::Int(_) => typechecker::Type::Int,
            Lit::Bool(_) => typechecker::Type::Bool,
        }
    }
}

#[derive(Debug)]
pub struct UnparsedPat {
    pub vars: Vec<(VarId, Span)>,
    pub tokens: TokenStream,
}

pub struct ParsedPat {
    pub vars: Vec<PatVar>,
    pub ctx: Ctx<Node>,
    pub node: RawNodeId,
}

impl ParsedPat {
    fn get_var_id(&self, id: RawNodeId) -> VarId {
        // TODO this is O(n^2) given the surrounding context.
        self.vars
            .iter()
            .find(|var| var.ctx_id == id)
            .map(|var| var.var_id)
            .unwrap()
    }
}

impl std::fmt::Debug for ParsedPat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Pat").field("vars", &self.vars).finish()
    }
}

#[derive(Debug, Clone)]
pub struct PatVar {
    pub ctx_id: RawNodeId,
    pub var_id: VarId,
    pub span: Span,
}

impl MoltFile {
    pub fn new(input: &Input) -> Result<Self, Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let file: grammar::MoltFile = crate::parser::parse_str(source, Mode::Molt)
            .map_err(|e| Error::parse(e, file_id))?
            .item;
        let file = file.add_implicit_main().map_err(Error::FileStructure)?;
        let resolved = Resolver::resolve(file).map_err(|e| Error::Resolver(e, file_id))?;
        let typeck = Typechecker::check(&resolved).map_err(|e| Error::Typechecker(e, file_id))?;
        resolved
            .parse_pats(&typeck)
            .map_err(|e| Error::ParsePats(e, file_id))
    }

    pub(crate) fn check_has_main_fn_with_input(&self) -> Result<(), Error> {
        if self
            .fns
            .iter()
            .any(|f| f.name == MAIN_FN_NAME && !f.args.is_empty())
        {
            Ok(())
        } else {
            Err(Error::FileStructure(FileStructureError::NoInputVarName))
        }
    }

    fn iter_builtins(&self) -> impl Iterator<Item = (VarId, BuiltinFn)> {
        self.builtin_map.iter().map(|(id, f)| (*id, *f))
    }
}
