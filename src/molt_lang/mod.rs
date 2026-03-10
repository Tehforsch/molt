mod builtin_fn;
mod context;
mod grammar;
mod interpreter;
mod resolver;
mod typechecker;

use std::collections::HashMap;

pub(crate) use context::Context;
pub(crate) use grammar::FileStructureError;
pub(crate) use interpreter::Error as InterpreterError;
pub(crate) use interpreter::Interpreter;
pub(crate) use resolver::Error as ResolverError;
pub(crate) use typechecker::Error as TypeError;

pub(crate) use builtin_fn::BuiltinFn;

use codespan_reporting::files::Files;

use crate::Ctx;
use crate::Id;
use crate::molt_lang::resolver::Resolver;
use crate::molt_lang::typechecker::Typechecker;
use crate::rust_grammar::Ident;
use crate::rust_grammar::Kind;
use crate::rust_grammar::Node;
use crate::{Error, Input, Mode};

const MAIN_FN_NAME: &str = "main";
const INPUT_VAR_NAME: &str = "input";

#[derive(Debug)]
pub struct MoltFile {
    pub fns: Vec<MoltFn>,
    pub builtin_map: HashMap<VarId, BuiltinFn>,
    pub num_vars: usize,
    pub main_fn_id: FnId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct VarId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct FnId(usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VarName {
    Ident(Ident),
    /// For "artificially" created name, such as builtin functions
    /// or the implicit main function.
    String(String),
}

impl VarName {
    fn is_main(&self) -> bool {
        match self {
            VarName::Ident(ident) => ident == "main",
            VarName::String(s) => s == "main",
        }
    }
}

impl std::fmt::Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarName::Ident(ident) => write!(f, "{}", ident),
            VarName::String(s) => write!(f, "{}", s),
        }
    }
}

impl From<&str> for VarName {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}

impl From<&Ident> for VarName {
    fn from(value: &Ident) -> Self {
        Self::Ident(value.clone())
    }
}

#[derive(Debug)]
pub struct MoltFn {
    pub name: VarName,
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
    Unit,
}

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    Let(LetStmt),
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
    Pat(Pat),
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
        let resolver = Resolver::default();
        let resolved = resolver
            .resolve_file(file)
            .map_err(|e| Error::Resolver(e, file_id))?;
        let typechecker = Typechecker::default();
        let typeck_result = typechecker
            .check(&resolved)
            .map_err(|e| Error::Typechecker(e, file_id))?;
        dbg!(&typeck_result);
        Ok(resolved)
    }

    pub(crate) fn check_has_main_fn_with_input(&self) -> Result<(), Error> {
        if self
            .fns
            .iter()
            .any(|f| f.name.is_main() && !f.args.is_empty())
        {
            Ok(())
        } else {
            Err(Error::FileStructure(FileStructureError::NoInputVarName))
        }
    }

    fn iter_fns(&self) -> impl Iterator<Item = (FnId, &MoltFn)> {
        self.fns.iter().enumerate().map(|(i, f)| (FnId(i), f))
    }

    fn iter_builtins(&self) -> impl Iterator<Item = (VarId, BuiltinFn)> {
        self.builtin_map.iter().map(|(id, f)| (*id, *f))
    }
}
