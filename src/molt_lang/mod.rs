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
    pub var_names: Vec<String>,
    pub builtin_map: HashMap<VarId, BuiltinFn>,
    pub main_fn_id: FnId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct VarId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct FnId(usize);

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
    Unit,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Let(LetStmt),
    Return(ReturnStmt),
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
    pub fn new(input: &Input, debug_print: bool) -> Result<Self, Error> {
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
        if debug_print {
            for (id, resolved_type) in typeck_result.iter_vars() {
                println!("{}: {}", resolved.var_names[id.0], resolved_type);
            }
        }
        Ok(resolved)
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

    fn iter_fns(&self) -> impl Iterator<Item = (FnId, &MoltFn)> {
        self.fns.iter().enumerate().map(|(i, f)| (FnId(i), f))
    }

    fn iter_builtins(&self) -> impl Iterator<Item = (VarId, BuiltinFn)> {
        self.builtin_map.iter().map(|(id, f)| (*id, *f))
    }
}
