mod parse;

use proc_macro2::TokenStream;

use crate::Span;
use crate::molt_lang::index_types::FnId;
use crate::parser::punctuated::Punctuated;
use crate::parser::token::Comma;
use crate::rust_grammar::{Ident, Kind, LitBool, LitInt, LitStr};
use crate::storage::Storage;
pub use parse::FileStructureError;

#[derive(Debug)]
pub(crate) struct MoltFile {
    pub fns: Storage<FnId, MoltFn>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct MoltFn {
    pub name: Ident,
    pub args: Punctuated<FnArg, Comma>,
    pub stmts: Block,
    pub return_type: Option<Type>,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl FromIterator<Stmt> for Block {
    fn from_iter<T: IntoIterator<Item = Stmt>>(iter: T) -> Self {
        Self {
            stmts: iter.into_iter().collect(),
        }
    }
}

#[derive(Debug)]
pub struct FnArg {
    pub var_name: Ident,
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
    Expr(ExprStmt),
    Let(LetStmt),
    Return(ReturnStmt),
    Assignment(Assignment),
    If(If),
    IfLet(IfLet),
    For(For),
}

#[derive(Debug)]
pub struct If {
    pub if_branches: Vec<(Expr, Block)>,
    pub else_branch: Option<Block>,
}

#[derive(Debug)]
pub struct IfLet {
    pub if_branches: Vec<(LetLhs, Expr, Block)>,
    pub else_branch: Option<Block>,
}

#[derive(Debug)]
pub struct For {
    pub lhs: LetLhs,
    pub iterable: Expr,
    pub block: Block,
}

#[derive(Debug)]
pub struct Assignment {
    pub lhs: Ident,
    pub rhs: Expr,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
    pub has_trailing_semi: bool,
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
    Var(Ident),
    Pat(Pat),
}

#[derive(Debug)]
pub struct FnCall {
    pub fn_name: Ident,
    pub args: Punctuated<Expr, Comma>,
}

#[derive(Debug)]
pub enum Expr {
    FnCall(FnCall),
    Atom(Atom),
    Pat(Pat),
}

#[derive(Debug)]
pub enum Atom {
    Lit(Lit),
    Var(Ident),
    List(List),
}

#[derive(Debug)]
pub enum Lit {
    Int(LitInt),
    Str(LitStr),
    Bool(LitBool),
}

#[derive(Debug)]
pub struct List {
    pub items: Punctuated<Expr, Comma>,
}

#[derive(Debug)]
pub struct Pat {
    pub tokens: TokenStream,
    pub vars: Vec<PatVar>,
}

#[derive(Debug, Clone)]
pub struct PatVar {
    pub name: Ident,
    pub span: Span,
}

pub struct PatVars(pub Vec<PatVar>);
