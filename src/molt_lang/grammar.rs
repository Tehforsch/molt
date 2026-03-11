mod parse;

use proc_macro2::TokenStream;

use crate::parser::punctuated::Punctuated;
use crate::parser::token::Comma;
use crate::rust_grammar::{Ident, Kind, LitBool, LitInt, LitStr};
pub use parse::FileStructureError;

#[derive(Debug)]
pub(crate) struct MoltFile {
    pub fns: Vec<MoltFn>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct MoltFn {
    pub name: Ident,
    pub args: Punctuated<FnArg, Comma>,
    pub stmts: Vec<Stmt>,
    pub return_type: Option<Type>,
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
}

#[derive(Debug)]
pub enum Atom {
    Lit(Lit),
    Var(Ident),
}

#[derive(Debug)]
pub enum Lit {
    Int(LitInt),
    Str(LitStr),
    Bool(LitBool),
}

#[derive(Debug)]
pub struct Pat {
    pub tokens: TokenStream,
    pub vars: Vec<TokenVar>,
}

#[derive(Debug, Clone)]
pub struct TokenVar {
    pub name: Ident,
}

pub struct TokenVars(pub Vec<TokenVar>);
