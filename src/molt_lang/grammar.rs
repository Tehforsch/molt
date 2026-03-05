mod parse;

use proc_macro2::TokenStream;

use crate::parser::punctuated::Punctuated;
use crate::parser::token::Comma;
use crate::rust_grammar::{Ident, Kind};

#[derive(Debug)]
pub(crate) struct MoltFile {
    pub fns: Vec<MoltFn>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct MoltFn {
    pub name: Ident,
    pub args: Punctuated<FnArg, Comma>,
    pub stmts: Punctuated<Stmt, Token![;]>,
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
    pub type_: Type,
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
    Atom(Ident),
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
