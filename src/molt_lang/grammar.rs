mod parse;

use proc_macro2::TokenStream;

use crate::parser::punctuated::Punctuated;
use crate::parser::token::Comma;
use crate::rust_grammar::{Ident, Type};

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
pub enum Stmt {
    Assignment(Assignment),
    FnCall(FnCall),
    Let(LetStmt),
}

#[derive(Debug)]
pub struct LetStmt {
    pub lhs: Ident,
    pub type_: Type,
    pub rhs: Option<Expr>,
}

#[derive(Debug)]
pub struct Assignment {
    pub lhs: Ident,
    pub rhs: Expr,
}

#[derive(Debug)]
pub struct FnCall {
    pub fn_name: Ident,
    pub args: Punctuated<Expr, Comma>,
}

#[derive(Debug)]
pub enum Expr {
    Pattern(Pat),
    Atom(Ident), // Make this an actual atom if we ever get arithmetic
}

#[derive(Debug)]
pub struct Pat {
    pub pat: TokenStream,
}
