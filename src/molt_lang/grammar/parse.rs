use proc_macro2::TokenStream;

use crate::molt_lang::grammar::{FnCall, Type};
use crate::parser::parse::{Parse, ParseStream};
use crate::parser::punctuated::Punctuated;
use crate::parser::{Result, token};
use crate::rust_grammar::{Ident, Kind};

use super::{Assignment, Expr, FnArg, LetLhs, LetStmt, MoltFile, MoltFn, Pat, Stmt};

impl Parse for MoltFile {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut fns = vec![];
        let mut stmts = vec![];
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![fn]) {
                fns.push(input.parse()?);
            } else {
                stmts.push(input.parse()?);
            }
        }
        Ok(MoltFile { fns, stmts })
    }
}

impl Parse for MoltFn {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![fn] = input.parse()?;
        let name: Ident = input.parse()?;

        let args_content;
        parenthesized!(args_content in input);
        let args = Punctuated::parse_terminated_with(&args_content, FnArg::parse)?;

        let body;
        braced!(body in input);
        let stmts = Punctuated::parse_terminated_with(&body, Stmt::parse)?;

        Ok(MoltFn { name, args, stmts })
    }
}

impl Parse for FnArg {
    fn parse(input: ParseStream) -> Result<Self> {
        let var_name: Ident = input.parse()?;
        let _: Token![:] = input.parse()?;
        let type_: Type = input.parse::<Type>()?;
        Ok(FnArg { var_name, type_ })
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        let kind: Kind = input.parse()?;
        Ok(Type::Kind(kind))
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![let]) {
            Ok(Stmt::Let(input.parse()?))
        } else if input.peek2(Token![=]) {
            Ok(Stmt::Assignment(input.parse()?))
        } else {
            Ok(Stmt::FnCall(input.parse()?))
        }
    }
}

impl Parse for LetStmt {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![let] = input.parse()?;
        let lhs: LetLhs = input.parse()?;
        let _: Token![:] = input.parse()?;
        let type_: Type = input.parse::<Type>()?;
        let rhs = if input.peek(Token![=]) {
            let _: Token![=] = input.parse()?;
            Some(input.parse()?)
        } else {
            None
        };
        let _: Token![;] = input.parse()?;
        Ok(LetStmt { lhs, type_, rhs })
    }
}

impl Parse for LetLhs {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(token::Brace) {
            let content;
            braced!(content in input);
            Ok(LetLhs::Pat(content.parse()?))
        } else {
            Ok(LetLhs::Var(input.parse()?))
        }
    }
}

impl Parse for Assignment {
    fn parse(input: ParseStream) -> Result<Self> {
        let lhs: Ident = input.parse()?;
        let _: Token![=] = input.parse()?;
        let rhs: Expr = input.parse()?;
        let _: Token![;] = input.parse()?;
        Ok(Assignment { lhs, rhs })
    }
}

impl Parse for FnCall {
    fn parse(input: ParseStream) -> Result<Self> {
        let fn_name: Ident = input.parse()?;

        let content;
        parenthesized!(content in input);
        let args = Punctuated::parse_terminated_with(&content, Expr::parse)?;

        let _: Token![;] = input.parse()?;
        Ok(FnCall { fn_name, args })
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(token::Brace) {
            let content;
            braced!(content in input);
            Ok(Expr::Pattern(content.parse()?))
        } else {
            Ok(Expr::Atom(input.parse()?))
        }
    }
}

impl Parse for Pat {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        let pat = if lookahead.peek(token::Brace) {
            let brace_content;
            braced!(brace_content in input);
            brace_content.parse::<TokenStream>()
        } else {
            Err(lookahead.error())
        }?;
        Ok(Pat { pat })
    }
}
