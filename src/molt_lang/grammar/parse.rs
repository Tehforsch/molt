use proc_macro2::TokenStream;

use crate::molt_lang::grammar::{Atom, FnCall, Lit, TokenVar, TokenVars, Type};
use crate::molt_lang::{FnName, INPUT_VAR_NAME, MAIN_FN_NAME};
use crate::parser::parse::{self, Parse, ParseStream};
use crate::parser::punctuated::Punctuated;
use crate::parser::{Result, token};
use crate::rust_grammar::ext::IdentExt;
use crate::rust_grammar::{Ident, Kind, LitBool, LitInt, LitStr};

use super::{Expr, FnArg, LetLhs, LetStmt, MoltFile, MoltFn, Pat, Stmt};

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

#[derive(Debug)]
pub enum FileStructureError {
    MainFnAndTopLevelStmtExist,
    InvalidInputVarName,
    NoInputVarName,
}

impl std::fmt::Display for FileStructureError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileStructureError::MainFnAndTopLevelStmtExist => {
                write!(f, "Both a main function and top level statements exist")
            }
            FileStructureError::InvalidInputVarName => {
                write!(f, "The first variable needs to have name `input`.")
            }
            FileStructureError::NoInputVarName => {
                write!(f, "No `input` variable declared.")
            }
        }
    }
}

impl MoltFile {
    pub fn add_implicit_main(mut self) -> Result<Self, FileStructureError> {
        // Clearly very ugly, but we need to get started somehow.
        if !self.stmts.is_empty() {
            if self.fns.iter().any(|f| MAIN_FN_NAME == f.name.to_string()) {
                return Err(FileStructureError::MainFnAndTopLevelStmtExist);
            }
            if let Stmt::Let(l) = self.stmts.remove(0) {
                let LetLhs::Var(ref var_name) = l.lhs else {
                    return Err(FileStructureError::InvalidInputVarName);
                };
                if *var_name != INPUT_VAR_NAME {
                    return Err(FileStructureError::InvalidInputVarName);
                }
                let mut args = Punctuated::new();
                args.push(FnArg {
                    var_name: var_name.clone(),
                    type_: l.type_.unwrap(), // TODO error handling
                });
                self.fns.push(MoltFn {
                    name: FnName::ImplicitMain,
                    args,
                    stmts: self.stmts.drain(..).collect(),
                });
            } else {
                return Err(FileStructureError::NoInputVarName);
            }
        }
        Ok(self)
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
        let mut stmts = vec![];
        while !body.is_empty() {
            stmts.push(body.parse()?);
        }

        Ok(MoltFn {
            name: FnName::Ident(name),
            args,
            stmts,
        })
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
        } else {
            let expr: Expr = input.parse()?;
            let _: Token![;] = input.parse()?;
            Ok(Stmt::ExprStmt(expr))
        }
    }
}

impl Parse for LetStmt {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![let] = input.parse()?;
        let lhs: LetLhs = input.parse()?;
        let type_: Option<Type> = if input.peek(Token![:]) {
            let _: Token![:] = input.parse()?;
            Some(input.parse::<Type>()?)
        } else {
            None
        };
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
            Ok(LetLhs::Pat(input.parse()?))
        } else {
            Ok(LetLhs::Var(input.parse()?))
        }
    }
}

impl Parse for FnCall {
    fn parse(input: ParseStream) -> Result<Self> {
        let fn_name: Ident = input.parse()?;

        let content;
        parenthesized!(content in input);
        let args = Punctuated::parse_terminated_with(&content, Expr::parse)?;

        Ok(FnCall { fn_name, args })
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitInt) {
            Ok(Expr::Atom(Atom::Lit(Lit::Int(input.parse()?))))
        } else if lookahead.peek(LitStr) {
            Ok(Expr::Atom(Atom::Lit(Lit::Str(input.parse()?))))
        } else if LitBool::peek(input) {
            Ok(Expr::Atom(Atom::Lit(Lit::Bool(input.parse()?))))
        } else if input.peek(Ident::peek_any) {
            if input.peek2(token::Paren) {
                Ok(Expr::FnCall(input.parse()?))
            } else {
                Ok(Expr::Atom(Atom::Var(input.parse()?)))
            }
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for Pat {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        let (vars, tokens) = if lookahead.peek(token::Brace) {
            let content;
            braced!(content in input);
            let fork = content.fork();
            let vars = fork.parse::<TokenVars>().unwrap().0;
            (vars, content.parse::<TokenStream>()?)
        } else {
            return Err(lookahead.error());
        };
        Ok(Pat { tokens, vars })
    }
}

impl Parse for TokenVars {
    fn parse(input: ParseStream) -> crate::parser::Result<Self> {
        let mut vars = TokenVars(vec![]);
        loop {
            if input.cursor().eof() {
                break;
            } else if input.parse::<Token![$]>().is_ok() {
                let name = input.parse()?;
                vars.0.push(TokenVar { name });
            } else if input.cursor().any_group().is_some() {
                let inner_vars: TokenVars = input.step(|cursor| {
                    let (inner, _delim, _span, rest) = cursor.any_group().unwrap();
                    let scope = _span.close();
                    let ctx = cursor.ctx.clone();
                    let nested = parse::advance_step_cursor(cursor, inner);
                    let unexpected = parse::get_unexpected(input);
                    let content =
                        parse::new_parse_buffer(scope, nested, unexpected, ctx, input.mode());
                    let inner_vars: TokenVars = content.parse().unwrap();
                    Ok((inner_vars, rest))
                })?;
                vars.0.extend(inner_vars.0);
            } else {
                input
                    .step(|cursor| Ok(((), cursor.skip().unwrap())))
                    .unwrap();
            }
        }
        Ok(vars)
    }
}
