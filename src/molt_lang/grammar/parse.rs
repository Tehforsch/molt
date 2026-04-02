use proc_macro2::{Span, TokenStream};

use crate::molt_lang::grammar::{
    Assignment, AssignmentLhs, Atom, Block, ExprStmt, FnCall, List, Lit, PatVar, PatVars,
    ReturnStmt, Type,
};
use crate::molt_lang::{INPUT_VAR_NAME, MAIN_FN_NAME};
use crate::parser::parse::{self, Parse, ParseStream};
use crate::parser::punctuated::Punctuated;
use crate::parser::token::{Brace, Bracket};
use crate::parser::{self, Result, token};
use crate::rust_grammar::ext::IdentExt;
use crate::rust_grammar::{Ident, LitBool, LitInt, LitStr, NodeKind};
use crate::storage::Storage;

use super::{
    Expr, FieldAccess, FnArg, For, If, IfLet, LetLhs, LetStmt, MoltFile, MoltFn, Pat, Stmt,
};

impl Parse for MoltFile {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut fns = Storage::default();
        let mut stmts = vec![];
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![fn]) {
                fns.add(input.parse()?);
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
    NoInputVarName,
    UntypedInputVariable, // This is a little bit of an ugly error
}

impl std::fmt::Display for FileStructureError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileStructureError::MainFnAndTopLevelStmtExist => {
                write!(f, "Both a main function and top level statements exist")
            }
            FileStructureError::NoInputVarName => {
                write!(f, "No `input` variable declared.")
            }
            FileStructureError::UntypedInputVariable => {
                write!(
                    f,
                    "Could not determine type of `input`. Consider adding an annotation."
                )
            }
        }
    }
}

impl MoltFile {
    pub fn add_implicit_main(mut self) -> Result<Self, FileStructureError> {
        // Clearly very ugly, but we need to get started somehow.
        if !self.stmts.is_empty() {
            if self.fns.iter().any(|f| f.name == MAIN_FN_NAME) {
                return Err(FileStructureError::MainFnAndTopLevelStmtExist);
            }
            let mut args = Punctuated::new();
            if let Some(Stmt::Let(l)) = self.stmts.first()
                && let LetLhs::Var(ref var_name) = l.lhs
                && *var_name == INPUT_VAR_NAME
            {
                args.push(FnArg {
                    var_name: var_name.clone(),
                    type_: l
                        .type_
                        .clone()
                        .ok_or(FileStructureError::UntypedInputVariable)?,
                });
                self.stmts.remove(0);
            }
            self.fns.add(MoltFn {
                name: Ident::new(MAIN_FN_NAME, Span::call_site()),
                args,
                stmts: self.stmts.drain(..).collect(),
                return_type: Some(Type::Unit),
            });
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

        let return_type = if input.peek(Token![->]) {
            let _: Token![->] = input.parse()?;
            Some(input.parse()?)
        } else {
            None
        };
        let stmts = input.parse::<Block>()?;

        Ok(MoltFn {
            name,
            args,
            stmts: stmts.stmts.into_iter().collect(),
            return_type,
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
        let mut lh = input.lookahead1();
        if NodeKind::peek(&mut lh) {
            let kind = input.parse().unwrap();
            Ok(Type::Kind(kind))
        } else if lh.peek(Ident::peek_any) {
            let ident: Ident = input.parse()?;
            if ident == "bool" {
                Ok(Type::Bool)
            } else if ident == "int" {
                Ok(Type::Int)
            } else if ident == "str" {
                Ok(Type::Str)
            } else if ident == "List" {
                let _: Token![<] = input.parse()?;
                let ty = input.parse()?;
                let _: Token![>] = input.parse()?;
                Ok(Type::List(Box::new(ty)))
            } else {
                let e = lh.error();
                let m = e.messages().last().unwrap();
                Err(parser::Error::new(
                    ident.span(),
                    format!("{} or primitives `bool`, `int`, `str`, `List`", m),
                ))
            }
        } else {
            Err(lh.error())
        }
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![if]) && input.peek2(Token![let]) {
            Ok(Stmt::IfLet(input.parse()?))
        } else if lookahead.peek(Token![if]) {
            Ok(Stmt::If(input.parse()?))
        } else if lookahead.peek(Token![for]) {
            Ok(Stmt::For(input.parse()?))
        } else if lookahead.peek(Token![let]) {
            Ok(Stmt::Let(input.parse()?))
        } else if lookahead.peek(Token![return]) {
            Ok(Stmt::Return(input.parse()?))
        } else {
            let (span, expr) = input.parse_spanned::<Expr>()?.decompose();
            if input.peek(Token![=]) {
                let lhs = AssignmentLhs::from_expr(expr)
                    .map_err(|msg| parser::Error::new_molt(span, msg))?;
                let _: Token![=] = input.parse()?;
                let rhs = input.parse()?;
                let _: Token![;] = input.parse()?;
                Ok(Stmt::Assignment(Assignment { lhs, rhs }))
            } else {
                let has_trailing_semi = if input.peek(Token![;]) {
                    let _: Token![;] = input.parse()?;
                    true
                } else {
                    false
                };
                Ok(Stmt::Expr(ExprStmt {
                    expr,
                    has_trailing_semi,
                }))
            }
        }
    }
}

impl AssignmentLhs {
    fn from_expr(expr: Expr) -> Result<AssignmentLhs, String> {
        let make_error = || "Invalid left hand side of assignment".to_string();
        let get_ident = |atom| match atom {
            Atom::Lit(_) => Err(make_error()),
            Atom::List(_) => Err(make_error()),
            Atom::Var(ident) => Ok(ident),
        };
        match expr {
            Expr::FnCall(_) => Err(make_error()),
            Expr::Pat(_) => Err(make_error()),
            Expr::Atom(atom) => Ok(Self::Var(get_ident(atom)?)),
            Expr::FieldAccess(fa) => Ok(Self::FieldAccess {
                lhs: Box::new(AssignmentLhs::from_expr(*fa.lhs)?),
                field: fa.field,
            }),
        }
    }
}

impl Parse for Block {
    fn parse(input: ParseStream) -> Result<Block> {
        let body;
        braced!(body in input);
        let mut stmts = vec![];
        while !body.is_empty() {
            let stmt = body.parse()?;
            if !body.is_empty()
                && let Stmt::Expr(ref expr_stmt) = stmt
                && !expr_stmt.has_trailing_semi
            {
                return Err(body.error("expected semicolon"));
            }
            stmts.push(stmt);
        }
        Ok(stmts.into_iter().collect())
    }
}

impl Parse for If {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut if_branches = vec![];

        let _: Token![if] = input.parse()?;
        let condition: Expr = input.parse()?;
        let body = Block::parse(input)?;
        if_branches.push((condition, body));

        while input.peek(Token![else]) {
            let _: Token![else] = input.parse()?;
            if input.peek(Token![if]) {
                let _: Token![if] = input.parse()?;
                let condition: Expr = input.parse()?;
                let body = input.parse()?;
                if_branches.push((condition, body));
            } else {
                let body = input.parse()?;
                return Ok(If {
                    if_branches,
                    else_branch: Some(body),
                });
            }
        }

        Ok(If {
            if_branches,
            else_branch: None,
        })
    }
}

impl Parse for IfLet {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut if_branches = vec![];

        let _: Token![if] = input.parse()?;
        let _: Token![let] = input.parse()?;
        let lhs: LetLhs = input.parse()?;
        let _: Token![=] = input.parse()?;
        let expr: Expr = input.parse()?;
        let body = Block::parse(input)?;
        if_branches.push((lhs, expr, body));

        while input.peek(Token![else]) {
            let _: Token![else] = input.parse()?;
            if input.peek(Token![if]) {
                let _: Token![if] = input.parse()?;
                let _: Token![let] = input.parse()?;
                let lhs: LetLhs = input.parse()?;
                let _: Token![=] = input.parse()?;
                let expr: Expr = input.parse()?;
                let body = Block::parse(input)?;
                if_branches.push((lhs, expr, body));
            } else {
                let body = input.parse()?;
                return Ok(IfLet {
                    if_branches,
                    else_branch: Some(body),
                });
            }
        }

        Ok(IfLet {
            if_branches,
            else_branch: None,
        })
    }
}

impl Parse for For {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![for] = input.parse()?;
        let lhs: LetLhs = input.parse()?;
        let _: Token![in] = input.parse()?;
        let iterable: Expr = input.parse()?;
        let block = Block::parse(input)?;
        Ok(For {
            lhs,
            iterable,
            block,
        })
    }
}

impl Parse for ReturnStmt {
    fn parse(input: ParseStream) -> Result<Self> {
        let _: Token![return] = input.parse()?;
        let expr = Some(input.parse()?);
        let _: Token![;] = input.parse()?;
        Ok(ReturnStmt { expr })
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
        let mut expr = if lookahead.peek(LitInt) {
            Ok(Expr::Atom(Atom::Lit(Lit::Int(input.parse()?))))
        } else if lookahead.peek(LitStr) {
            Ok(Expr::Atom(Atom::Lit(Lit::Str(input.parse()?))))
        } else if LitBool::peek(input) {
            Ok(Expr::Atom(Atom::Lit(Lit::Bool(input.parse()?))))
        } else if input.peek(Bracket) {
            Ok(Expr::Atom(Atom::List(input.parse()?)))
        } else if input.peek(Brace) {
            Ok(Expr::Pat(input.parse()?))
        } else if input.peek(Ident::peek_any) {
            if input.peek2(token::Paren) {
                Ok(Expr::FnCall(input.parse()?))
            } else {
                Ok(Expr::Atom(Atom::Var(input.parse()?)))
            }
        } else {
            Err(lookahead.error())
        }?;
        while input.peek(Token![.]) {
            let _: Token![.] = input.parse()?;
            let field: Ident = input.parse()?;
            expr = Expr::FieldAccess(FieldAccess {
                lhs: Box::new(expr),
                field,
            });
        }
        Ok(expr)
    }
}

impl Parse for List {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        let items = Punctuated::parse_terminated_with(&content, Expr::parse)?;
        Ok(List { items })
    }
}

impl Parse for Pat {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        let (vars, tokens) = if lookahead.peek(token::Brace) {
            let content;
            braced!(content in input);
            let fork = content.fork();
            let vars = fork.parse::<PatVars>().unwrap().0;
            (vars, content.parse::<TokenStream>()?)
        } else {
            return Err(lookahead.error());
        };
        Ok(Pat { tokens, vars })
    }
}

impl Parse for PatVars {
    fn parse(input: ParseStream) -> parser::Result<Self> {
        let mut vars = PatVars(vec![]);
        loop {
            let marker = input.marker();
            if input.cursor().eof() {
                break;
            } else if input.parse::<Token![$]>().is_ok() {
                let name = input.parse()?;
                let span = input.span_from_marker(marker);
                vars.0.push(PatVar { name, span });
            } else if input.cursor().any_group().is_some() {
                let inner_vars: PatVars = input.step(|cursor| {
                    let (inner, _delim, _span, rest) = cursor.any_group().unwrap();
                    let scope = _span.close();
                    let ctx = cursor.ctx.clone();
                    let nested = parse::advance_step_cursor(cursor, inner);
                    let unexpected = parse::get_unexpected(input);
                    let content =
                        parse::new_parse_buffer(scope, nested, unexpected, ctx, input.mode());
                    let inner_vars: PatVars = content.parse().unwrap();
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
