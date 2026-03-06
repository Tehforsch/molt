use std::cell::RefCell;
use std::rc::Rc;

use super::*;
use crate::Ctx;
use crate::Mode;
use crate::Var;
use crate::molt_lang::MoltFile;
use crate::molt_lang::MoltFn;
use crate::molt_lang::grammar;
use crate::parser;
use crate::rust_grammar::Kind;
use crate::rust_grammar::Node;
use crate::rust_grammar::parse_node_with_kind;

type Result<T, E = parser::Error> = std::result::Result<T, E>;

pub struct Resolver;

impl Resolver {
    pub fn resolve_file(&self, file: grammar::MoltFile) -> Result<MoltFile> {
        let grammar::MoltFile { fns, stmts: _ } = file;
        let fns: Vec<_> = fns
            .into_iter()
            .map(|f| self.resolve_fn(f))
            .collect::<Result<_>>()?;
        Ok(MoltFile { fns })
    }

    fn resolve_fn(&self, f: grammar::MoltFn) -> Result<MoltFn> {
        Ok(MoltFn {
            name: f.name,
            args: f
                .args
                .into_iter()
                .map(|a| self.resolve_fn_arg(a))
                .collect::<Result<_>>()?,
            stmts: f
                .stmts
                .into_iter()
                .map(|s| self.resolve_stmt(s))
                .collect::<Result<_>>()?,
        })
    }

    fn resolve_fn_arg(&self, arg: grammar::FnArg) -> Result<FnArg> {
        Ok(FnArg {
            var_name: arg.var_name,
            type_: self.resolve_type(arg.type_)?,
        })
    }

    fn resolve_type(&self, arg: grammar::Type) -> Result<Type> {
        Ok(match arg {
            grammar::Type::Kind(ident) => Type::Kind(ident),
        })
    }

    fn resolve_stmt(&self, stmt: grammar::Stmt) -> Result<Stmt> {
        match stmt {
            grammar::Stmt::ExprStmt(e) => Ok(Stmt::ExprStmt(self.resolve_expr(e)?)),
            grammar::Stmt::Let(l) => Ok(Stmt::Let(self.resolve_let_stmt(l)?)),
        }
    }

    fn resolve_let_stmt(&self, l: grammar::LetStmt) -> Result<LetStmt> {
        let type_ = self.resolve_type(l.type_)?;
        Ok(LetStmt {
            lhs: self.resolve_let_lhs(l.lhs, &type_)?,
            _type_: type_,
            rhs: l.rhs.map(|e| self.resolve_expr(e)).transpose()?,
        })
    }

    fn resolve_let_lhs(&self, lhs: grammar::LetLhs, type_: &Type) -> Result<LetLhs> {
        Ok(match lhs {
            grammar::LetLhs::Var(ident) => LetLhs::Var(ident),
            grammar::LetLhs::Pat(pat) => LetLhs::Pat(self.resolve_pat(pat, type_)?),
        })
    }

    fn resolve_fn_call(&self, f: grammar::FnCall) -> Result<FnCall> {
        Ok(FnCall {
            fn_name: f.fn_name,
            args: f
                .args
                .into_iter()
                .map(|e| self.resolve_expr(e))
                .collect::<Result<_>>()?,
        })
    }

    fn resolve_expr(&self, expr: grammar::Expr) -> Result<Expr> {
        match expr {
            grammar::Expr::FnCall(f) => Ok(Expr::FnCall(self.resolve_fn_call(f)?)),
            grammar::Expr::Atom(name) => Ok(Expr::Atom(name)),
        }
    }

    fn resolve_pat(&self, p: grammar::Pat, type_: &Type) -> Result<Pat> {
        let mut pat_ctx = Ctx::<Node>::new(Mode::Molt);
        for var in p.vars.into_iter() {
            let kind = Kind::infer_from_name(&var.name.to_string())
                .unwrap_or_else(|| panic!("For now, names need to be called like their kind. Identifier {:?} does not follow this rule.", var.name.to_string()));
            pat_ctx.add_var::<Node>(Var::new(var.name, kind));
        }
        let ctx = Rc::new(RefCell::new(pat_ctx));
        let Type::Kind(kind) = type_;
        let node = crate::parser::parse_with_ctx(
            ctx.clone(),
            |stream| parse_node_with_kind(stream, *kind),
            p.tokens,
            Mode::Molt,
        )?;
        let ctx = ctx.replace(Ctx::new(Mode::Molt));
        Ok(Pat {
            vars: ctx
                .iter_vars_ids()
                .map(|(id, var)| TokenVar {
                    id,
                    _kind: var.kind(),
                    ident: var.ident().clone(),
                })
                .collect(),
            ctx,
            node,
        })
    }
}
