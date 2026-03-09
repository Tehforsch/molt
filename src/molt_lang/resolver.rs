use std::cell::RefCell;
use std::collections::HashMap;
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

#[derive(Debug)]
pub(crate) enum Error {
    Parse(parser::Error),
    UndefinedVar(Ident),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse(error) => write!(f, "{:?}", error),
            Error::UndefinedVar(ident) => write!(f, "Undefined variable: '{:?}'", ident),
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

type ScopeIndex = usize;

#[derive(Default)]
struct Scope {
    parent: Option<ScopeIndex>,
    variables: HashMap<Ident, VarId>,
    index: ScopeIndex,
}

impl Scope {
    fn child_of(active_scope: &Scope, index: usize) -> Scope {
        Self {
            parent: Some(active_scope.index),
            variables: HashMap::default(),
            index,
        }
    }

    fn insert(&mut self, var_name: Ident, var_id: VarId) {
        self.variables.insert(var_name, var_id);
    }
}

pub struct Resolver {
    scopes: Vec<Scope>,
    num_vars: usize,
}

impl Default for Resolver {
    fn default() -> Self {
        Self {
            num_vars: 0,
            scopes: vec![Scope::default()],
        }
    }
}

impl Resolver {
    fn active_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn active_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn next_scope_index(&self) -> usize {
        self.scopes.len()
    }

    fn fresh_var_id(&mut self) -> VarId {
        self.num_vars += 1;
        VarId(self.num_vars - 1)
    }

    fn register_var(&mut self, name: &Ident) -> VarId {
        let var_id = self.fresh_var_id();
        self.active_scope_mut().insert(name.clone(), var_id);
        var_id
    }

    fn lookup_var(&self, name: &Ident) -> Result<VarId> {
        let mut scope_idx = Some(self.scopes.len() - 1);
        while let Some(idx) = scope_idx {
            let scope = &self.scopes[idx];
            if let Some(val) = scope.variables.get(name) {
                return Ok(*val);
            }
            scope_idx = scope.parent;
        }
        Err(Error::UndefinedVar(name.clone()))
    }

    pub fn resolve_file(&mut self, file: grammar::MoltFile) -> Result<MoltFile> {
        let grammar::MoltFile { fns, stmts: _ } = file;
        let fns: Vec<_> = fns
            .into_iter()
            .map(|f| self.resolve_fn(f))
            .collect::<Result<_>>()?;
        Ok(MoltFile {
            fns,
            num_vars: self.num_vars,
        })
    }

    fn resolve_fn(&mut self, f: grammar::MoltFn) -> Result<MoltFn> {
        let scope = Scope::child_of(self.active_scope(), self.next_scope_index());
        self.scopes.push(scope);
        let args = f
            .args
            .into_iter()
            .map(|a| self.resolve_fn_arg(a))
            .collect::<Result<_>>()?;
        let result = MoltFn {
            name: f.name,
            args,
            stmts: f
                .stmts
                .into_iter()
                .map(|s| self.resolve_stmt(s))
                .collect::<Result<_>>()?,
        };
        self.scopes.pop();
        Ok(result)
    }

    fn resolve_fn_arg(&mut self, arg: grammar::FnArg) -> Result<FnArg> {
        let var_id = self.register_var(&arg.var_name);
        Ok(FnArg {
            var_id,
            type_: self.resolve_type(arg.type_)?,
        })
    }

    fn resolve_type(&mut self, arg: grammar::Type) -> Result<Type> {
        Ok(match arg {
            grammar::Type::Kind(ident) => Type::Kind(ident),
        })
    }

    fn resolve_stmt(&mut self, stmt: grammar::Stmt) -> Result<Stmt> {
        match stmt {
            grammar::Stmt::ExprStmt(e) => Ok(Stmt::ExprStmt(self.resolve_expr(e)?)),
            grammar::Stmt::Let(l) => Ok(Stmt::Let(self.resolve_let_stmt(l)?)),
        }
    }

    fn resolve_let_stmt(&mut self, l: grammar::LetStmt) -> Result<LetStmt> {
        let type_ = self.resolve_type(l.type_)?;
        Ok(LetStmt {
            lhs: self.resolve_let_lhs(l.lhs, &type_)?,
            _type_: type_,
            rhs: l.rhs.map(|e| self.resolve_expr(e)).transpose()?,
        })
    }

    fn resolve_let_lhs(&mut self, lhs: grammar::LetLhs, type_: &Type) -> Result<LetLhs> {
        Ok(match lhs {
            grammar::LetLhs::Var(ident) => {
                let var_id = self.register_var(&ident);
                LetLhs::Var(var_id)
            }
            grammar::LetLhs::Pat(pat) => LetLhs::Pat(self.resolve_pat(pat, type_)?),
        })
    }

    fn resolve_fn_call(&mut self, f: grammar::FnCall) -> Result<FnCall> {
        Ok(FnCall {
            fn_name: f.fn_name,
            args: f
                .args
                .into_iter()
                .map(|e| self.resolve_expr(e))
                .collect::<Result<_>>()?,
        })
    }

    fn resolve_expr(&mut self, expr: grammar::Expr) -> Result<Expr> {
        match expr {
            grammar::Expr::FnCall(f) => Ok(Expr::FnCall(self.resolve_fn_call(f)?)),
            grammar::Expr::Atom(atom) => Ok(Expr::Atom(self.resolve_atom(&atom)?)),
        }
    }

    fn resolve_atom(&self, atom: &grammar::Atom) -> Result<Atom> {
        match atom {
            grammar::Atom::Lit(lit) => Ok(Atom::Lit(self.resolve_lit(lit)?)),
            grammar::Atom::Var(ident) => Ok(Atom::Var(self.lookup_var(ident)?)),
        }
    }

    fn resolve_lit(&self, lit: &grammar::Lit) -> Result<Lit> {
        match lit {
            grammar::Lit::Int(lit_int) => {
                let val = lit_int
                    .base10_digits()
                    .parse()
                    .map_err(|err| Error::Parse(parser::Error::new(lit_int.span(), err)))?;
                Ok(Lit::Int(val))
            }
            grammar::Lit::Str(lit_str) => Ok(Lit::Str(lit_str.value())),
        }
    }

    fn resolve_pat(&mut self, p: grammar::Pat, type_: &Type) -> Result<Pat> {
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
        )
        .map_err(Error::Parse)?;
        let ctx = ctx.replace(Ctx::new(Mode::Molt));
        Ok(Pat {
            vars: ctx
                .iter_vars_ids()
                .map(|(id, var)| {
                    let var_id = self.register_var(var.ident());
                    TokenVar { ctx_id: id, var_id }
                })
                .collect(),
            ctx,
            node,
        })
    }
}
