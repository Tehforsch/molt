use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use super::*;
use crate::Ctx;
use crate::Mode;
use crate::Var;
use crate::molt_lang::MoltFile;
use crate::molt_lang::MoltFn;
use crate::molt_lang::builtin_fn::BuiltinFn;
use crate::molt_lang::builtin_fn::builtins_def;
use crate::molt_lang::grammar;
use crate::molt_lang::typechecker::TypecheckResult;
use crate::parser;
use crate::rust_grammar::Node;
use crate::rust_grammar::parse_node_with_kind;
use crate::storage::Storage;

#[derive(Debug)]
pub(crate) enum Error {
    Parse(parser::Error),
    UndefinedVar(Ident),
    DuplicateDefinitionFn(Ident),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO format these
        match self {
            Error::Parse(error) => write!(f, "{:?}", error),
            Error::UndefinedVar(ident) => write!(f, "Undefined variable: '{:?}'", ident),
            Error::DuplicateDefinitionFn(ident) => {
                write!(f, "Function defined twice: '{:?}'", ident)
            }
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

type ScopeIndex = usize;

#[derive(Default, Debug)]
struct Scope {
    parent: Option<ScopeIndex>,
    variables: HashMap<String, VarId>,
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

    fn insert(&mut self, var_name: &str, var_id: VarId) {
        self.variables.insert(var_name.into(), var_id);
    }
}

pub struct Resolver {
    scopes: Vec<Scope>,
    var_names: Vec<Ident>,
    fn_map: HashMap<VarId, FnId>,
    builtin_map: HashMap<VarId, BuiltinFn>,
    pats: Storage<PatId, UnresolvedPat>,
}

impl Default for Resolver {
    fn default() -> Self {
        Self {
            var_names: vec![],
            scopes: vec![Scope::default()],
            fn_map: HashMap::default(),
            builtin_map: HashMap::default(),
            pats: Storage::default(),
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

    fn register_var(&mut self, name: &Ident) -> VarId {
        let name_str = name.to_string();
        self.var_names.push(name.clone());
        let var_id = VarId(self.var_names.len() - 1);
        self.active_scope_mut().insert(&name_str, var_id);
        var_id
    }

    fn lookup_var(&self, name: &Ident) -> Result<VarId> {
        let mut scope_idx = Some(self.scopes.len() - 1);
        while let Some(idx) = scope_idx {
            let scope = &self.scopes[idx];
            if let Some(val) = scope.variables.get(&name.to_string()) {
                return Ok(*val);
            }
            scope_idx = scope.parent;
        }
        Err(Error::UndefinedVar(name.clone()))
    }

    fn lookup_or_register_var(&mut self, name: &Ident) -> Result<VarId> {
        Ok(self
            .lookup_var(name) // TODO: the implicit clone in the error variant here is ugly
            .unwrap_or_else(|_| self.register_var(name)))
    }

    pub fn resolve_file(
        mut self,
        file: grammar::MoltFile,
    ) -> Result<(MoltFile, Storage<PatId, UnresolvedPat>)> {
        let grammar::MoltFile { fns, stmts: _ } = file;
        // Register all user defined functions
        self.register_builtins();
        self.register_user_fns(&fns);
        let fns: Storage<_, _> = fns
            .into_iter()
            .map(|f| self.resolve_fn(f))
            .collect::<Result<_>>()?;
        check_names_unique(&fns)?;
        Ok((
            MoltFile {
                main_fn_id: fns.find_id(|f| f.name == MAIN_FN_NAME),
                var_names: self.var_names,
                fns,
                builtin_map: self.builtin_map,
            },
            self.pats,
        ))
    }

    fn register_builtins(&mut self) {
        for (name, f) in builtins_def() {
            let var = self.register_var(&name);
            self.builtin_map.insert(var, f);
        }
    }

    fn register_user_fns(&mut self, fns: &[grammar::MoltFn]) {
        for (i, f) in fns.iter().enumerate() {
            let var = self.register_var(&f.name);
            self.fn_map.insert(var, FnId(i));
        }
    }

    fn resolve_fn(&mut self, f: grammar::MoltFn) -> Result<MoltFn> {
        let scope = Scope::child_of(self.active_scope(), self.next_scope_index());
        self.scopes.push(scope);
        let args = f
            .args
            .into_iter()
            .map(|a| self.resolve_fn_arg(a))
            .collect::<Result<_>>()?;
        let num_stmts = f.stmts.len();
        let result = MoltFn {
            id: self.lookup_var(&f.name).unwrap(),
            name: f.name,
            args,
            stmts: f
                .stmts
                .into_iter()
                .enumerate()
                .map(|(i, s)| self.resolve_stmt(s, i == num_stmts - 1))
                .collect::<Result<_>>()?,
            return_type: f
                .return_type
                .map(|t| self.resolve_type(t))
                .transpose()?
                .unwrap_or(default_function_type()),
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
            grammar::Type::Unit => Type::Unit,
            grammar::Type::Int => Type::Int,
            grammar::Type::Bool => Type::Bool,
            grammar::Type::Str => Type::Str,
        })
    }

    fn resolve_stmt(&mut self, stmt: grammar::Stmt, is_last_stmt_in_block: bool) -> Result<Stmt> {
        match stmt {
            grammar::Stmt::Expr(e) => Ok(self.resolve_expr_stmt(e, is_last_stmt_in_block)?),
            grammar::Stmt::Let(l) => Ok(Stmt::Let(self.resolve_let_stmt(l)?)),
            grammar::Stmt::Return(s) => Ok(Stmt::Return(self.resolve_return_stmt(s)?)),
            grammar::Stmt::Assignment(a) => Ok(Stmt::Assignment(self.resolve_assignment(a)?)),
        }
    }

    fn resolve_assignment(&mut self, s: grammar::Assignment) -> Result<Assignment> {
        let lhs = self.lookup_var(&s.lhs)?;
        let rhs = self.resolve_expr(s.rhs)?;

        Ok(Assignment { lhs, rhs })
    }

    fn resolve_expr_stmt(
        &mut self,
        s: grammar::ExprStmt,
        is_last_stmt_in_block: bool,
    ) -> Result<Stmt> {
        let expr = self.resolve_expr(s.expr)?;
        if !s.has_trailing_semi {
            if is_last_stmt_in_block {
                Ok(Stmt::Return(ReturnStmt { expr: Some(expr) }))
            } else {
                // We should never get here, since a missing semi should cause a
                // parsing error upstream
                unreachable!()
            }
        } else {
            Ok(Stmt::Expr(expr))
        }
    }

    fn resolve_return_stmt(&mut self, l: grammar::ReturnStmt) -> Result<ReturnStmt> {
        Ok(ReturnStmt {
            expr: l.expr.map(|e| self.resolve_expr(e)).transpose()?,
        })
    }

    fn resolve_let_stmt(&mut self, l: grammar::LetStmt) -> Result<LetStmt> {
        let type_ = l.type_.map(|t| self.resolve_type(t)).transpose()?;
        Ok(LetStmt {
            lhs: self.resolve_let_lhs(l.lhs)?,
            type_,
            rhs: l.rhs.map(|e| self.resolve_expr(e)).transpose()?,
        })
    }

    fn resolve_let_lhs(&mut self, lhs: grammar::LetLhs) -> Result<LetLhs> {
        Ok(match lhs {
            grammar::LetLhs::Var(ident) => {
                let var_id = self.register_var(&ident);
                LetLhs::Var(var_id)
            }
            grammar::LetLhs::Pat(pat) => LetLhs::Pat(self.resolve_pat(pat)?),
        })
    }

    fn resolve_fn_call(&mut self, f: grammar::FnCall) -> Result<FnCall> {
        Ok(FnCall {
            id: self.lookup_var(&f.fn_name)?,
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
            grammar::Expr::Pat(pat) => Ok(Expr::Pat(self.resolve_pat(pat)?)),
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
            grammar::Lit::Bool(lit_bool) => Ok(Lit::Bool(lit_bool.value())),
        }
    }

    fn resolve_pat(&mut self, p: grammar::Pat) -> Result<PatId> {
        let pat = UnresolvedPat {
            tokens: p.tokens,
            vars: p
                .vars
                .into_iter()
                .map(|x| self.lookup_or_register_var(&x.name))
                .collect::<Result<_>>()?,
        };
        Ok(self.pats.add(pat))
    }
}

fn check_names_unique(fns: &[MoltFn]) -> Result<()> {
    let mut set = HashSet::new();
    for f in fns.iter() {
        if !set.insert(&f.name) {
            return Err(ResolverError::DuplicateDefinitionFn(f.name.clone()));
        }
    }
    Ok(())
}

fn default_function_type() -> Type {
    Type::Unit
}

pub(crate) fn resolve_pats(
    file: &MoltFile,
    typeck: &TypecheckResult,
    unresolved: Storage<PatId, UnresolvedPat>,
) -> Result<Storage<PatId, ResolvedPat>> {
    unresolved
        .into_iter_enumerate()
        .map(|(i, pat)| resolve_pat(file, typeck, pat, i))
        .collect()
}

fn resolve_pat(
    file: &MoltFile,
    typeck: &TypecheckResult,
    p: UnresolvedPat,
    id: PatId,
) -> Result<ResolvedPat> {
    let mut pat_ctx = Ctx::<Node>::new(Mode::Molt);
    let vars = p
        .vars
        .iter()
        .map(|var_id| {
            let typechecker::Type::Kind(kind) = typeck.get_type(*var_id).unwrap() else {
                unreachable!()
            };
            let name = &file.var_names[var_id.0];
            let ctx_id = pat_ctx.add_var::<Node>(Var::new(name.clone(), *kind));
            TokenVar {
                ctx_id: ctx_id.into(),
                var_id: *var_id,
            }
        })
        .collect();
    let typechecker::Type::Kind(kind) = typeck.get_pat_type(id).unwrap() else {
        unreachable!()
    };
    let ctx = Rc::new(RefCell::new(pat_ctx));
    let node = crate::parser::parse_with_ctx(
        ctx.clone(),
        |stream| parse_node_with_kind(stream, *kind),
        p.tokens,
        Mode::Molt,
    )
    .map_err(Error::Parse)?;
    let ctx = ctx.replace(Ctx::new(Mode::Molt));
    Ok(ResolvedPat { vars, ctx, node })
}
