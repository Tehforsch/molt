//! Performs name resolution on a molt file,
//! so that each variable is assigned a unique
//! id.
use std::collections::HashMap;
use std::collections::HashSet;

use super::*;
use crate::diag::Diag;
use crate::diag::WithWarnings;
use crate::diag::has_errors;
use crate::error;
use crate::molt_lang::MoltFn;
use crate::molt_lang::builtin_fn::BuiltinFn;
use crate::molt_lang::builtin_fn::builtins_def;
use crate::molt_lang::grammar;
use crate::parser;
use crate::storage::Storage;
use crate::warn;

const IDENTIFIER_PREFIX_ALLOW_UNUSED: &str = "_";

type Result<T, E = Diag> = std::result::Result<T, E>;

type ScopeIndex = usize;

#[derive(Default, Debug)]
struct Scope {
    parent: Option<ScopeIndex>,
    variables: HashMap<String, VarId>,
    initialized: HashSet<VarId>,
    index: ScopeIndex,
}

impl Scope {
    fn child_of(active_scope: &Scope, index: usize) -> Scope {
        Self {
            parent: Some(active_scope.index),
            variables: HashMap::default(),
            initialized: HashSet::default(),
            index,
        }
    }

    fn insert(&mut self, var_name: &str, var_id: VarId) {
        self.variables.insert(var_name.into(), var_id);
    }
}

struct Var {
    name: Ident,
}

pub struct Resolver {
    scopes: Vec<Scope>,
    vars: Storage<VarId, Var>,
    fn_map: HashMap<VarId, FnId>,
    builtin_map: HashMap<VarId, BuiltinFn>,
    pats: Storage<PatId, UnparsedPat>,
    used: HashSet<VarId>,
}

impl Default for Resolver {
    fn default() -> Self {
        Self {
            vars: Storage::default(),
            scopes: vec![Scope::default()],
            fn_map: HashMap::default(),
            builtin_map: HashMap::default(),
            pats: Storage::default(),
            used: HashSet::default(),
        }
    }
}

impl Resolver {
    pub fn resolve(
        file: grammar::MoltFile,
    ) -> std::result::Result<WithWarnings<ResolvedMoltFile>, Vec<Diag>> {
        let r = Resolver::default();
        r.resolve_internal(file)
    }

    fn resolve_internal(
        mut self,
        file: grammar::MoltFile,
    ) -> std::result::Result<WithWarnings<ResolvedMoltFile>, Vec<Diag>> {
        let grammar::MoltFile { fns, stmts: _ } = file;
        self.register_builtins();
        self.register_user_fns(&fns);
        let mut errors = Vec::new();
        let mut resolved_fns = Vec::new();
        for f in fns.into_iter() {
            match self.resolve_fn(f) {
                Ok(f) => resolved_fns.push(f),
                Err(e) => errors.push(e),
            }
        }
        // TODO: Check if this is necessary
        if has_errors(&errors) {
            return Err(errors);
        }
        let fns: Storage<_, _> = resolved_fns.into_iter().collect();
        errors.extend(check_names_unique(&fns));
        errors.extend(self.check_used());
        if has_errors(&errors) {
            return Err(errors);
        }
        Ok(WithWarnings::new(
            ResolvedMoltFile {
                var_names: self.vars.into_iter().map(|var| var.name).collect(),
                fns,
                builtin_map: self.builtin_map,
                pats: self.pats,
            },
            errors,
        ))
    }

    fn active_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn active_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn next_scope_index(&self) -> usize {
        self.scopes.len()
    }

    fn register_var(&mut self, name: &Ident, initialized: bool) -> VarId {
        let name_str = name.to_string();
        let var_id = self.vars.add(Var { name: name.clone() });
        let scope = self.active_scope_mut();
        scope.insert(&name_str, var_id);
        if initialized {
            scope.initialized.insert(var_id);
        }
        var_id
    }

    fn lookup_var_inner(&self, name: &Ident) -> Option<VarId> {
        let mut scope_idx = Some(self.scopes.len() - 1);
        while let Some(idx) = scope_idx {
            let scope = &self.scopes[idx];
            if let Some(val) = scope.variables.get(&name.to_string()) {
                return Some(*val);
            }
            scope_idx = scope.parent;
        }
        None
    }

    fn is_initialized(&self, id: VarId) -> bool {
        let mut scope_idx = Some(self.scopes.len() - 1);
        while let Some(idx) = scope_idx {
            let scope = &self.scopes[idx];
            if scope.initialized.contains(&id) {
                return true;
            }
            scope_idx = scope.parent;
        }
        false
    }

    fn lookup_var(&self, name: &Ident) -> Result<VarId> {
        let Some(id) = self.lookup_var_inner(name) else {
            return Err(error!("undefined variable: '{name}'")
                .label(name.span(), "not found in this scope"));
        };
        if !self.is_initialized(id) {
            return Err(error!("variable not initialized: '{name}'")
                .label(name.span(), "used before initialization")
                .label(self.vars[id].name.span(), "declared here without a value"));
        }
        Ok(id)
    }

    fn initialize_var(&mut self, id: VarId) {
        self.active_scope_mut().initialized.insert(id);
    }

    fn lookup_or_register_var(&mut self, name: &Ident) -> VarId {
        let id = self
            .lookup_var_inner(name)
            .unwrap_or_else(|| self.register_var(name, true));
        self.initialize_var(id);
        id
    }

    fn make_scope(&mut self) -> Scope {
        Scope::child_of(self.active_scope(), self.next_scope_index())
    }

    fn mark_used(&mut self, id: VarId) {
        self.used.insert(id);
    }

    fn register_builtins(&mut self) {
        for (name, f) in builtins_def() {
            let var = self.register_var(&name, true);
            self.builtin_map.insert(var, f);
            self.mark_used(var);
        }
    }

    fn register_user_fns(&mut self, fns: &Storage<FnId, grammar::MoltFn>) {
        for (i, f) in fns.enumerate() {
            let var = self.register_var(&f.name, true);
            // The main function is implicitly used
            if f.name == MAIN_FN_NAME {
                self.mark_used(var);
            }
            self.fn_map.insert(var, i);
        }
    }

    fn resolve_fn(&mut self, f: grammar::MoltFn) -> Result<MoltFn> {
        let id = self.lookup_var(&f.name).unwrap();
        let scope = self.make_scope();
        self.scopes.push(scope);
        let args = f
            .args
            .into_iter()
            .map(|a| {
                let arg = self.resolve_fn_arg(a)?;
                // The input argument to the main fn is implicitly used.
                if f.name == MAIN_FN_NAME && self.vars[arg.var_id].name == INPUT_VAR_NAME {
                    self.mark_used(arg.var_id);
                }
                Ok(arg)
            })
            .collect::<Result<_>>()?;
        let stmts = self.resolve_block_stmts(f.stmts)?;
        self.scopes.pop();
        let result = MoltFn {
            id,
            name: f.name,
            args,
            stmts,
            return_type: f
                .return_type
                .map(|t| self.resolve_type(t))
                .transpose()?
                .unwrap_or(default_function_type()),
        };
        Ok(result)
    }

    fn resolve_block(&mut self, f: grammar::Block, scope: Scope) -> Result<Vec<Stmt>> {
        self.scopes.push(scope);
        let result = self.resolve_block_stmts(f);
        self.scopes.pop();
        result
    }

    fn resolve_block_stmts(&mut self, f: grammar::Block) -> Result<Vec<Stmt>> {
        let num_stmts = f.stmts.len();
        f.stmts
            .into_iter()
            .enumerate()
            .map(|(i, s)| self.resolve_stmt(s, i == num_stmts - 1))
            .collect::<Result<_>>()
    }

    fn resolve_fn_arg(&mut self, arg: grammar::FnArg) -> Result<FnArg> {
        let var_id = self.register_var(&arg.var_name, true);
        Ok(FnArg {
            var_id,
            type_: self.resolve_type(arg.type_)?,
        })
    }

    fn resolve_type(&mut self, arg: grammar::Type) -> Result<Type> {
        Ok(match arg {
            grammar::Type::Kind(k) => Type::Kind(k),
            grammar::Type::Unit => Type::Unit,
            grammar::Type::Int => Type::Int,
            grammar::Type::Bool => Type::Bool,
            grammar::Type::List(ty) => Type::List(Box::new(self.resolve_type(*ty)?)),
            grammar::Type::Str => Type::Str,
        })
    }

    fn resolve_stmt(&mut self, stmt: grammar::Stmt, is_last_stmt_in_block: bool) -> Result<Stmt> {
        match stmt {
            grammar::Stmt::Expr(e) => Ok(self.resolve_expr_stmt(e, is_last_stmt_in_block)?),
            grammar::Stmt::Let(l) => Ok(Stmt::Let(self.resolve_let_stmt(l)?)),
            grammar::Stmt::Return(s) => Ok(Stmt::Return(self.resolve_return_stmt(s)?)),
            grammar::Stmt::Assignment(a) => Ok(Stmt::Assignment(self.resolve_assignment(a)?)),
            grammar::Stmt::If(i) => Ok(Stmt::If(self.resolve_if(i)?)),
            grammar::Stmt::IfLet(i) => Ok(Stmt::IfLet(self.resolve_if_let(i)?)),
            grammar::Stmt::For(f) => Ok(Stmt::For(self.resolve_for(f)?)),
        }
    }

    fn resolve_assignment(&mut self, s: grammar::Assignment) -> Result<Assignment> {
        let lhs = self.resolve_assignment_lhs(s.lhs)?;
        let rhs = self.resolve_expr(s.rhs)?;

        Ok(Assignment { lhs, rhs })
    }

    fn resolve_assignment_lhs(&mut self, lhs: grammar::AssignmentLhs) -> Result<AssignmentLhs> {
        match lhs {
            grammar::AssignmentLhs::Var(ident) => {
                let Some(id) = self.lookup_var_inner(&ident) else {
                    return Err(error!("undefined variable: '{ident}'")
                        .label(ident.span(), "not found in this scope"));
                };
                self.initialize_var(id);
                // TODO: Due to the way the molt grammar works,
                // assignments (a = b) have side effects due to
                // modifications happening when values are assigned
                // to a node LHS. This means we need to consider a variable
                // used even if it only appears on the LHS of a statement.
                // Unfortunately we don't have type information at this point
                // so we cannot check if it the LHS is of node type or not.
                // Eventually, we might want to re-think the assignment syntax
                // for modifications anyways.
                self.mark_used(id);
                Ok(AssignmentLhs::Var(id))
            }
            grammar::AssignmentLhs::FieldAccess { lhs, field } => Ok(AssignmentLhs::FieldAccess {
                lhs: Box::new(self.resolve_assignment_lhs(*lhs)?),
                field,
            }),
        }
    }

    fn resolve_if(&mut self, i: grammar::If) -> Result<If> {
        let if_branches = i
            .if_branches
            .into_iter()
            .map(|(cond, stmts)| {
                let cond = self.resolve_expr(cond)?;
                let scope = self.make_scope();
                let stmts = self.resolve_block(stmts, scope)?;
                Ok((cond, stmts))
            })
            .collect::<Result<_>>()?;
        let else_branch: Option<Vec<Stmt>> = i
            .else_branch
            .map(|stmts| {
                let scope = self.make_scope();
                self.resolve_block(stmts, scope)
            })
            .transpose()?;
        Ok(If {
            if_branches,
            else_branch,
        })
    }

    fn resolve_if_let(&mut self, i: grammar::IfLet) -> Result<IfLet> {
        let if_branches = i
            .if_branches
            .into_iter()
            .map(|(lhs, expr, block)| {
                let expr = self.resolve_expr(expr)?;
                let scope = self.make_scope();
                self.scopes.push(scope);
                let has_rhs = true;
                let lhs = self.resolve_let_lhs(lhs, has_rhs)?;
                let num_stmts = block.stmts.len();
                let stmts = block
                    .stmts
                    .into_iter()
                    .enumerate()
                    .map(|(i, s)| self.resolve_stmt(s, i == num_stmts - 1))
                    .collect::<Result<_>>()?;
                self.scopes.pop();
                Ok((lhs, expr, stmts))
            })
            .collect::<Result<_>>()?;
        let else_branch = i
            .else_branch
            .map(|stmts| {
                let scope = self.make_scope();
                self.resolve_block(stmts, scope)
            })
            .transpose()?;
        Ok(IfLet {
            if_branches,
            else_branch,
        })
    }

    fn resolve_for(&mut self, f: grammar::For) -> Result<For> {
        let iterable = self.resolve_expr(f.iterable)?;
        let scope = self.make_scope();
        self.scopes.push(scope);
        let has_rhs = true;
        let lhs = self.resolve_let_lhs(f.lhs, has_rhs)?;
        let num_stmts = f.block.stmts.len();
        let stmts = f
            .block
            .stmts
            .into_iter()
            .enumerate()
            .map(|(i, s)| self.resolve_stmt(s, i == num_stmts - 1))
            .collect::<Result<_>>()?;
        self.scopes.pop();
        Ok(For {
            lhs,
            iterable,
            stmts,
        })
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
        let has_rhs = l.rhs.is_some();
        Ok(LetStmt {
            lhs: self.resolve_let_lhs(l.lhs, has_rhs)?,
            type_,
            rhs: l.rhs.map(|e| self.resolve_expr(e)).transpose()?,
        })
    }

    fn resolve_let_lhs(&mut self, lhs: grammar::LetLhs, has_rhs: bool) -> Result<LetLhs> {
        Ok(match lhs {
            grammar::LetLhs::Var(ident) => {
                let initialized = has_rhs;
                let var_id = self.register_var(&ident, initialized);
                LetLhs::Var(var_id)
            }
            grammar::LetLhs::Pat(pat) => {
                if !has_rhs {
                    return Err(error!(
                        "Let statement with pattern on left-hand side has no right-hand side."
                    ));
                }
                LetLhs::Pat(self.resolve_pat(pat)?)
            }
        })
    }

    fn resolve_fn_call(&mut self, f: grammar::FnCall) -> Result<FnCall> {
        let id = self.lookup_var(&f.fn_name)?;
        self.mark_used(id);
        Ok(FnCall {
            id,
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
            grammar::Expr::Atom(atom) => Ok(Expr::Atom(self.resolve_atom(atom)?)),
            grammar::Expr::Pat(pat) => Ok(Expr::Pat(self.resolve_pat(pat)?)),
            grammar::Expr::FieldAccess(fa) => Ok(Expr::FieldAccess(FieldAccess {
                lhs: Box::new(self.resolve_expr(*fa.lhs)?),
                field: fa.field,
            })),
        }
    }

    fn resolve_atom(&mut self, atom: grammar::Atom) -> Result<Atom> {
        match atom {
            grammar::Atom::Lit(lit) => Ok(Atom::Lit(self.resolve_lit(&lit)?)),
            grammar::Atom::Var(ident) => Ok(Atom::Var(self.resolve_var_atom(&ident)?)),
            grammar::Atom::List(list) => Ok(Atom::List(self.resolve_list(list)?)),
        }
    }

    fn resolve_lit(&self, lit: &grammar::Lit) -> Result<Lit> {
        match lit {
            grammar::Lit::Int(lit_int) => {
                let val = lit_int
                    .base10_digits()
                    .parse()
                    .map_err::<Diag, _>(|err| parser::Error::new(lit_int.span(), err).into())?;
                Ok(Lit::Int(val))
            }
            grammar::Lit::Str(lit_str) => Ok(Lit::Str(lit_str.value())),
            grammar::Lit::Bool(lit_bool) => Ok(Lit::Bool(lit_bool.value())),
        }
    }

    fn resolve_list(&mut self, lit: grammar::List) -> Result<List> {
        Ok(List {
            items: lit
                .items
                .into_iter()
                .map(|e| self.resolve_expr(e))
                .collect::<Result<_>>()?,
        })
    }

    fn resolve_pat(&mut self, p: grammar::Pat) -> Result<PatId> {
        let pat = UnparsedPat {
            tokens: p.tokens,
            vars: p
                .vars
                .into_iter()
                .map(|x| {
                    let id = self.lookup_or_register_var(&x.name);
                    // Variables in patterns can merely be used for pattern
                    // matching but never referred to afterwards. This is
                    // perfectly fine and does not mean the variable is unused.
                    self.mark_used(id);
                    Ok((id, x.span))
                })
                .collect::<Result<_>>()?,
        };
        Ok(self.pats.add(pat))
    }

    fn resolve_var_atom(&mut self, ident: &Ident) -> Result<VarId> {
        let id = self.lookup_var(ident)?;
        self.mark_used(id);
        Ok(id)
    }

    fn check_used(&self) -> Vec<Diag> {
        let mut diags = Vec::new();
        for (id, var) in self.vars.enumerate() {
            if !self.used.contains(&id)
                && !var
                    .name
                    .to_string()
                    .starts_with(IDENTIFIER_PREFIX_ALLOW_UNUSED)
            {
                diags.push(
                    warn!("Variable is never used: {}", var.name)
                        .label(var.name.span(), "declared here"),
                );
            }
        }
        diags
    }
}

fn check_names_unique(fns: &[MoltFn]) -> Vec<Diag> {
    let mut diags = Vec::new();
    let mut seen: HashSet<&Ident> = HashSet::new();
    for f in fns.iter() {
        if let Some(prev) = seen.get(&f.name) {
            diags.push(
                error!("function defined twice: '{}'", f.name)
                    .label(f.name.span(), "duplicate definition")
                    .label(prev.span(), "first defined here"),
            );
        } else {
            seen.insert(&f.name);
        }
    }
    diags
}

fn default_function_type() -> Type {
    Type::Unit
}
