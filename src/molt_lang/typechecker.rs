use std::collections::HashMap;

use crate::{
    KindType,
    molt_lang::{BuiltinFn, MoltFile, MoltFn, PartialMoltFile, PatId, Stmt, UnresolvedPat, VarId},
    rust_grammar::{Ident, Kind},
    storage::{Storage, StorageIndex},
};

#[derive(Debug)]
pub(crate) enum Error {
    TypeMismatch(Type, Type),
    UntypedVar(Ident),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TypeMismatch(t1, t2) => {
                write!(f, "Type mismatch. Expected {t1:?}, found {t2:?}")
            }
            Error::UntypedVar(t2) => {
                write!(f, "Could not infer type for variable `{t2}`")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Var,
    Kind(Kind),
    Int,
    Bool,
    Str,
    Unit,
    Fun(Vec<TypeId>, TypeId),
}

impl Type {
    fn substitute(&mut self, old: TypeId, new: TypeId) {
        match self {
            Type::Var | Type::Kind(_) | Type::Int | Type::Bool | Type::Str | Type::Unit => {}
            Type::Fun(args, output) => {
                for id in args.iter_mut() {
                    if *id == old {
                        *id = new;
                    }
                }
                if *output == old {
                    *output = new;
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct Scheme {
    generalized: Vec<TypeId>,
    type_id: TypeId,
}

#[derive(Debug, Clone)]
pub enum ResolvedType {
    Var,
    Kind(Kind),
    Int,
    Bool,
    Str,
    Unit,
    Fun(Vec<ResolvedType>, Box<ResolvedType>),
}

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Kind(kind) => write!(f, "{}", kind),
            ResolvedType::Var => write!(f, "var"),
            ResolvedType::Int => write!(f, "int"),
            ResolvedType::Bool => write!(f, "bool"),
            ResolvedType::Str => write!(f, "str"),
            ResolvedType::Unit => write!(f, "()"),
            ResolvedType::Fun(args, ret) => {
                write!(f, "fn(")?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
                write!(f, " -> {}", ret)
            }
        }
    }
}

impl From<crate::molt_lang::Type> for Type {
    fn from(value: crate::molt_lang::Type) -> Self {
        match value {
            super::Type::Kind(kind) => Type::Kind(kind),
            super::Type::Unit => Type::Unit,
            super::Type::Int => Type::Int,
            super::Type::Bool => Type::Bool,
            super::Type::Str => Type::Str,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct TypeId(usize);

enum VarType {
    Mono(TypeId),
    Scheme(Scheme),
}

pub(super) struct TypecheckResult {
    types: Vec<Type>,
    substitutions: HashMap<TypeId, TypeId>,
    vars: HashMap<VarId, VarType>,
    pat_types: HashMap<PatId, TypeId>,
}

impl TypecheckResult {
    // duplication :|
    pub(super) fn get_type(&self, var: VarId) -> Option<&Type> {
        let type_id = match &self.vars[&var] {
            VarType::Mono(type_id) => type_id,
            VarType::Scheme(_) => {
                // Don't iterate over schemes
                return None;
            }
        };
        Some(&self.types[self.resolve(*type_id).0])
    }

    pub(super) fn get_pat_type(&self, pat: PatId) -> Option<&Type> {
        let type_id = &self.pat_types[&pat];
        Some(&self.types[self.resolve(*type_id).0])
    }

    // duplication :|
    fn resolve(&self, id: TypeId) -> TypeId {
        self.substitutions
            .get(&id)
            .map(|id| self.resolve(*id))
            .unwrap_or(id)
    }
}

pub(super) struct Typechecker<'a> {
    types: Vec<Type>,
    substitutions: HashMap<TypeId, TypeId>,
    vars: HashMap<VarId, VarType>,
    pats: &'a Storage<PatId, UnresolvedPat>,
    pat_types: HashMap<PatId, TypeId>,
}

impl<'a> Typechecker<'a> {
    pub(super) fn new(pats: &'a Storage<PatId, UnresolvedPat>) -> Self {
        Self {
            types: Default::default(),
            substitutions: Default::default(),
            vars: Default::default(),
            pat_types: Default::default(),
            pats,
        }
    }

    pub(super) fn get_type(&self, var: VarId) -> Option<&Type> {
        let type_id = match &self.vars[&var] {
            VarType::Mono(type_id) => type_id,
            VarType::Scheme(_) => {
                // Don't iterate over schemes
                return None;
            }
        };
        Some(&self.types[self.resolve(*type_id).0])
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = (VarId, ResolvedType)> {
        let mut keys: Vec<_> = self.vars.keys().cloned().collect();
        keys.sort_by_key(|id| id.to_index());
        keys.into_iter()
            .filter_map(|var| self.get_type(var).map(|ty| (var, self.as_resolved(ty))))
    }

    fn as_resolved(&self, type_: &Type) -> ResolvedType {
        match type_ {
            Type::Var => ResolvedType::Var,
            Type::Kind(kind) => ResolvedType::Kind(*kind),
            Type::Int => ResolvedType::Int,
            Type::Bool => ResolvedType::Bool,
            Type::Str => ResolvedType::Str,
            Type::Unit => ResolvedType::Unit,
            Type::Fun(type_ids, type_id) => ResolvedType::Fun(
                type_ids
                    .iter()
                    .map(|t| self.as_resolved(&self.types[t.0]))
                    .collect(),
                Box::new(self.as_resolved(&self.types[type_id.0])),
            ),
        }
    }

    #[allow(unused)] // For now.
    pub(crate) fn debug_print(&self, file: &MoltFile) {
        for (id, resolved_type) in self.iter_vars() {
            println!("{}: {}", file.var_names[id], resolved_type);
        }
    }

    fn add_type(&mut self, t: Type) -> TypeId {
        self.types.push(t);
        TypeId(self.types.len() - 1)
    }

    fn add_var_type(&mut self, id: VarId, type_: VarType) -> Result<TypeId, Error> {
        let type_id = match &type_ {
            VarType::Mono(type_id) => *type_id,
            VarType::Scheme(scheme) => scheme.type_id,
        };
        if let Some(before) = self.lookup(id) {
            self.unify(type_id, before)?;
        } else {
            self.vars.insert(id, type_);
        }
        Ok(type_id)
    }

    fn add_var(&mut self, id: VarId, type_: Type) -> Result<TypeId, Error> {
        let type_id = self.add_type(type_);
        self.add_var_type(id, VarType::Mono(type_id))
    }

    fn add_pat(&mut self, pat: PatId, type_: Type) -> Result<TypeId, Error> {
        let type_id = self.add_type(type_);
        let entry_before = self.pat_types.insert(pat, type_id);
        assert!(entry_before.is_none());
        Ok(type_id)
    }

    fn resolve(&self, id: TypeId) -> TypeId {
        self.substitutions
            .get(&id)
            .map(|id| self.resolve(*id))
            .unwrap_or(id)
    }

    pub(crate) fn check(mut self, file: &PartialMoltFile) -> Result<TypecheckResult, Error> {
        let fn_return_types: Vec<_> = file.fns.iter().map(|f| self.declare_fn(f)).collect();
        for (id, f) in file.builtin_map.iter() {
            self.declare_builtin(*id, *f);
        }
        for (f, type_id) in file.fns.iter().zip(fn_return_types) {
            self.check_fn(f, type_id)?;
        }
        self.check_no_untyped_vars(file)?;
        Ok(TypecheckResult {
            types: self.types,
            substitutions: self.substitutions,
            vars: self.vars,
            pat_types: self.pat_types,
        })
    }

    pub(crate) fn check_no_untyped_vars(&self, file: &PartialMoltFile) -> Result<(), Error> {
        for id in self.vars.keys() {
            if let Some(Type::Var) = self.get_type(*id) {
                let name = file.var_names[*id].clone();
                return Err(Error::UntypedVar(name));
            }
        }
        Ok(())
    }

    fn declare_fn(&mut self, f: &MoltFn) -> TypeId {
        let input = f
            .args
            .iter()
            .map(|arg| {
                self.add_var(arg.var_id, arg.type_.clone().into())
                    // At this point, the variable cannot have been in scope before, so
                    // this always returns Ok(..)
                    .unwrap()
            })
            .collect();
        let output = self.add_type(f.return_type.clone().into());
        self.add_var(f.id, Type::Fun(input, output)).unwrap(); // We already checked that the fn only exists once during resolution
        output
    }

    fn declare_builtin(&mut self, id: VarId, f: BuiltinFn) {
        let builtin_type = self.builtin_type(f);
        self.add_var_type(id, builtin_type).unwrap();
    }

    fn check_fn(&mut self, f: &MoltFn, fn_return_type: TypeId) -> Result<(), Error> {
        for stmt in f.stmts.iter() {
            self.check_stmt(stmt, fn_return_type)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt, surrounding_fn_return_type: TypeId) -> Result<(), Error> {
        match stmt {
            Stmt::Expr(expr) => {
                self.infer_expr(expr)?;
            }
            Stmt::Let(let_stmt) => self.check_let(let_stmt)?,
            Stmt::Return(ret_stmt) => {
                self.check_return_stmt(ret_stmt, surrounding_fn_return_type)?
            }
            Stmt::Assignment(assignment) => {
                self.check_assignment(assignment)?;
            }
            Stmt::If(if_stmt) => {
                self.check_if(if_stmt, surrounding_fn_return_type)?;
            }
        }
        Ok(())
    }

    fn infer_expr(&mut self, expr: &super::Expr) -> Result<TypeId, Error> {
        match expr {
            super::Expr::FnCall(fn_call) => {
                let input = fn_call
                    .args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<Result<_, Error>>()?;
                let output = self.add_type(Type::Var);
                let fn_type = self.add_type(Type::Fun(input, output));
                // We know the function is defined,
                // so we can unwrap
                let lookup = self.lookup(fn_call.id).unwrap();
                self.unify(fn_type, lookup)?;
                Ok(output)
            }
            super::Expr::Atom(atom) => self.infer_atom(atom),
            super::Expr::Pat(pat) => self.infer_pat(pat),
        }
    }

    fn infer_pat(&mut self, id: &PatId) -> Result<TypeId, Error> {
        // TODO: Check correctness
        self.add_pat(*id, Type::Var)
    }

    fn infer_atom(&mut self, atom: &super::Atom) -> Result<TypeId, Error> {
        match atom {
            super::Atom::Var(var_id) => Ok(self.add_var(*var_id, Type::Var)?),
            super::Atom::Lit(lit) => Ok(self.add_type(lit.type_())),
        }
    }

    fn check_let(&mut self, let_stmt: &super::LetStmt) -> Result<(), Error> {
        let rhs = let_stmt
            .rhs
            .as_ref()
            .map(|rhs| self.infer_expr(rhs))
            .transpose()?;
        let type_id = match &let_stmt.lhs {
            super::LetLhs::Var(var_id) => {
                let type_ = if let Some(ref type_) = let_stmt.type_ {
                    type_.clone().into()
                } else {
                    Type::Var
                };
                self.add_var(*var_id, type_)?
            }
            super::LetLhs::Pat(pat_id) => {
                let pat = &self.pats[*pat_id];
                for (var, _) in pat.vars.iter() {
                    self.add_var(*var, Type::Var)?;
                }
                let type_ = if let Some(ref type_) = let_stmt.type_ {
                    type_.clone().into()
                } else {
                    Type::Var
                };
                self.add_pat(*pat_id, type_)?
            }
        };
        if let Some(rhs) = rhs {
            self.unify(type_id, rhs)?;
        }
        Ok(())
    }

    fn check_return_stmt(
        &mut self,
        ret_stmt: &super::ReturnStmt,
        fn_return_type: TypeId,
    ) -> Result<(), Error> {
        let expr_type = ret_stmt
            .expr
            .as_ref()
            .map(|expr| self.infer_expr(expr))
            .unwrap_or_else(|| {
                Ok(self.add_type(Type::Unit)) // Default return type 
            })?;
        self.unify(fn_return_type, expr_type)?;
        Ok(())
    }

    fn check_if(
        &mut self,
        if_stmt: &super::If,
        surrounding_fn_return_type: TypeId,
    ) -> Result<(), Error> {
        for branch in if_stmt.if_branches.iter() {
            let ty = self.infer_expr(&branch.0)?;
            let bool_ty = self.add_type(Type::Bool);
            self.unify(ty, bool_ty)?;

            for stmt in &branch.1 {
                self.check_stmt(stmt, surrounding_fn_return_type)?;
            }
        }
        Ok(())
    }

    fn check_assignment(&mut self, a: &super::Assignment) -> Result<(), Error> {
        let lhs = self.lookup(a.lhs).unwrap(); // Resolver makes sure this exists
        let rhs = self.infer_expr(&a.rhs)?;
        self.unify(lhs, rhs)
    }

    fn unify(&mut self, t1: TypeId, t2: TypeId) -> Result<(), Error> {
        let t1 = self.resolve(t1);
        let t2 = self.resolve(t2);
        if t1 == t2 {
            return Ok(());
        }
        let type1 = self.types[t1.0].clone();
        let type2 = self.types[t2.0].clone();
        let make_error = || Err(Error::TypeMismatch(type1.clone(), type2.clone()));
        match (&type1, &type2) {
            (Type::Var, _) => {
                self.substitutions.insert(t1, t2);
            }
            (_, Type::Var) => {
                self.substitutions.insert(t2, t1);
            }
            (Type::Kind(kind1), Type::Kind(kind2)) => {
                if !kind1.is_comparable_to(kind2.into_node_kind()) {
                    return make_error();
                }
            }
            (Type::Int, Type::Int) => {}
            (Type::Bool, Type::Bool) => {}
            (Type::Str, Type::Str) => {}
            (Type::Unit, Type::Unit) => {}
            (Type::Fun(args1, ret1), Type::Fun(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return make_error();
                }
                for (arg1, arg2) in args1.iter().zip(args2) {
                    self.unify(*arg1, *arg2)?;
                }
                self.unify(*ret1, *ret2)?;
            }
            (_, _) => return make_error(),
        }
        Ok(())
    }

    pub(crate) fn builtin_type(&mut self, f: BuiltinFn) -> VarType {
        let mut mk_fn_type = |args: Vec<Type>, output| {
            let args = args.into_iter().map(|arg| self.add_type(arg)).collect();
            let output = self.add_type(output);
            VarType::Mono(self.add_type(Type::Fun(args, output)))
        };
        match f {
            BuiltinFn::Print => mk_fn_type(vec![Type::Var], Type::Unit),
            BuiltinFn::Dbg => mk_fn_type(vec![Type::Var], Type::Unit),
            BuiltinFn::Assert => mk_fn_type(vec![Type::Bool], Type::Unit),
            BuiltinFn::AssertEq => {
                let t = self.add_type(Type::Var);
                let add_type = self.add_type(Type::Unit);
                let type_id = self.add_type(Type::Fun(vec![t, t], add_type));
                VarType::Scheme(Scheme {
                    type_id,
                    generalized: vec![t],
                })
            }
        }
    }

    fn lookup(&mut self, id: VarId) -> Option<TypeId> {
        #[allow(clippy::manual_map)] // borrowck
        if let Some(var) = self.vars.get(&id) {
            Some(match var {
                VarType::Mono(id) => *id,
                VarType::Scheme(scheme) => {
                    let scheme = scheme.clone(); // make borrowck happy
                    self.instantiate(&scheme)
                }
            })
        } else {
            None
        }
    }

    fn instantiate(&mut self, scheme: &Scheme) -> TypeId {
        let mut new_type = self.types[scheme.type_id.0].clone();
        for var in scheme.generalized.iter() {
            let new_var = self.add_type(Type::Var);
            new_type.substitute(*var, new_var);
        }
        self.add_type(new_type)
    }
}
