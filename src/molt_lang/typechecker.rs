use std::collections::HashMap;

use crate::{
    KindType,
    molt_lang::{BuiltinFn, MoltFile, MoltFn, Stmt, VarId},
    rust_grammar::Kind,
};

#[derive(Debug)]
pub(crate) enum Error {
    TypeMismatch(Type, Type),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TypeMismatch(t1, t2) => {
                write!(f, "Error: type mismatch. Expected {t1:?}, found {t2:?}")
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
            ResolvedType::Int => write!(f, "Int"),
            ResolvedType::Bool => write!(f, "Bool"),
            ResolvedType::Str => write!(f, "Str"),
            ResolvedType::Unit => write!(f, "Unit"),
            ResolvedType::Fun(args, ret) => {
                write!(f, "Fn(")?;
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

#[derive(Default)]
pub(super) struct Typechecker {
    types: Vec<Type>,
    substitutions: HashMap<TypeId, TypeId>,
    vars: HashMap<VarId, TypeId>,
}

pub(super) struct TypecheckResult {
    vars: HashMap<VarId, Type>,
    types: Vec<Type>,
}

impl TypecheckResult {
    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = (VarId, ResolvedType)> {
        self.vars
            .iter()
            .map(|(id, type_)| (*id, self.as_resolved(type_)))
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
}

impl Typechecker {
    fn add_type(&mut self, t: Type) -> TypeId {
        self.types.push(t);
        TypeId(self.types.len() - 1)
    }

    fn add_var(&mut self, id: VarId, type_: Type) -> Result<TypeId, Error> {
        let type_id = self.add_type(type_);
        if let Some(before) = self.vars.get(&id) {
            self.unify(type_id, *before)?;
        } else {
            self.vars.insert(id, type_id);
        }
        Ok(type_id)
    }

    fn resolve(&self, id: TypeId) -> TypeId {
        self.substitutions
            .get(&id)
            .map(|id| self.resolve(*id))
            .unwrap_or(id)
    }

    pub(crate) fn check(mut self, file: &MoltFile) -> Result<TypecheckResult, Error> {
        for f in file.fns.iter() {
            self.declare_fn(f);
        }
        for (id, f) in file.builtin_map.iter() {
            self.declare_builtin(*id, *f);
        }
        for f in file.fns.iter() {
            self.check_fn(f)?;
        }
        let vars = self
            .vars
            .iter()
            .map(|(var, id)| (*var, self.types[self.resolve(*id).0].clone()))
            .collect();
        Ok(TypecheckResult {
            vars,
            types: self.types,
        })
    }

    fn declare_fn(&mut self, f: &MoltFn) {
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
        self.add_var(f.id, Type::Fun(input, output))
            .unwrap() // We already checked that the fn only exists once during resolution
        ;
    }

    fn declare_builtin(&mut self, id: VarId, f: BuiltinFn) {
        let builtin_type = self.builtin_type(f);
        self.add_var(id, builtin_type).unwrap();
    }

    fn check_fn(&mut self, f: &MoltFn) -> Result<(), Error> {
        for stmt in f.stmts.iter() {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::ExprStmt(expr) => {
                self.infer_expr(expr)?;
            }
            Stmt::Let(let_stmt) => self.check_let(let_stmt)?,
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
                self.unify(fn_type, self.vars[&fn_call.id])?;
                Ok(output)
            }
            super::Expr::Atom(atom) => self.infer_atom(atom),
        }
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
        match &let_stmt.lhs {
            super::LetLhs::Var(var_id) => {
                let type_id = if let Some(ref type_) = let_stmt.type_ {
                    self.add_var(*var_id, type_.clone().into())
                } else {
                    self.add_var(*var_id, Type::Var)
                }?;
                if let Some(rhs) = rhs {
                    self.unify(type_id, rhs)?;
                }
            }
            super::LetLhs::Pat(pat) => {
                for var in pat.vars.iter() {
                    let kind = pat.ctx.get_var_kind(var.ctx_id);
                    self.add_var(var.var_id, Type::Kind(kind))?;
                }
            }
        }
        Ok(())
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
                if kind1.is_comparable_to(kind2.into_node_kind()) {
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

    pub(crate) fn builtin_type(&mut self, f: BuiltinFn) -> Type {
        let (input, output) = match f {
            BuiltinFn::Assert => (vec![self.add_type(Type::Bool)], self.add_type(Type::Unit)),
            BuiltinFn::AssertEq => {
                let t = self.add_type(Type::Var);
                (vec![t, t], self.add_type(Type::Unit))
            }
            BuiltinFn::Print => (vec![self.add_type(Type::Var)], self.add_type(Type::Unit)),
            BuiltinFn::Dbg => (vec![self.add_type(Type::Var)], self.add_type(Type::Unit)),
        };
        Type::Fun(input, output)
    }
}
