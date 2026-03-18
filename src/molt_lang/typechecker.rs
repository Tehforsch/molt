use std::collections::HashMap;

use crate::{
    KindType,
    molt_lang::{BuiltinFn, MoltFile, MoltFn, PartialMoltFile, PatId, Stmt, UnresolvedPat, VarId},
    rust_grammar::{Ident, Kind},
    storage::{Storage, StorageIndex},
};

pub(crate) struct Error {
    pub(crate) kind: ErrorKind,
    pub(crate) labels: Vec<ErrorLabel>,
}

pub(crate) struct ErrorLabel {
    pub(crate) span: crate::Span,
    pub(crate) message: String,
}

pub(crate) enum ErrorKind {
    TypeMismatch {
        expected: ResolvedType,
        found: ResolvedType,
    },
    UntypedVar(Ident),
    NotIterable(ResolvedType),
}

impl Error {
    fn type_mismatch(expected: ResolvedType, found: ResolvedType) -> Self {
        Self {
            kind: ErrorKind::TypeMismatch { expected, found },
            labels: Vec::new(),
        }
    }

    fn type_not_iterable(ty: ResolvedType) -> Self {
        Self {
            kind: ErrorKind::NotIterable(ty),
            labels: Vec::new(),
        }
    }

    fn untyped_var(ident: Ident) -> Self {
        Self {
            kind: ErrorKind::UntypedVar(ident),
            labels: Vec::new(),
        }
    }

    fn with_label(mut self, span: crate::Span, message: impl Into<String>) -> Self {
        self.labels.push(ErrorLabel {
            span,
            message: message.into(),
        });
        self
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::TypeMismatch { expected, found } => {
                write!(f, "type mismatch: expected `{expected}`, found `{found}`")
            }
            ErrorKind::UntypedVar(ident) => {
                write!(f, "could not infer type for variable `{ident}`")
            }
            ErrorKind::NotIterable(ty) => {
                write!(f, "cannot iterate over type `{ty}`")
            }
        }
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
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
    List(TypeId),
    Fun(Vec<TypeId>, TypeId),
}

impl Type {
    fn substitute(&mut self, old: TypeId, new: TypeId) {
        match self {
            Type::Var | Type::Kind(_) | Type::Int | Type::Bool | Type::Str | Type::Unit => {}
            Type::List(ty) => {
                if *ty == old {
                    *ty = new;
                }
            }
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
    List(Box<ResolvedType>),
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
            ResolvedType::List(ty) => write!(f, "List<{}>", ty),
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct TypeId(usize);

impl StorageIndex for TypeId {
    fn to_index(self) -> usize {
        self.0
    }

    fn from_index(index: usize) -> Self {
        Self(index)
    }
}

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

/// Used to check control flow logic - represents
/// whether a given statement returns in all
/// possible ways through the control flow (in all
/// branches of an if). We don't do any complex
/// analysis here, but simply check for a literal return
/// stmt in the branches.
#[derive(Clone, Copy)]
enum Returns {
    Always,
    NotAlways,
}

impl Returns {
    fn add_branch(&mut self, other: Self) {
        let new = match (&self, other) {
            (Returns::Always, Returns::Always) => Returns::Always,
            _ => Returns::NotAlways,
        };
        *self = new;
    }
}

pub(super) struct Typechecker<'a> {
    types: Storage<TypeId, Type>,
    substitutions: HashMap<TypeId, TypeId>,
    vars: HashMap<VarId, VarType>,
    var_names: &'a Storage<VarId, Ident>,
    pats: &'a Storage<PatId, UnresolvedPat>,
    pat_types: HashMap<PatId, TypeId>,
}

impl<'a> Typechecker<'a> {
    pub(super) fn new(
        var_names: &'a Storage<VarId, Ident>,
        pats: &'a Storage<PatId, UnresolvedPat>,
    ) -> Self {
        Self {
            types: Default::default(),
            substitutions: Default::default(),
            vars: Default::default(),
            var_names,
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
        Some(&self.types[self.resolve(*type_id)])
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
            Type::List(ty) => ResolvedType::List(Box::new(self.as_resolved(&self.types[*ty]))),
            Type::Fun(type_ids, type_id) => ResolvedType::Fun(
                type_ids
                    .iter()
                    .map(|t| self.as_resolved(&self.types[*t]))
                    .collect(),
                Box::new(self.as_resolved(&self.types[*type_id])),
            ),
        }
    }

    fn var_span(&self, id: VarId) -> crate::Span {
        self.var_names[id].span().byte_range().into()
    }

    #[allow(unused)] // For now.
    pub(crate) fn debug_print(&self, file: &MoltFile) {
        for (id, resolved_type) in self.iter_vars() {
            println!("{}: {}", file.var_names[id], resolved_type);
        }
    }

    fn convert_raw_type(&mut self, t: &super::Type) -> Type {
        match t {
            super::Type::Kind(kind) => Type::Kind(*kind),
            super::Type::Unit => Type::Unit,
            super::Type::Int => Type::Int,
            super::Type::Bool => Type::Bool,
            super::Type::Str => Type::Str,
            super::Type::List(ty) => {
                let ty = self.convert_raw_type(ty);
                let ty = self.add_type(ty);
                Type::List(ty)
            }
        }
    }

    fn add_type(&mut self, t: Type) -> TypeId {
        self.types.add(t)
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
        self.check_no_untyped_vars()?;
        Ok(TypecheckResult {
            types: self.types.into_iter().collect(),
            substitutions: self.substitutions,
            vars: self.vars,
            pat_types: self.pat_types,
        })
    }

    pub(crate) fn check_no_untyped_vars(&self) -> Result<(), Error> {
        for id in self.vars.keys() {
            if let Some(Type::Var) = self.get_type(*id) {
                let name = self.var_names[*id].clone();
                return Err(Error::untyped_var(name));
            }
        }
        Ok(())
    }

    fn declare_fn(&mut self, f: &MoltFn) -> TypeId {
        let input = f
            .args
            .iter()
            .map(|arg| {
                let arg_ty = self.convert_raw_type(&arg.type_);
                self.add_var(arg.var_id, arg_ty)
                    // At this point, the variable cannot have been in scope before, so
                    // this always returns Ok(..)
                    .unwrap()
            })
            .collect();
        let t = self.convert_raw_type(&f.return_type);
        let output = self.add_type(t);
        self.add_var(f.id, Type::Fun(input, output)).unwrap(); // We already checked that the fn only exists once during resolution
        output
    }

    fn declare_builtin(&mut self, id: VarId, f: BuiltinFn) {
        let builtin_type = self.builtin_type(f);
        self.add_var_type(id, builtin_type).unwrap();
    }

    fn check_fn(&mut self, f: &MoltFn, fn_return_type: TypeId) -> Result<(), Error> {
        let returns = self.check_block(&f.stmts, fn_return_type)?;
        if let Returns::NotAlways = returns {
            let unit = self.add_type(Type::Unit);
            let span = self.var_span(f.id);
            let ret_type = self.as_resolved(&self.types[self.resolve(fn_return_type)].clone());
            self.unify(unit, fn_return_type)
                .map_err(|e| e.with_label(span, format!("expects return type `{ret_type}`")))?;
        }
        Ok(())
    }

    fn check_block(&mut self, block: &[Stmt], fn_return_type: TypeId) -> Result<Returns, Error> {
        for stmt in block.iter() {
            if let Returns::Always = self.check_stmt(stmt, fn_return_type)? {
                return Ok(Returns::Always);
            }
        }
        Ok(Returns::NotAlways)
    }

    fn check_stmt(
        &mut self,
        stmt: &Stmt,
        surrounding_fn_return_type: TypeId,
    ) -> Result<Returns, Error> {
        match stmt {
            Stmt::Expr(expr) => {
                self.infer_expr(expr)?;
            }
            Stmt::Let(let_stmt) => self.check_let(let_stmt)?,
            Stmt::Return(ret_stmt) => {
                self.check_return_stmt(ret_stmt, surrounding_fn_return_type)?;
                return Ok(Returns::Always);
            }
            Stmt::Assignment(assignment) => {
                self.check_assignment(assignment)?;
            }
            Stmt::If(if_stmt) => {
                if let Returns::Always = self.check_if(if_stmt, surrounding_fn_return_type)? {
                    return Ok(Returns::Always);
                }
            }
            Stmt::For(for_stmt) => {
                self.check_for(for_stmt, surrounding_fn_return_type)?;
            }
        }
        Ok(Returns::NotAlways)
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
                // We know the function exists, so
                // we can unwrap.
                let lookup = self.lookup(fn_call.id).unwrap();
                let span = self.var_span(fn_call.id);
                let fn_resolved = self.as_resolved(&self.types[self.resolve(lookup)].clone());
                self.unify(fn_type, lookup)
                    .map_err(|e| e.with_label(span, format!("has type `{fn_resolved}`")))?;
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
            super::Atom::List(list) => Ok(self.infer_list(list)?),
        }
    }

    fn infer_list(&mut self, list: &super::List) -> Result<TypeId, Error> {
        let inner_ty: Option<TypeId> = list
            .items
            .first()
            .map(|item| self.infer_expr(item))
            .transpose()?;
        if list.items.len() > 1 {
            for item in list.items[1..].iter() {
                let expr_ty = self.infer_expr(item)?;
                self.unify(inner_ty.unwrap(), expr_ty)?;
            }
        }
        let inner_ty = inner_ty.unwrap_or(self.add_type(Type::Var));
        Ok(self.add_type(Type::List(inner_ty)))
    }

    fn check_let(&mut self, let_stmt: &super::LetStmt) -> Result<(), Error> {
        let rhs = let_stmt
            .rhs
            .as_ref()
            .map(|rhs| self.infer_expr(rhs))
            .transpose()?;
        let type_ = if let Some(ref type_) = let_stmt.type_ {
            self.convert_raw_type(type_)
        } else {
            Type::Var
        };
        let type_id = self.infer_let_lhs(&let_stmt.lhs, type_)?;
        if let Some(rhs) = rhs {
            let span = match &let_stmt.lhs {
                super::LetLhs::Var(var_id) => self.var_span(*var_id),
                super::LetLhs::Pat(_) => {
                    // No single variable to label for pattern bindings
                    self.unify(type_id, rhs)?;
                    return Ok(());
                }
            };
            let lhs_type = self.as_resolved(&self.types[self.resolve(type_id)].clone());
            self.unify(type_id, rhs)
                .map_err(|e| e.with_label(span, format!("expected `{lhs_type}`")))?;
        }
        Ok(())
    }

    fn infer_let_lhs(&mut self, lhs: &super::LetLhs, type_: Type) -> Result<TypeId, Error> {
        match lhs {
            super::LetLhs::Var(var_id) => self.add_var(*var_id, type_),
            super::LetLhs::Pat(pat_id) => {
                let pat = &self.pats[*pat_id];
                for (var, _) in pat.vars.iter() {
                    self.add_var(*var, Type::Var)?;
                }
                self.add_pat(*pat_id, type_)
            }
        }
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
            .unwrap_or_else(|| Ok(self.add_type(Type::Unit)))?;
        self.unify(fn_return_type, expr_type)
    }

    fn check_if(
        &mut self,
        if_stmt: &super::If,
        surrounding_fn_return_type: TypeId,
    ) -> Result<Returns, Error> {
        let mut returns = Returns::Always;
        for branch in if_stmt.if_branches.iter() {
            let ty = self.infer_expr(&branch.0)?;
            let bool_ty = self.add_type(Type::Bool);
            self.unify(ty, bool_ty)?;

            returns.add_branch(self.check_block(&branch.1, surrounding_fn_return_type)?);
        }
        if let Some(else_branch) = &if_stmt.else_branch {
            returns.add_branch(self.check_block(else_branch, surrounding_fn_return_type)?);
        } else {
            returns = Returns::NotAlways;
        }
        Ok(returns)
    }

    fn check_for(
        &mut self,
        for_: &super::For,
        surrounding_fn_return_type: TypeId,
    ) -> Result<(), Error> {
        let iterable_type = self.infer_expr(&for_.iterable)?;
        let item_type_id = self.get_item_type(iterable_type)?;
        let var_type = self.infer_let_lhs(&for_.lhs, self.types[item_type_id].clone())?;
        self.unify(var_type, item_type_id)?;
        self.check_block(&for_.stmts, surrounding_fn_return_type)?;
        Ok(())
    }

    fn get_item_type(&self, id: TypeId) -> Result<TypeId, Error> {
        let ty = &self.types[id];
        match ty {
            Type::Var
            | Type::Kind(_)
            | Type::Int
            | Type::Bool
            | Type::Str
            | Type::Unit
            | Type::Fun(_, _) => Err(Error::type_not_iterable(self.as_resolved(ty))),
            Type::List(inner) => Ok(*inner),
        }
    }

    fn check_assignment(&mut self, a: &super::Assignment) -> Result<(), Error> {
        let lhs = self.lookup(a.lhs).unwrap(); // Resolver makes sure this exists
        let rhs = self.infer_expr(&a.rhs)?;
        let span = self.var_span(a.lhs);
        let lhs_type = self.as_resolved(&self.types[self.resolve(lhs)].clone());
        self.unify(lhs, rhs)
            .map_err(|e| e.with_label(span, format!("has type `{lhs_type}`")))
    }

    fn unify(&mut self, t1: TypeId, t2: TypeId) -> Result<(), Error> {
        let t1 = self.resolve(t1);
        let t2 = self.resolve(t2);
        if t1 == t2 {
            return Ok(());
        }
        let type1 = self.types[t1].clone();
        let type2 = self.types[t2].clone();
        let make_error = || {
            Err(Error::type_mismatch(
                self.as_resolved(&type1),
                self.as_resolved(&type2),
            ))
        };
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
            (Type::List(ty1), Type::List(ty2)) => {
                self.unify(*ty1, *ty2)?;
            }
            (Type::Fun(args1, ret1), Type::Fun(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return make_error();
                }
                for (arg1, arg2) in args1.iter().zip(args2) {
                    self.unify(*arg1, *arg2)?;
                }
                self.unify(*ret1, *ret2)?;
            }
            // Write out the remaining types here instead of
            // a catchall, so we notices it when adding variants.
            (Type::Int, _)
            | (Type::Bool, _)
            | (Type::Str, _)
            | (Type::Unit, _)
            | (Type::Kind(_), _)
            | (Type::List(_), _)
            | (Type::Fun(_, _), _) => {
                return make_error();
            }
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
            BuiltinFn::Assert => mk_fn_type(vec![Type::Bool], Type::Unit),
            BuiltinFn::Print | BuiltinFn::Dbg => {
                let t = self.add_type(Type::Var);
                let add_type = self.add_type(Type::Unit);
                let type_id = self.add_type(Type::Fun(vec![t], add_type));
                VarType::Scheme(Scheme {
                    type_id,
                    generalized: vec![t],
                })
            }
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
        let mut new_type = self.types[scheme.type_id].clone();
        for var in scheme.generalized.iter() {
            let new_var = self.add_type(Type::Var);
            new_type.substitute(*var, new_var);
        }
        self.add_type(new_type)
    }
}
