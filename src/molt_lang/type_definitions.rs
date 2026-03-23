use std::collections::HashMap;

use crate::{
    modify::NodeSpec,
    molt_lang::{RuntimeCtx, interpreter::Value, typechecker::QualifiedType},
    rust_grammar::NodeKind,
    typechecker_bug,
};

use super::{FieldAccess, typechecker::Type};

type FieldAccessFn = dyn Fn(&RuntimeCtx, Value) -> Value;

pub struct FieldDef {
    ty: Type,
    field_access_fn: Box<FieldAccessFn>,
}

pub struct TypeDef {
    fields: HashMap<String, FieldDef>,
}

impl TypeDef {
    pub(crate) fn get_field_type(&self, s: &str) -> Option<&Type> {
        self.fields.get(s).map(|f| &f.ty)
    }
}

pub struct TypeDefinitions {
    defs: HashMap<QualifiedType, TypeDef>,
}

impl TypeDefinitions {
    pub(crate) fn get(&self, ty: &QualifiedType) -> Option<&TypeDef> {
        self.defs.get(ty)
    }

    pub(crate) fn get_field_access_fn(
        &self,
        ctx: &RuntimeCtx,
        lhs: Value,
        fa: &FieldAccess,
    ) -> Value {
        let lhs_ty = lhs.get_type(ctx);
        let Some(def) = self.get(&lhs_ty) else {
            typechecker_bug!()
        };
        let Some(field) = def.fields.get(&fa.field.to_string()) else {
            typechecker_bug!()
        };
        (*field.field_access_fn)(ctx, lhs)
    }
}

fn get_fn_name(i: &RuntimeCtx, val: Value) -> Value {
    let Value::Node(NodeSpec::Real(val)) = val else {
        typechecker_bug!();
    };
    let f: &crate::rust_grammar::Item = i
        .real_ctx
        .get::<crate::rust_grammar::Item>(val)
        .unwrap_item();
    let crate::rust_grammar::Item::Fn(f) = f else {
        typechecker_bug!()
    };
    Value::Node(NodeSpec::Real(f.sig.ident.into()))
}

impl Default for TypeDefinitions {
    fn default() -> Self {
        let mut defs = HashMap::default();
        defs.insert(QualifiedType::Kind(NodeKind::Item), {
            let mut fields = HashMap::default();
            fields.insert(
                "name".into(),
                FieldDef {
                    ty: Type::Kind(NodeKind::Ident),
                    field_access_fn: Box::new(get_fn_name),
                },
            );
            TypeDef { fields }
        });
        Self { defs }
    }
}
