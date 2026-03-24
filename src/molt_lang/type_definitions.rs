use std::collections::HashMap;

use crate::{
    modify::NodeSpec,
    molt_lang::{RuntimeCtx, interpreter::Value, typechecker::QualifiedType},
    node::Kinds,
    rust_grammar::{Ident, NodeKind},
    typechecker_bug,
};

use super::typechecker::Type;

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
    defs: Vec<(QualifiedType, TypeDef)>,
}

impl TypeDefinitions {
    pub(crate) fn get(&self, ty: &QualifiedType) -> Option<&TypeDef> {
        for (def_ty, ty_def) in self.defs.iter() {
            if def_ty.is_super_type(ty) {
                return Some(ty_def);
            }
        }
        #[allow(clippy::needless_return)]
        return None;
    }

    pub(crate) fn get_field_access_fn(
        &self,
        ctx: &RuntimeCtx,
        lhs: Value,
        field_name: &Ident,
    ) -> Value {
        let lhs_ty = lhs.get_type(ctx);
        let Some(def) = self.get(&lhs_ty) else {
            typechecker_bug!()
        };
        let Some(field) = def.fields.get(&field_name.to_string()) else {
            typechecker_bug!()
        };
        (*field.field_access_fn)(ctx, lhs)
    }
}

fn get_fn_name(i: &RuntimeCtx, val: Value) -> Value {
    let Value::Node(NodeSpec::Real(val)) = val else {
        typechecker_bug!();
    };
    let f: &crate::rust_grammar::Node = i.real_ctx.get(val).unwrap_item();
    match f {
        crate::rust_grammar::Node::ItemFn(f) => Value::Node(NodeSpec::Real(f.sig.ident.into())),
        crate::rust_grammar::Node::ImplItemFn(f) => Value::Node(NodeSpec::Real(f.sig.ident.into())),
        _ => typechecker_bug!(),
    }
}

impl Default for TypeDefinitions {
    fn default() -> Self {
        let mut defs = vec![];
        defs.push((
            QualifiedType::Kind(Kinds::new(vec![NodeKind::ItemFn, NodeKind::ImplItemFn])),
            {
                let mut fields = HashMap::default();
                fields.insert(
                    "name".into(),
                    FieldDef {
                        ty: Type::Kind(Kinds::single(NodeKind::Ident)),
                        field_access_fn: Box::new(get_fn_name),
                    },
                );
                TypeDef { fields }
            },
        ));
        Self { defs }
    }
}
