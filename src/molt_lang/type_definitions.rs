use std::collections::HashMap;

use crate::{
    NodeType, ToNode,
    modify::NodeSpec,
    molt_lang::{RuntimeCtx, interpreter::Value, typechecker::QualifiedType},
    node::Kinds,
    rust_grammar::{Ident, ImplItemFn, ItemFn, Node, NodeKind},
    typechecker_bug,
};

use super::typechecker::Type;

// Trick the trait solver into allowing a clone of the needed function.
trait FieldAcc: Fn(&RuntimeCtx, Value) -> Value + Sync + Send {
    fn clone_box<'a>(&self) -> Box<dyn 'a + FieldAcc>
    where
        Self: 'a;
}

impl<F> FieldAcc for F
where
    F: Fn(&RuntimeCtx, Value) -> Value + Clone + Sync + Send,
{
    fn clone_box<'a>(&self) -> Box<dyn 'a + FieldAcc>
    where
        Self: 'a,
    {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn '_ + FieldAcc> {
    fn clone(&self) -> Self {
        (**self).clone_box()
    }
}

pub struct FieldDef {
    ty: Type,
    field_access_fn: Box<dyn FieldAcc>,
}

impl std::fmt::Debug for FieldDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FieldDef").field("ty", &self.ty).finish()
    }
}

#[derive(Debug)]
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

    fn merge(&mut self, k1: NodeKind, k2: NodeKind) {
        let entries1: Vec<_> = self
            .defs
            .extract_if(.., |(ty, _)| *ty == QualifiedType::Kind(Kinds::single(k1)))
            .collect();
        let entries2: Vec<_> = self
            .defs
            .extract_if(.., |(ty, _)| *ty == QualifiedType::Kind(Kinds::single(k2)))
            .collect();
        for ((ty1, e1), (ty2, e2)) in entries1.into_iter().zip(entries2) {
            let QualifiedType::Kind(ref kinds1) = ty1 else {
                unimplemented!()
            };
            let QualifiedType::Kind(ref kinds2) = ty2 else {
                unimplemented!()
            };
            let mut fields = HashMap::new();
            let ty = QualifiedType::Kind(kinds1.merge_with(kinds2));
            for ((field_name1, e1), (field_name2, e2)) in e1.fields.into_iter().zip(e2.fields) {
                assert_eq!(field_name1, field_name2);
                fields.insert(
                    field_name1,
                    FieldDef {
                        field_access_fn: Box::new(move |ctx, val| {
                            if val.get_type(ctx) == QualifiedType::Kind(Kinds::single(k1)) {
                                (e1.field_access_fn.clone())(ctx, val)
                            } else {
                                (e2.field_access_fn.clone())(ctx, val)
                            }
                        }),
                        ty: e1.ty,
                    },
                );
            }
            self.defs.push((ty, TypeDef { fields }));
        }
    }
}

fn get_defs_for_type<T: MoltFields<Node> + ToNode<Node>>() -> (QualifiedType, TypeDef) {
    (
        QualifiedType::Kind(Kinds::single(<T as ToNode<Node>>::node_kind())),
        T::get_fields(),
    )
}

impl Default for TypeDefinitions {
    fn default() -> Self {
        let defs = vec![
            get_defs_for_type::<ItemFn>(),
            get_defs_for_type::<ImplItemFn>(),
        ];
        let mut s = Self { defs };
        s.merge(NodeKind::ItemFn, NodeKind::ImplItemFn);
        s
    }
}

trait MoltFields<Node: NodeType> {
    fn get_fields() -> TypeDef;
}

impl MoltFields<Node> for crate::rust_grammar::ItemFn {
    fn get_fields() -> TypeDef {
        let mut fields = HashMap::default();

        fn get_fn_name(i: &RuntimeCtx, val: Value) -> Value {
            let Value::Node(NodeSpec::Real(val)) = val else {
                typechecker_bug!();
            };
            let f: &crate::rust_grammar::Node = i.real_ctx.get(val).unwrap_item();
            if let crate::rust_grammar::Node::ItemFn(f) = f {
                Value::Node(NodeSpec::Real(f.sig.ident.into()))
            } else {
                typechecker_bug!()
            }
        }

        fields.insert(
            "name".into(),
            FieldDef {
                ty: Type::Kind(Kinds::single(NodeKind::Ident)),
                field_access_fn: Box::new(get_fn_name),
            },
        );
        TypeDef { fields }
    }
}

impl MoltFields<Node> for crate::rust_grammar::ImplItemFn {
    fn get_fields() -> TypeDef {
        let mut fields = HashMap::default();

        fn get_fn_name(i: &RuntimeCtx, val: Value) -> Value {
            let Value::Node(NodeSpec::Real(val)) = val else {
                typechecker_bug!();
            };
            let f: &crate::rust_grammar::Node = i.real_ctx.get(val).unwrap_item();
            if let crate::rust_grammar::Node::ImplItemFn(f) = f {
                Value::Node(NodeSpec::Real(f.sig.ident.into()))
            } else {
                typechecker_bug!()
            }
        }

        fields.insert(
            "name".into(),
            FieldDef {
                ty: Type::Kind(Kinds::single(NodeKind::Ident)),
                field_access_fn: Box::new(get_fn_name),
            },
        );
        TypeDef { fields }
    }
}
