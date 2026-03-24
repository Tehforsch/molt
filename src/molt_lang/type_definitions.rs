use std::collections::HashMap;

use crate::{
    NodeId, ToNode,
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

    /// Merge field definitions for kinds that are very similar, such as
    /// `ItemFn` and `ImplItemFn`.
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
            let mut fields1: Vec<_> = e1.fields.into_iter().collect();
            let mut fields2: Vec<_> = e2.fields.into_iter().collect();
            fields1.sort_by_key(|(k, _)| k.clone()); // TODO: unnecessary clone
            fields2.sort_by_key(|(k, _)| k.clone()); // TODO: unnecessary clone
            for ((field_name1, e1), (field_name2, e2)) in fields1.into_iter().zip(fields2) {
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

fn get_defs_for_type<T: MoltFields + ToNode<Node>>() -> (QualifiedType, TypeDef) {
    let mut builder = FieldDefBuilder::default();
    T::add_fields(&mut builder);
    (
        QualifiedType::Kind(Kinds::single(<T as ToNode<Node>>::node_kind())),
        builder.build(),
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

trait MoltFields {
    fn add_fields(b: &mut FieldDefBuilder);
}

#[derive(Default)]
struct FieldDefBuilder {
    fields: HashMap<String, FieldDef>,
}

impl FieldDefBuilder {
    fn add<Struct: ToNode<Node>, Field: ToNode<Node>>(
        &mut self,
        name: &str,
        field_getter: impl Fn(&Struct) -> NodeId<Field> + Send + Sync + Clone + 'static,
    ) {
        self.fields.insert(
            name.into(),
            FieldDef {
                ty: Type::Kind(Kinds::single(Field::node_kind())),
                field_access_fn: Box::new(move |ctx, val| {
                    let Value::Node(NodeSpec::Real(val)) = val else {
                        typechecker_bug!();
                    };
                    let f: &crate::rust_grammar::Node = ctx.real_ctx.get(val).unwrap_item();
                    if let Some(item) = Struct::from_node_ref(f) {
                        Value::Node(NodeSpec::Real(field_getter(item).into()))
                    } else {
                        typechecker_bug!()
                    }
                }),
            },
        );
    }

    fn build(self) -> TypeDef {
        TypeDef {
            fields: self.fields,
        }
    }
}

impl MoltFields for crate::rust_grammar::ItemFn {
    fn add_fields(builder: &mut FieldDefBuilder) {
        builder.add("name", |f: &Self| f.sig.ident);
        builder.add("vis", |f: &Self| f.vis);
    }
}

impl MoltFields for crate::rust_grammar::ImplItemFn {
    fn add_fields(builder: &mut FieldDefBuilder) {
        builder.add("name", |f: &Self| f.sig.ident);
        builder.add("vis", |f: &Self| f.vis);
    }
}
