use crate::{
    NodeType,
    modify::NodeRef,
    molt_lang::{BuiltinFn, FnId, RuntimeCtx, typechecker::QualifiedType},
    node::Kinds,
    rust_grammar::Node,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    String(String),
    Int(i64),
    Bool(bool),
    Node(NodeRef),
    // TODO: Get rid of this once we have proper semantics
    Unit,
    UserFn(FnId),
    BuiltinFn(BuiltinFn),
    List(Vec<Value>),
}

#[derive(Debug)]
pub enum StmtValue {
    NoMatch,
    #[allow(unused)]
    Value(Value),
    Return(Value),
}

impl Value {
    // This method shouldnt really exist, once we store
    // type information about expressions and move it
    // into the interpreter, we can simply query for this.
    pub fn get_type(&self, ctx: &RuntimeCtx) -> QualifiedType {
        match self {
            Value::String(_) => QualifiedType::Str,
            Value::Int(_) => QualifiedType::Int,
            Value::Bool(_) => QualifiedType::Bool,
            Value::Node(NodeRef::Real(node)) => QualifiedType::Kind(Kinds::single(
                ctx.real_ctx.get::<Node>(*node).unwrap_item().node_kind(),
            )),
            Value::Node(_) => {
                todo!()
            }
            Value::Unit => QualifiedType::Unit,
            Value::List(values) => QualifiedType::List(Box::new(values[0].get_type(ctx))),
            Value::UserFn(_) => todo!(),
            Value::BuiltinFn(_) => todo!(),
        }
    }
}
