use crate::{
    modify::NodeSpec,
    molt_lang::{BuiltinFn, FnId},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    String(String),
    Int(i64),
    Bool(bool),
    Node(NodeSpec),
    // TODO: Get rid of this once we have proper semantics
    Unit,
    UserFn(FnId),
    BuiltinFn(BuiltinFn),
    List(Vec<Value>),
}

impl Value {
    pub(crate) fn is_comparable_to(&self, v: &Value) -> bool {
        match (self, v) {
            (Value::String(_), Value::String(_)) => true,
            (Value::Int(_), Value::Int(_)) => true,
            (Value::Bool(_), Value::Bool(_)) => true,
            (Value::Node(_), Value::Node(_)) => true,
            (Value::Unit, Value::Unit) => true,
            (Value::UserFn(_), Value::UserFn(_)) => true,
            (Value::BuiltinFn(_), Value::BuiltinFn(_)) => true,
            (Value::List(_), Value::List(_)) => true,
            (Value::String(_), _) => false,
            (Value::Int(_), _) => false,
            (Value::Bool(_), _) => false,
            (Value::Node(_), _) => false,
            (Value::Unit, _) => false,
            (Value::UserFn(_), _) => false,
            (Value::BuiltinFn(_), _) => false,
            (Value::List(_), _) => false,
        }
    }
}

#[derive(Debug)]
pub enum StmtValue {
    NoMatch,
    #[allow(unused)]
    Value(Value),
    Return(Value),
}
