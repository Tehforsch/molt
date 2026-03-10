use crate::Id;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    #[allow(unused)]
    String(String),
    Int(i64),
    Bool(bool),
    Node(Id),
    // TODO: Get rid of this once we have proper semantics
    Unit,
}

impl Value {
    pub(crate) fn is_comparable_to(&self, v: &Value) -> bool {
        match (self, v) {
            (Value::String(_), Value::String(_)) => true,
            (Value::Int(_), Value::Int(_)) => true,
            (Value::Bool(_), Value::Bool(_)) => true,
            (Value::Node(_), Value::Node(_)) => true,
            (Value::Unit, Value::Unit) => true,
            (Value::String(_), _) => false,
            (Value::Int(_), _) => false,
            (Value::Bool(_), _) => false,
            (Value::Node(_), _) => false,
            (Value::Unit, _) => false,
        }
    }
}

pub enum StmtValue {
    NoMatch,
    #[allow(unused)]
    Value(Value),
}
