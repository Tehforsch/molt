use crate::Id;

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Node(Id),
    // TODO: Get rid of this once we have proper semantics
    Null,
}

pub enum StmtValue {
    Return,
    Value(Value),
}
