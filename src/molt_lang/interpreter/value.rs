use crate::Id;

#[derive(Clone, Debug)]
pub enum Value {
    #[allow(unused)]
    String(String),
    Int(i64),
    Node(Id),
    // TODO: Get rid of this once we have proper semantics
    Unit,
}

pub enum StmtValue {
    NoMatch,
    #[allow(unused)]
    Value(Value),
}
