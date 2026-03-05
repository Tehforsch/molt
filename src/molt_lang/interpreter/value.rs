use crate::Id;

#[derive(Clone, Debug)]
pub enum Value {
    #[allow(unused)]
    String(String),
    Node(Id),
    // TODO: Get rid of this once we have proper semantics
    Null,
}

pub enum StmtValue {
    NoMatch,
    #[allow(unused)]
    Value(Value),
}
