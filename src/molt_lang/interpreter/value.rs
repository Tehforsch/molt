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

#[derive(Debug)]
pub enum StmtValue {
    NoMatch,
    #[allow(unused)]
    Value(Value),
    Return(Value),
}
