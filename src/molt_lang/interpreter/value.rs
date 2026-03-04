use crate::Id;

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Node(Id),
}
