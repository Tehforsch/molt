use crate::ctx::{Id, NodeId};

use super::{Kind, tokenizer::Ident};

pub(crate) struct File {
    pub vars: Vec<VarDecl>,
    pub commands: Vec<Command>,
}

pub(crate) struct VarDecl {
    pub name: NodeId<Ident>,
    // TODO make this optional and infer if possible.
    pub kind: Kind,
    pub node: Id,
}

pub(crate) struct Var(NodeId<Ident>);

pub(crate) enum Command {
    Match(Var),
}
