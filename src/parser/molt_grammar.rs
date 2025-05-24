use crate::ctx::Id;

use super::{Kind, tokenizer::Ident};

pub(crate) struct MoltFile {
    pub vars: Vec<VarDecl>,
    pub commands: Vec<Command>,
    pub sorted: bool,
}

pub(crate) struct VarDecl {
    pub id: VarId,
    // TODO make this optional and infer if possible.
    pub kind: Kind,
    pub node: Option<Id>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct VarId(pub Id);

#[derive(Debug)]
pub(crate) struct Var(Ident);

pub(crate) enum Command {
    Match(VarId),
}

impl VarId {
    pub(crate) fn new(id: Id) -> Self {
        Self(id)
    }
}

impl Var {
    pub(crate) fn ident(&self) -> Ident {
        self.0
    }
}
