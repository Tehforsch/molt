use crate::ctx::Id;

use super::tokenizer::Ident;

#[derive(Debug)]
pub(crate) struct MoltFile {
    pub vars: Vec<VarDecl>,
    pub commands: Vec<Command>,
    pub sorted: bool,
}

pub(crate) enum Decl {
    VarDecl(VarDecl),
    Command(Command),
}

#[derive(Debug)]
pub(crate) struct VarDecl {
    pub id: VarId,
    pub node: Option<Id>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct VarId(pub Id);

#[derive(Debug)]
pub(crate) struct Var(Ident);

#[derive(Debug)]
pub(crate) enum Command {
    Match(VarId),
}

impl VarId {
    pub(crate) fn new(id: Id) -> Self {
        Self(id)
    }
}

impl Var {
    pub(crate) fn new(ident: Ident) -> Self {
        Self(ident)
    }

    pub(crate) fn ident(&self) -> Ident {
        self.0
    }
}

#[derive(Debug)]
pub(crate) struct Todo;
