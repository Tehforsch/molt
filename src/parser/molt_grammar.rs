use crate::{
    Error,
    ctx::{Id, NodeId},
    error::ResolveError,
};

use super::{Kind, tokenizer::Ident};

pub(crate) struct MoltFile {
    pub vars: Vec<VarDecl>,
    pub commands: Vec<Command>,
}

pub(crate) struct VarDecl {
    pub name: VarId,
    // TODO make this optional and infer if possible.
    pub kind: Kind,
    pub node: Option<Id>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct VarId(NodeId<Ident>);

pub(crate) struct Var(Ident);

pub(crate) enum Command {
    Match(VarId),
}

impl MoltFile {
    pub(crate) fn get_command(&self) -> Result<&Command, Error> {
        if self.commands.len() > 1 {
            return Err(Error::Resolve(ResolveError::MultipleCommandGiven));
        }
        self.commands
            .get(0)
            .ok_or_else(|| Error::Resolve(ResolveError::NoCommandGiven))
    }
}
