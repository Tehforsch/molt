use crate::ctx::Id;

use super::tokenizer::Ident;

pub(crate) struct File {
    pub vars: Vec<VarDecl>,
    pub commands: Vec<Command>,
}

pub(crate) struct VarDecl {
    pub name: Var,
    // TODO make this optional and infer if possible.
    pub kind: Kind,
    pub node: Id,
}

pub(crate) struct Var(Ident);

pub(crate) enum Command {
    Match(Var),
}

pub(crate) enum Kind {}
