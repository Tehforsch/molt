mod parse;

use crate::ctx::Id;

use super::{Kind, UserKind, rust_grammar::Ident};

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
pub(crate) struct Var {
    ident: Ident,
    kind: Kind,
}

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
    pub(crate) fn new(ident: Ident, user_kind: UserKind) -> Self {
        Self {
            ident,
            kind: user_kind.to_kind(),
        }
    }

    pub(crate) fn ident(&self) -> &Ident {
        &self.ident
    }

    pub(crate) fn ident_mut(&mut self) -> &mut Ident {
        &mut self.ident
    }

    pub(crate) fn kind(&self) -> Kind {
        self.kind
    }
}

#[derive(Debug)]
pub(crate) struct Todo;

pub(crate) struct UntypedVar(Ident);

impl UntypedVar {
    pub(crate) fn to_var(self, user_kind: UserKind) -> Var {
        Var {
            kind: user_kind.to_kind(),
            ident: self.0,
        }
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.ident())
    }
}
