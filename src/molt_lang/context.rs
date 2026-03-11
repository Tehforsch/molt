use codespan_reporting::files::Files;

use crate::{
    Config, Ctx, FileId, Input,
    molt_lang::{PatId, ResolvedPat},
    rust_grammar::Node,
    storage::Storage,
    writer::Writer,
};

pub struct Context<'a> {
    pub real_id: FileId,
    pub molt_id: FileId,
    pub real_ctx: &'a Ctx<Node>,
    pub input: &'a Input,
    pub writer: &'a Writer,
    pub config: &'a Config,

    pub pats: &'a Storage<PatId, ResolvedPat>,
}

impl Context<'_> {
    pub fn real_ctx(&self) -> &'_ Ctx<Node> {
        self.real_ctx
    }

    pub fn real_src(&self) -> &'_ str {
        self.input.source(self.real_id).unwrap()
    }

    pub(crate) fn writer(&self) -> &'_ Writer {
        self.writer
    }

    pub(crate) fn input(&self) -> &Input {
        self.input
    }

    pub(crate) fn get_pat(&self, id: PatId) -> &ResolvedPat {
        &self.pats[id]
    }
}
