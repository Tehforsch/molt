use codespan_reporting::files::Files;

use crate::{Config, Ctx, FileId, Input, rust_grammar::Node, writer::Writer};

pub struct Context<'a> {
    pub real_id: FileId,
    pub molt_id: FileId,
    pub real_ctx: &'a Ctx<Node>,
    pub input: &'a Input,
    pub writer: &'a Writer,
    pub config: &'a Config,
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
}
