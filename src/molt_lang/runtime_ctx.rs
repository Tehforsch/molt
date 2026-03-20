use codespan_reporting::files::Files;

use crate::{
    Config, Ctx, FileId, Input, Span,
    modify::RealNodeRef,
    molt_lang::{ParsedPat, PatId, type_definitions::TypeDefinitions},
    rust_grammar::Node,
    storage::Storage,
    writer::Writer,
};

#[derive(Clone)]
pub struct RuntimeCtx<'a> {
    pub real_id: FileId,
    pub molt_id: FileId,
    pub real_ctx: &'a Ctx<Node>,
    pub input: &'a Input,
    pub writer: &'a Writer,
    pub config: &'a Config,

    pub pats: &'a Storage<PatId, ParsedPat>,

    pub type_defs: &'a TypeDefinitions,
}

impl RuntimeCtx<'_> {
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

    pub(crate) fn get_pat(&self, id: PatId) -> &ParsedPat {
        &self.pats[id]
    }

    pub(crate) fn get_span(&self, new: &RealNodeRef) -> Span {
        match new {
            RealNodeRef::Real(id) => self.real_ctx.get_span(*id),
            RealNodeRef::List(ids) => {
                if ids.is_empty() {
                    todo!()
                }
                let mut span = self.get_span(&ids[0]);
                for id in &ids[1..] {
                    span = span.join(self.get_span(id));
                }
                span
            }
        }
    }

    pub(crate) fn molt_code(&self) -> &str {
        self.input.source(self.molt_id).unwrap()
    }

    pub(crate) fn real_code(&self) -> &str {
        self.input.source(self.real_id).unwrap()
    }
}
