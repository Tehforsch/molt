use codespan_reporting::files::Files;

use crate::{
    Config, Ctx, FileId, Input, Span,
    modify::NodeSpec,
    molt_lang::{PatId, ResolvedPat},
    rust_grammar::Node,
    storage::Storage,
    writer::Writer,
};

#[derive(Clone)]
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

    pub(crate) fn get_span(&self, new: NodeSpec) -> Span {
        match new {
            NodeSpec::Rust(id) => self.real_ctx.get_span(id),
            NodeSpec::MoltPat { pat: _ } => todo!(),
            NodeSpec::MoltVar { id: _, pat: _ } => todo!(),
        }
    }

    pub(crate) fn print(&self, new: NodeSpec) -> String {
        match new {
            NodeSpec::Rust(id) => self.real_ctx.print(id, self.real_code()).into(),
            NodeSpec::MoltPat { pat } => {
                let pat = &self.pats[pat];
                pat.ctx.print(pat.node, self.molt_code()).into()
            }
            NodeSpec::MoltVar { pat, id } => self.pats[pat].ctx.print(id, self.molt_code()).into(),
        }
    }

    fn molt_code(&self) -> &str {
        self.input.source(self.molt_id).unwrap()
    }

    fn real_code(&self) -> &str {
        self.input.source(self.real_id).unwrap()
    }
}
