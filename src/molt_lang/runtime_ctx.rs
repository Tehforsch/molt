use codespan_reporting::files::Files;

use crate::{
    Config, Ctx, FileId, Input, Span,
    change_buffer::ChangeBuffer,
    modify::NodeSpec,
    molt_lang::{ParsedPat, PatId},
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

    pub(crate) fn get_span(&self, new: &NodeSpec) -> Span {
        match new {
            NodeSpec::Real(id) => self.real_ctx.get_span(*id),
            NodeSpec::Molt {
                id: _,
                vars: _,
                pat: _,
            } => todo!(),
        }
    }

    pub(crate) fn print(&self, new: &NodeSpec) -> String {
        match new {
            NodeSpec::Real(id) => self.real_ctx.print(*id, self.real_code()).into(),
            NodeSpec::Molt { pat, id, vars } => {
                let ctx = &self.pats[*pat].ctx;
                let mut output =
                    ChangeBuffer::new_subspan(self.molt_code().into(), ctx.get_span(*id));
                assert_eq!(vars.len(), self.pats[*pat].vars.len());
                for (var, token_var) in vars.iter().zip(self.pats[*pat].vars.iter()) {
                    let span = token_var.span;
                    let new_code = self.print(var);
                    output.make_change(span, &new_code);
                }
                output.code()
            }
        }
    }

    fn molt_code(&self) -> &str {
        self.input.source(self.molt_id).unwrap()
    }

    fn real_code(&self) -> &str {
        self.input.source(self.real_id).unwrap()
    }
}
