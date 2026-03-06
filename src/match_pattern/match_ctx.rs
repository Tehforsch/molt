use codespan_reporting::files::Files;

use crate::config::Config;
use crate::molt_lang::Context;
use crate::{Ctx, FileId, Id, Input, Mode, NodeType, Pattern, ToNode, Var, rust_grammar};

pub(super) struct MatchCtx<'a, Node: NodeType> {
    pub input: &'a Input,
    pub molt_ctx: &'a Ctx<Node>,
    pub real_ctx: &'a Ctx<Node>,
    pub molt_id: FileId,
    pub real_id: FileId,
    pub config: &'a Config,
}

impl<'a> MatchCtx<'a, rust_grammar::Node> {
    pub(crate) fn from_interpreter_ctx(
        context: &'a Context,
        ctx: &'a Ctx<crate::rust_grammar::Node>,
    ) -> MatchCtx<'a, rust_grammar::Node> {
        Self {
            molt_ctx: ctx,
            real_ctx: context.real_ctx,
            input: context.input,
            molt_id: context.molt_id,
            real_id: context.real_id,
            config: context.config,
        }
    }
}

impl<'a, Node: NodeType> MatchCtx<'a, Node> {
    pub fn config(&self) -> &Config {
        self.config
    }

    #[allow(unused)]
    pub fn dump(&self) {
        println!("--------------------------------");
        for id in self.real_ctx.iter() {
            let node = self.real_ctx.get::<Node>(id).unwrap_item();
            let kind_str = format!("{:?}", node.node_kind());
            println!(
                "RealNode({:02}): {:13} = {}",
                id.unwrap_idx(),
                kind_str,
                self.print_real(id)
            );
        }
        println!("--------------------------------");
        for id in self.molt_ctx.iter() {
            let node = self.molt_ctx.get::<Node>(id).unwrap_item();
            let kind_str = format!("{:?}", node.node_kind());
            println!(
                "MoltNode({:02}): {:13} = {}",
                id.unwrap_idx(),
                kind_str,
                self.print_molt(id)
            );
        }
        println!("--------------------------------");
        for (idx, var) in self.molt_ctx.iter_vars().enumerate() {
            println!("MoltVar({:02}): {:14} = {:?}", idx, "", var.name());
        }
    }

    fn print_real(&self, id: Id) -> &str {
        self.real_ctx.print(id, self.real_src())
    }

    fn print_molt(&self, id: Id) -> &str {
        if id.is_var() {
            self.molt_ctx.get_var(id).name()
        } else {
            self.molt_ctx.print(id, self.molt_src())
        }
    }

    pub fn print(&self, id: Id) -> &str {
        match id.mode() {
            Mode::Real => self.print_real(id),
            Mode::Molt => self.print_molt(id),
        }
    }

    pub fn get<T: ToNode<Node>>(&self, id: Id) -> Pattern<&T, Id> {
        match id.mode() {
            Mode::Real => self.real_ctx.get(id),
            Mode::Molt => self.molt_ctx.get(id),
        }
    }

    pub fn get_var(&self, var: Id) -> &Var<Node::Kind> {
        self.molt_ctx.get_var(var)
    }

    fn real_src(&self) -> &str {
        self.input.source(self.real_id).unwrap()
    }

    fn molt_src(&self) -> &str {
        self.input.source(self.molt_id).unwrap()
    }
}
