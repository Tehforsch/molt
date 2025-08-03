use std::path::Path;

use crate::config::Config;
use crate::{Ctx, Id, Mode, NodeType, Pattern, ToNode, Var};

#[derive(Clone)]
pub struct MatchPatternData<'a> {
    pub real_src: &'a str,
    pub molt_src: &'a str,
    pub real_path: &'a Path,
    pub config: &'a Config,
}

pub struct MatchCtx<'a, Node: NodeType> {
    pub molt_ctx: &'a Ctx<Node>,
    pub real_ctx: &'a Ctx<Node>,
    data: MatchPatternData<'a>,
}

impl<'a, Node: NodeType> MatchCtx<'a, Node> {
    pub fn new(
        molt_ctx: &'a Ctx<Node>,
        real_ctx: &'a Ctx<Node>,
        data: MatchPatternData<'a>,
    ) -> Self {
        Self {
            molt_ctx,
            real_ctx,
            data,
        }
    }

    pub fn config(&self) -> &Config {
        self.data.config
    }

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
        self.real_ctx.print(id, self.data.real_src)
    }

    fn print_molt(&self, id: Id) -> &str {
        if id.is_var() {
            self.molt_ctx.get_var(id).name()
        } else {
            self.molt_ctx.print(id, self.data.molt_src)
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
}
