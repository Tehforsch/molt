use std::path::Path;

use crate::config::Config;
use crate::{Ctx, Id, Mode, NodeType, Pattern, ToNode, Var};

#[derive(Clone)]
pub struct MatchPatternData<'a> {
    pub rust_src: &'a str,
    pub molt_src: &'a str,
    pub rust_path: &'a Path,
    pub config: &'a Config,
}

pub struct MatchCtx<'a, Node: NodeType> {
    pub pat_ctx: &'a Ctx<Node>,
    pub ast_ctx: &'a Ctx<Node>,
    data: MatchPatternData<'a>,
}

impl<'a, Node: NodeType> MatchCtx<'a, Node> {
    pub fn new(pat_ctx: &'a Ctx<Node>, ast_ctx: &'a Ctx<Node>, data: MatchPatternData<'a>) -> Self {
        Self {
            pat_ctx,
            ast_ctx,
            data,
        }
    }

    pub fn config(&self) -> &Config {
        self.data.config
    }

    pub fn dump(&self) {
        println!("--------------------------------");
        for id in self.ast_ctx.iter() {
            let node = self.ast_ctx.get::<Node>(id).unwrap_real();
            let kind_str = format!("{:?}", node.node_kind());
            println!(
                "AstNode({:02}): {:13} = {}",
                id.unwrap_idx(),
                kind_str,
                self.print_ast(id)
            );
        }
        println!("--------------------------------");
        for id in self.pat_ctx.iter() {
            let node = self.pat_ctx.get::<Node>(id).unwrap_real();
            let kind_str = format!("{:?}", node.node_kind());
            println!(
                "PatNode({:02}): {:13} = {}",
                id.unwrap_idx(),
                kind_str,
                self.print_pat(id)
            );
        }
        println!("--------------------------------");
        for (idx, var) in self.pat_ctx.iter_vars().enumerate() {
            println!("PatVar({:02}): {:14} = {:?}", idx, "", var.name());
        }
    }

    fn print_ast(&self, id: Id) -> &str {
        self.ast_ctx.print(id, self.data.rust_src)
    }

    fn print_pat(&self, id: Id) -> &str {
        if id.is_var() {
            self.pat_ctx.get_var(id).name()
        } else {
            self.pat_ctx.print(id, self.data.molt_src)
        }
    }

    pub fn print(&self, id: Id) -> &str {
        match id.mode() {
            Mode::Real => self.print_ast(id),
            Mode::Molt => self.print_pat(id),
        }
    }

    pub fn get<T: ToNode<Node>>(&self, id: Id) -> Pattern<&T, Id> {
        match id.mode() {
            Mode::Real => self.ast_ctx.get(id),
            Mode::Molt => self.pat_ctx.get(id),
        }
    }

    pub fn get_var(&self, var: Id) -> &Var<Node::Kind> {
        self.pat_ctx.get_var(var)
    }
}
