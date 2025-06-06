use crate::{Ctx, GetKind, ToNode, Var};
use crate::{Id, Pattern};

use crate::match_pattern::PatType;

pub struct MatchCtx<Node: GetKind> {
    pub debug_print: bool,
    pub pat_ctx: Ctx<Node>,
    pub ast_ctx: Ctx<Node>,
    rust_src: String,
    molt_src: String,
}

impl<Node: GetKind> MatchCtx<Node> {
    pub fn new(
        pat_ctx: Ctx<Node>,
        ast_ctx: Ctx<Node>,
        rust_src: &str,
        molt_src: &str,
        debug_print: bool,
    ) -> Self {
        Self {
            debug_print,
            pat_ctx,
            ast_ctx,
            rust_src: rust_src.to_owned(),
            molt_src: molt_src.to_owned(),
        }
    }

    pub fn dump(&self) {
        if !self.debug_print {
            return;
        }
        println!("--------------------------------");
        for id in self.ast_ctx.iter() {
            let node = self.ast_ctx.get::<Node>(id).unwrap_real();
            let kind_str = format!("{:?}", node.kind());
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
            let kind_str = format!("{:?}", node.kind());
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

    pub fn print_ast<'a>(&'a self, id: Id) -> &'a str {
        self.ast_ctx.print(id, &self.rust_src)
    }

    pub fn print_pat<'a>(&'a self, id: Id) -> &'a str {
        self.pat_ctx.print(id, &self.molt_src)
    }

    pub fn get<T: ToNode<Node>>(&self, pat_id: Id, pat_type: PatType) -> Pattern<&T, Id> {
        match pat_type {
            PatType::FromAst => self.ast_ctx.get(pat_id),
            PatType::FromPat => self.pat_ctx.get(pat_id),
        }
    }

    pub fn get_var(&self, var: Id) -> &Var<Node> {
        self.pat_ctx.get_var(var)
    }

    pub fn get_kind(&self, pat_id: Id, pat_type: PatType) -> Node::Kind {
        match pat_type {
            PatType::FromAst => self.ast_ctx.get_kind(pat_id),
            PatType::FromPat => self.pat_ctx.get_kind(pat_id),
        }
    }
}
