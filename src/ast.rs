use std::path::Path;

use crate::{
    ctx::{AstCtx, Ctx, NodeId},
    grammar::Item,
};

pub(crate) struct Ast {
    pub ctx: AstCtx,
    pub items: Vec<NodeId<Item>>,
}

impl Ast {
    pub(crate) fn parse(path: &Path) -> Ast {
        let ast = syn::parse_file(&std::fs::read_to_string(path).unwrap()).unwrap();
        let mut ctx = Ctx::default();
        let items = ast
            .items
            .into_iter()
            .map(|item| ctx.add_convert(item))
            .collect();
        Ast {
            items,
            ctx: AstCtx::new(ctx),
        }
    }
}
