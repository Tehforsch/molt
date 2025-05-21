use std::path::Path;

use crate::ctx::{AstCtx, ConvertCtx};

pub(crate) struct Ast {
    pub ctx: AstCtx,
}

impl Ast {
    pub(crate) fn parse(path: &Path) -> Ast {
        let ast = syn::parse_file(&std::fs::read_to_string(path).unwrap()).unwrap();
        let mut ctx = AstCtx::default();
        for item in ast.items.into_iter() {
            ctx.add_convert(item);
        }
        Ast { ctx }
    }
}
