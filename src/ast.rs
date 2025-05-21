use std::path::Path;

use codespan_reporting::files::SimpleFile;

use crate::ctx::{AstCtx, ConvertCtx};

pub(crate) struct Ast {
    pub ctx: AstCtx,
    pub file: SimpleFile<String, String>,
}

impl Ast {
    pub(crate) fn parse(path: &Path) -> Ast {
        let contents = std::fs::read_to_string(path).unwrap();
        let ast = syn::parse_file(&contents).unwrap();
        let file = SimpleFile::new(format!("{:?}", path), contents);
        let mut ctx = AstCtx::default();
        for item in ast.items.into_iter() {
            ctx.add_convert(item);
        }
        Ast { ctx, file }
    }
}
