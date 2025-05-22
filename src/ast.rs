use crate::{
    Error,
    ctx::{AstCtx, ConvertCtx},
    input::RustSourceFile,
};

impl AstCtx {
    pub(crate) fn parse(file: &RustSourceFile) -> Result<Self, Error> {
        let contents = file.contents();
        let ast = syn::parse_file(&contents).map_err(|e| Error::Parse(e))?;
        let mut ctx = AstCtx::default();
        for item in ast.items.into_iter() {
            ctx.add_convert(item);
        }
        Ok(ctx)
    }
}
