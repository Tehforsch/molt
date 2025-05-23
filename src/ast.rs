use crate::{
    Error,
    ctx::AstCtx,
    input::RustSourceFile,
    parser::{Mode, Parse, Parser, RustFile, Tokenizer},
};

impl AstCtx {
    pub(crate) fn parse(file: &RustSourceFile) -> Result<Self, Error> {
        let contents = file.contents();
        let tokens = Tokenizer::tokenize_rust(contents)?;
        let parser = Parser::new(tokens, Mode::Rust);
        let (file, ctx) = parser.parse_file::<RustFile>()?;
        Ok(AstCtx::new(ctx))
    }
}
