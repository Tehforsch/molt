use syn::File;

/// Represents a real Rust AST.
pub struct RustAst {
    pub file: File,
}

impl RustAst {
    pub fn new(file: File) -> Self {
        Self { file }
    }

    pub fn dump(self) -> String {
        prettyplease::unparse(&self.file)
    }
}
