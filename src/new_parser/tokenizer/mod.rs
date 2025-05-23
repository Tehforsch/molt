mod token;

use std::ops::Range;

use rustc_lexer::strip_shebang;
use thiserror::Error;
use token::Token;

#[derive(Clone, Copy, Debug)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub(crate) fn from_range(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    pub(crate) fn range(self) -> Range<usize> {
        self.start..self.end
    }
}

pub struct Ident;

#[derive(Debug, Error)]
#[error("Error during tokenization")]
pub struct TokenizerError;

fn tokenize_file(source: &str) -> Result<Vec<Token>, TokenizerError> {
    // special case because rustc_lexer assumes the file
    // is nonempty
    if source.len() == 0 {
        return Ok(vec![]);
    }
    let start = strip_shebang(source).unwrap_or(0);
    let mut tokens = vec![];
    for token in rustc_lexer::tokenize(&source[start..]).map(Token::from_rustc_token) {
        match token {
            Ok(token) => tokens.extend(token),
            Err(e) => return Err(e),
        }
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use rustc_lexer::tokenize;
    use walkdir::WalkDir;

    fn get_source_files(path: &Path) -> Vec<PathBuf> {
        let mut src_files = vec![];
        for entry in WalkDir::new(path) {
            let entry = entry.map_err(|e| e).unwrap();
            let path = entry.path();
            let is_rust_src = path.extension().map_or(false, |ext| ext == "rs");
            if is_rust_src {
                src_files.push(path.to_owned());
            }
        }
        src_files
    }

    #[test]
    fn tokenize_stuff() {
        let mut path = std::env::current_dir()
            .unwrap()
            .parent()
            .unwrap()
            .join("crates")
            .join("rust");
        for file in get_source_files(&path).iter() {
            let file = path.join(file);
            let code = std::fs::read_to_string(&file).unwrap();
            super::tokenize_file(&code);
        }
    }
}
