mod token;

use std::ops::Range;

use rustc_lexer::strip_shebang;
use thiserror::Error;
use token::LiteralKind;
pub use token::{Keyword, Token, TokenKind};

use super::Mode;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub(crate) fn from_range(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    pub(crate) fn range(self) -> Range<usize> {
        self.start..self.end
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Ident(Span);

impl Ident {
    pub(crate) fn new(span: Span) -> Self {
        Self(span)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Lit {
    kind: LiteralKind,
    span: Span,
}

impl Lit {
    pub(crate) fn new(lit: LiteralKind, span: Span) -> Self {
        Self { kind: lit, span }
    }
}

#[derive(Debug, Error)]
#[error("Error during tokenization")]
pub struct TokenizerError;

pub struct Tokenizer<'a> {
    source: &'a str,
    position: usize,
    tokens: Vec<Token>,
    mode: Mode,
}

impl<'a> Tokenizer<'a> {
    #[cfg(test)]
    pub fn tokenize_rust(source: &str) -> Result<Vec<Token>, TokenizerError> {
        Self::tokenize(source, Mode::Rust)
    }

    #[cfg(test)]
    pub fn tokenize_molt(source: &str) -> Result<Vec<Token>, TokenizerError> {
        Self::tokenize(source, Mode::Molt)
    }

    pub fn tokenize(source: &str, mode: Mode) -> Result<Vec<Token>, TokenizerError> {
        // special case because rustc_lexer assumes the file
        // is nonempty
        if source.len() == 0 {
            return Ok(vec![]);
        }
        let start = strip_shebang(source).unwrap_or(0);
        let source = &source[start..];
        let mut tokenizer = Tokenizer {
            source,
            position: 0,
            tokens: vec![],
            mode,
        };
        for token in rustc_lexer::tokenize(&source) {
            tokenizer.consume(token)?;
        }
        Ok(tokenizer.tokens)
    }

    fn consume(&mut self, token: rustc_lexer::Token) -> Result<(), TokenizerError> {
        let len = token.len;
        let span = Span::new(self.position, self.position + len);
        match TokenKind::from_rustc_token_kind(&self.source[span.range()], self.mode, token.kind) {
            Ok(kind) => {
                self.tokens.extend(kind.map(|kind| Token { kind, span }));
            }
            Err(e) => return Err(e),
        }
        self.position += len;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use walkdir::WalkDir;

    use crate::parser::tokenizer::{Keyword, TokenKind};

    use super::Tokenizer;

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
        let path = std::env::current_dir()
            .unwrap()
            .parent()
            .unwrap()
            .join("crates")
            .join("rust");
        for file in get_source_files(&path).iter().take(100) {
            let file = path.join(file);
            let code = std::fs::read_to_string(&file).unwrap();
            Tokenizer::tokenize_rust(&code).unwrap();
        }
    }

    #[test]
    fn check_kw_and_ident() {
        let tokens = Tokenizer::tokenize_rust("foo bar async fn Ident transform").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident);
        assert_eq!(tokens[1].kind, TokenKind::Ident);
        assert_eq!(tokens[2].kind, TokenKind::Keyword(Keyword::Async));
        assert_eq!(tokens[3].kind, TokenKind::Keyword(Keyword::Fn));
        assert_eq!(tokens[4].kind, TokenKind::Ident);
        assert_eq!(tokens[5].kind, TokenKind::Ident);

        let tokens = Tokenizer::tokenize_molt("foo bar async fn Ident transform").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident);
        assert_eq!(tokens[1].kind, TokenKind::Ident);
        assert_eq!(tokens[2].kind, TokenKind::Keyword(Keyword::Async));
        assert_eq!(tokens[3].kind, TokenKind::Keyword(Keyword::Fn));
        assert_eq!(tokens[4].kind, TokenKind::Keyword(Keyword::Ident));
        assert_eq!(tokens[5].kind, TokenKind::Keyword(Keyword::Transform));
    }
}
