use super::{Span, tokenizer::TokenKind};

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    span: Span,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    TokenExpected(TokenKind),
    InvalidNodeKind,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO
        match self.kind {
            ParseErrorKind::TokenExpected(_) => write!(f, "Wrong token."),
            ParseErrorKind::InvalidNodeKind => write!(f, "Invalid node kind."),
        }
    }
}
