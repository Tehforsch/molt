use super::{Span, tokenizer::TokenKind};

pub struct ParseError {
    kind: ParseErrorKind,
    span: Span,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

pub enum ParseErrorKind {
    TokenExpected(TokenKind),
}
