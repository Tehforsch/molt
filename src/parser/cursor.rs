use super::{
    Peek, Span,
    tokenizer::{Token, TokenKind},
};

pub struct Cursor {
    tokens: Vec<Token>,
    index: usize,
    pos: usize,
}

impl Peek for Cursor {
    fn peek_next_token(&self) -> TokenKind {
        self.next().kind
    }

    fn peek_token(&self) -> TokenKind {
        self.current().kind
    }
}

fn eof(pos: usize) -> Token {
    Token {
        span: Span::new(pos, pos),
        kind: TokenKind::Eof,
    }
}

impl Cursor {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            pos: 0,
        }
    }

    pub fn advance(&mut self) -> Token {
        self.pos += 1;
        self.index += 1;
        self.current()
    }

    fn current(&self) -> Token {
        *self.tokens.get(self.index).unwrap_or(&eof(self.pos))
    }

    fn next(&self) -> Token {
        *self.tokens.get(self.index + 1).unwrap_or(&eof(self.pos))
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub(crate) fn char_pos(&self) -> usize {
        self.current().span.start()
    }

    pub(crate) fn current_token_span(&self) -> super::Span {
        self.current().span
    }
}
