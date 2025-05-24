use super::{
    Peek,
    tokenizer::{Token, TokenKind},
};

pub struct Cursor {
    tokens: Vec<Token>,
    index: usize,
    pos: usize,
}

impl Peek for Cursor {
    fn peek_next(&self) -> TokenKind {
        self.next().kind
    }

    fn peek(&self) -> TokenKind {
        self.current().kind
    }
}

fn eof() -> Token {
    Token {
        len: 1,
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
        self.pos += self.current().len;
        self.index += 1;
        self.current()
    }

    pub(super) fn current(&self) -> Token {
        *self.tokens.get(self.index).unwrap_or(&eof())
    }

    fn next(&self) -> Token {
        *self.tokens.get(self.index + 1).unwrap_or(&eof())
    }

    pub fn pos(&self) -> usize {
        self.pos
    }
}
