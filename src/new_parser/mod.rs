mod cursor;
mod error;
mod molt_grammar;
mod node;
mod parse;
mod rust_grammar;
mod tokenizer;

use cursor::Cursor;
use error::ParseError as Error;
use error::ParseErrorKind as ErrorKind;
use tokenizer::{Token, TokenKind};

pub use error::{ParseError, ParseErrorKind};
pub use tokenizer::{Span, TokenizerError};

use crate::ctx::Ctx;

pub type Result<T, E = Error> = std::result::Result<T, E>;

trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

trait Matches: Sized {
    fn matches(p: &impl Peek) -> bool;
}

trait Peek: Sized {
    fn peek(&self) -> TokenKind;
    fn peek_next(&self) -> TokenKind;

    fn matches<T: Matches>(&self) -> bool {
        T::matches(self)
    }

    fn token_matches(&self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    fn next_token_matches(&self, kind: TokenKind) -> bool {
        self.peek_next() == kind
    }

    fn parse_from_peek<T: FromPeek>(&self) -> Option<T> {
        T::from_peek(self)
    }
}

trait FromPeek: Sized {
    fn from_peek(p: &impl Peek) -> Option<Self>;
}

trait Delimiter: Default {
    fn start() -> TokenKind;
    fn end() -> TokenKind;
}

pub struct Parser {
    cursor: Cursor,
    current_node_start: usize,
    ctx: Ctx,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            cursor: Cursor::new(tokens),
            current_node_start: 0,
            ctx: Ctx::default(),
        }
    }

    fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }

    fn advance(&mut self) -> Token {
        self.cursor.advance()
    }

    fn consume_if_matches(&mut self, expected: TokenKind) -> bool {
        self.consume(expected).is_ok()
    }

    fn consume(&mut self, expected: TokenKind) -> Result<()> {
        if self.peek() != expected {
            self.error(ErrorKind::TokenExpected(expected))
        } else {
            self.advance();
            Ok(())
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek() == TokenKind::Eof
    }

    fn error(&self, kind: ErrorKind) -> Result<(), Error> {
        Err(self.make_error(kind))
    }

    fn make_error(&self, kind: ErrorKind) -> Error {
        Error::new(kind, Span::new(self.current_node_start, self.cursor.pos()))
    }
}

impl Peek for Parser {
    fn peek(&self) -> TokenKind {
        self.cursor.peek()
    }

    fn peek_next(&self) -> TokenKind {
        self.cursor.peek_next()
    }
}
