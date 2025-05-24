mod cursor;
mod error;
mod molt_grammar;
mod node;
mod parse;
mod rust_grammar;
#[cfg(test)]
mod tests;
mod tokenizer;

use cursor::Cursor;
use error::ParseError as Error;
use error::ParseErrorKind as ErrorKind;
use tokenizer::{Token, TokenKind};

pub use error::{ParseError, ParseErrorKind};
pub(crate) use molt_grammar::{Command, Decl, MoltFile, Var, VarDecl, VarId};
pub(crate) use node::{CustomDebug, Kind, Node, Pattern, ToNode};
pub(crate) use rust_grammar::RustFile;
pub(crate) use tokenizer::Tokenizer;
pub use tokenizer::{Span, TokenizerError};

pub use tokenizer::{Ident, Lit};

use crate::ctx::Ctx;

fn parse_file<T: Parse>(code: &str, mode: Mode) -> Result<(T, Ctx), crate::Error> {
    let tokens = Tokenizer::tokenize(code, mode)?;
    let mut parser = Parser::new(tokens, mode);
    Ok((parser.parse::<T>()?, parser.ctx))
}

pub fn parse_rust_file(code: &str) -> Result<(RustFile, Ctx), crate::Error> {
    parse_file::<RustFile>(code, Mode::Rust)
}

pub fn parse_molt_file(code: &str) -> Result<(MoltFile, Ctx), crate::Error> {
    parse_file::<MoltFile>(code, Mode::Molt)
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Copy, Clone)]
pub enum Mode {
    Molt,
    Rust,
}

pub(crate) trait Parse: Sized {
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
    node_positions: Vec<usize>,
    ctx: Ctx,
    mode: Mode,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, mode: Mode) -> Self {
        Self {
            cursor: Cursor::new(tokens),
            node_positions: vec![0],
            ctx: Ctx::default(),
            mode,
        }
    }

    fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }

    fn parse_spanned<T: Parse>(&mut self) -> Result<T> {
        self.push_node_pos();
        let parsed = self.parse::<T>()?;
        self.pop_node_pos();
        Ok(parsed)
    }

    fn push_node_pos(&mut self) {
        self.node_positions.push(self.cursor.pos());
    }

    fn pop_node_pos(&mut self) {
        self.node_positions.pop();
    }

    fn advance(&mut self) -> Token {
        self.cursor.advance()
    }

    fn consume_if_matches(&mut self, expected: TokenKind) -> bool {
        if self.peek() != expected {
            false
        } else {
            self.advance();
            true
        }
    }

    fn consume(&mut self, expected: TokenKind) -> Result<()> {
        if self.peek() != expected {
            self.token_error(ErrorKind::TokenExpected(expected))
        } else {
            self.advance();
            Ok(())
        }
    }

    fn consume_pat<T>(&mut self, f: impl Fn(TokenKind) -> Option<T>) -> Result<T> {
        if let Some(t) = f(self.peek()) {
            self.advance();
            Ok(t)
        } else {
            Err(self.make_error(ErrorKind::UnexpectedToken))
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek() == TokenKind::Eof
    }

    fn token_error(&mut self, kind: ErrorKind) -> Result<(), Error> {
        self.advance();
        Err(Error::new(
            kind,
            Span::new(
                self.cursor.pos(),
                self.cursor.pos() + self.cursor.current().len,
            ),
        ))
    }

    fn error(&self, kind: ErrorKind) -> Result<(), Error> {
        Err(self.make_error(kind))
    }

    fn make_error(&self, kind: ErrorKind) -> Error {
        Error::new(
            kind,
            Span::new(*self.node_positions.last().unwrap(), self.cursor.pos()),
        )
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
