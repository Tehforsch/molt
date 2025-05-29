mod cursor;
mod error;
mod molt_grammar;
mod node;
mod parse;
mod rust_grammar;
#[cfg(test)]
mod tests;
pub mod token;
mod tokenizer;

use cursor::Cursor;
use error::ParseError as Error;
use error::ParseErrorKind as ErrorKind;
use token::IsToken;
use tokenizer::{Token, TokenKind};

pub use error::{ParseError, ParseErrorKind};
pub(crate) use molt_grammar::{Command, Decl, MoltFile, Todo, Var, VarDecl, VarId};
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
    fn peek_token(&self) -> TokenKind;
    fn peek_next_token(&self) -> TokenKind;

    fn peek<T: IsToken>(&self) -> bool {
        T::is_token(self.peek_token())
    }

    fn matches<T: Matches>(&self) -> bool {
        T::matches(self)
    }

    fn token_matches(&self, kind: TokenKind) -> bool {
        self.peek_token() == kind
    }

    fn next_token_matches(&self, kind: TokenKind) -> bool {
        self.peek_next_token() == kind
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
        self.node_positions.push(self.cursor.char_pos());
    }

    fn pop_node_pos(&mut self) {
        self.node_positions.pop();
    }

    fn advance(&mut self) -> Token {
        self.cursor.advance()
    }

    fn consume_if_matches(&mut self, expected: TokenKind) -> bool {
        if self.peek_token() != expected {
            false
        } else {
            self.advance();
            true
        }
    }

    fn consume(&mut self, expected: TokenKind) -> Result<()> {
        if self.peek_token() != expected {
            self.token_error(ErrorKind::TokenExpected(expected))
        } else {
            self.advance();
            Ok(())
        }
    }

    fn consume_pat<T>(&mut self, f: impl Fn(TokenKind) -> Option<T>) -> Result<T> {
        if let Some(t) = f(self.peek_token()) {
            self.advance();
            Ok(t)
        } else {
            Err(self.make_error(ErrorKind::UnexpectedToken))
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek_token() == TokenKind::Eof
    }

    fn token_error(&mut self, kind: ErrorKind) -> Result<(), Error> {
        Err(self.make_error(kind))
    }

    fn make_error(&mut self, kind: ErrorKind) -> Error {
        Error::new(kind, self.cursor.current_token_span())
    }

    fn lookahead1(&self) -> Lookahead1 {
        Lookahead1 {
            token: self.cursor.peek_next_token(),
        }
    }
}

impl Peek for Parser {
    fn peek_token(&self) -> TokenKind {
        self.cursor.peek_token()
    }

    fn peek_next_token(&self) -> TokenKind {
        self.cursor.peek_next_token()
    }
}

pub struct Lookahead1 {
    token: TokenKind,
}

impl Peek for Lookahead1 {
    fn peek_token(&self) -> TokenKind {
        self.token
    }

    fn peek_next_token(&self) -> TokenKind {
        unimplemented!()
    }
}
