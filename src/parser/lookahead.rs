use std::cell::RefCell;

use proc_macro2::{Delimiter, Span};

use crate::parser::ParseCtx;
use crate::parser::buffer::Cursor;
use crate::parser::error::{self, Error};
use crate::parser::parse::{PeekPat, peek_pat};
use crate::parser::span::IntoSpans;
use crate::parser::token::Token;

/// Support for checking the next token in a stream to decide how to parse.
///
/// An important advantage over [`ParseStream::peek`] is that here we
/// automatically construct an appropriate error message based on the token
/// alternatives that get peeked. If you are producing your own error message,
/// go ahead and use `ParseStream::peek` instead.
pub struct Lookahead1<'a> {
    scope: Span,
    cursor: Cursor<'a>,
    comparisons: RefCell<Vec<&'static str>>,
    ctx: ParseCtx,
}

pub(crate) fn new(scope: Span, cursor: Cursor, ctx: ParseCtx) -> Lookahead1 {
    Lookahead1 {
        scope,
        cursor,
        comparisons: RefCell::new(Vec::new()),
        ctx,
    }
}

fn peek_impl(
    lookahead: &Lookahead1,
    peek: fn(Cursor) -> bool,
    display: fn() -> &'static str,
) -> bool {
    if peek(lookahead.cursor) {
        return true;
    }
    lookahead.comparisons.borrow_mut().push(display());
    false
}

impl<'a> Lookahead1<'a> {
    /// Looks at the next token in the parse stream to determine whether it
    /// matches the requested type of token.
    pub fn peek<T: Peek>(&self, token: T) -> bool {
        let _ = token;
        peek_impl(self, T::Token::peek, T::Token::display)
    }

    pub fn peek_pat<T: PeekPat>(&self) -> bool {
        peek_pat::<T>(self.cursor, &self.ctx.borrow())
    }

    /// Triggers an error at the current position of the parse stream.
    ///
    /// The error message will identify all of the expected token types that
    /// have been peeked against this lookahead instance.
    pub fn error(self) -> Error {
        let mut comparisons = self.comparisons.into_inner();
        comparisons.retain_mut(|display| {
            if *display == "`)`" {
                *display = match self.cursor.scope_delimiter() {
                    Delimiter::Parenthesis => "`)`",
                    Delimiter::Brace => "`}`",
                    Delimiter::Bracket => "`]`",
                    Delimiter::None => return false,
                }
            }
            true
        });
        match comparisons.len() {
            0 => {
                if self.cursor.eof() {
                    Error::new(self.scope, "unexpected end of input")
                } else {
                    Error::new(self.cursor.span(), "unexpected token")
                }
            }
            1 => {
                let message = format!("expected {}", comparisons[0]);
                error::new_at(self.scope, self.cursor, message)
            }
            2 => {
                let message = format!("expected {} or {}", comparisons[0], comparisons[1]);
                error::new_at(self.scope, self.cursor, message)
            }
            _ => {
                let join = comparisons.join(", ");
                let message = format!("expected one of: {join}");
                error::new_at(self.scope, self.cursor, message)
            }
        }
    }
}

/// Types that can be parsed by looking at just one token.
///
/// Use [`ParseStream::peek`] to peek one of these types in a parse stream
/// without consuming it from the stream.
///
/// [`ParseStream::peek`]: crate::parser::parse::ParseBuffer::peek
pub trait Peek {
    // Not public API.
    #[doc(hidden)]
    type Token: Token;
}

impl<F: Copy + FnOnce(TokenMarker) -> T, T: Token> Peek for F {
    type Token = T;
}

pub enum TokenMarker {}

impl<S> IntoSpans<S> for TokenMarker {
    fn into_spans(self) -> S {
        match self {}
    }
}
