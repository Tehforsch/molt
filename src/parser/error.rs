use std::fmt::{self, Debug, Display};
use std::vec;

use proc_macro2::{LexError, Span};

use crate::parser::buffer::Cursor;

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Error {
    messages: Vec<ErrorMessage>,
}

struct ErrorMessage {
    span: std::ops::Range<crate::span::Span>,
    message: String,
}

impl Error {
    pub fn new<T: Display>(span: Span, message: T) -> Self {
        Error {
            messages: vec![ErrorMessage {
                span: span.into()..span.into(),
                message: message.to_string(),
            }],
        }
    }

    pub fn new_molt<T: Display>(span: crate::span::Span, message: T) -> Self {
        Error {
            messages: vec![ErrorMessage {
                span: span..span,
                message: message.to_string(),
            }],
        }
    }

    pub fn span(&self) -> crate::span::Span {
        self.messages[0].span.start.join(self.messages[0].span.end)
    }

    pub fn messages(&self) -> impl Iterator<Item = &str> {
        self.messages.iter().map(|msg| msg.message.as_str())
    }
}

pub(crate) fn new_at<T: Display>(scope: Span, cursor: Cursor, message: T) -> Error {
    if cursor.eof() {
        Error::new(scope, format!("unexpected end of input, {message}"))
    } else {
        let span = crate::parser::buffer::open_span_of_group(cursor);
        Error::new(span, message)
    }
}

pub(crate) fn new2<T: Display>(start: Span, end: Span, message: T) -> Error {
    return new2(start, end, message.to_string());

    fn new2(start: Span, end: Span, message: String) -> Error {
        Error {
            messages: vec![ErrorMessage {
                span: start.into()..end.into(),
                message,
            }],
        }
    }
}

impl Debug for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if self.messages.len() == 1 {
            formatter
                .debug_tuple("Error")
                .field(&self.messages[0])
                .finish()
        } else {
            formatter
                .debug_tuple("Error")
                .field(&self.messages)
                .finish()
        }
    }
}

impl Debug for ErrorMessage {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.message, formatter)
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(&self.messages[0].message)
    }
}

impl Clone for Error {
    fn clone(&self) -> Self {
        Error {
            messages: self.messages.clone(),
        }
    }
}

impl Clone for ErrorMessage {
    fn clone(&self) -> Self {
        ErrorMessage {
            span: self.span.clone(),
            message: self.message.clone(),
        }
    }
}

impl From<LexError> for Error {
    fn from(err: LexError) -> Self {
        Error::new(err.span(), err)
    }
}
