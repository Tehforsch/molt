use std::fmt::{self, Debug, Display};
use std::vec;

use proc_macro2::{LexError, Span};

use crate::parser::buffer::Cursor;
use crate::parser::thread::ThreadBound;

pub type Result<T> = std::result::Result<T, Error>;

pub struct Error {
    messages: Vec<ErrorMessage>,
}

struct ErrorMessage {
    // Span is implemented as an index into a thread-local interner to keep the
    // size small. It is not safe to access from a different thread. We want
    // errors to be Send and Sync to play nicely with ecosystem crates for error
    // handling, so pin the span we're given to its original thread and assume
    // it is Span::call_site if accessed from any other thread.
    span: ThreadBound<SpanRange>,
    message: String,
}

// Cannot use std::ops::Range<Span> because that does not implement Copy,
// whereas ThreadBound<T> requires a Copy impl as a way to ensure no Drop impls
// are involved.
struct SpanRange {
    start: Span,
    end: Span,
}

#[cfg(test)]
struct _Test
where
    Error: Send + Sync;

impl Error {
    pub fn new<T: Display>(span: Span, message: T) -> Self {
        return new(span, message.to_string());

        fn new(span: Span, message: String) -> Error {
            Error {
                messages: vec![ErrorMessage {
                    span: ThreadBound::new(SpanRange {
                        start: span,
                        end: span,
                    }),
                    message,
                }],
            }
        }
    }

    pub fn span(&self) -> Span {
        let SpanRange { start, end } = match self.messages[0].span.get() {
            Some(span) => *span,
            None => return Span::call_site(),
        };
        start.join(end).unwrap_or(start)
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
                span: ThreadBound::new(SpanRange { start, end }),
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
            span: self.span,
            message: self.message.clone(),
        }
    }
}

impl Clone for SpanRange {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for SpanRange {}

impl From<LexError> for Error {
    fn from(err: LexError) -> Self {
        Error::new(err.span(), err)
    }
}
