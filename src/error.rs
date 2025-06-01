use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use rust_grammar::Span;

use crate::input::{FileId, Input};
use crate::resolve::ResolveError;

#[derive(Debug)]
pub enum Error {
    Parse(rust_grammar::Error),
    Resolve(ResolveError),
    Misc(String),
}

impl Error {
    fn span(&self) -> Option<Span> {
        match self {
            Error::Parse(error) => Some(error.span()),
            Error::Resolve(_) => None,
            Error::Misc(_) => None,
        }
    }
}

pub(crate) fn make_error_diagnostic(file: FileId, err: &Error) -> Diagnostic<FileId> {
    let message = format!("{}", err);
    let mut diagnostic = Diagnostic::error().with_message(&message);
    if let Some(span) = err.span() {
        diagnostic = diagnostic.with_labels(vec![
            Label::primary(file, span.byte_range()).with_message(&message),
        ]);
    }
    diagnostic
}

pub(crate) fn emit_error(input: &Input, file: FileId, err: &Error) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let diagnostic = make_error_diagnostic(file, err);
    term::emit(&mut writer.lock(), &config, input, &diagnostic).unwrap();
}

#[cfg(test)]
pub(crate) fn emit_diagnostic_str(input: &Input, diagnostic: Diagnostic<FileId>) -> String {
    use codespan_reporting::term::termcolor::Buffer;

    let mut writer = Buffer::no_color();
    let config = codespan_reporting::term::Config::default();
    term::emit(&mut writer, &config, input, &diagnostic).unwrap();
    String::from_utf8(writer.into_inner()).unwrap()
}

macro_rules! impl_from {
    ($ty: ty, $ident: ident) => {
        impl From<$ty> for Error {
            fn from(t: $ty) -> Self {
                Self::$ident(t)
            }
        }
    };
}

impl_from!(ResolveError, Resolve);
impl_from!(rust_grammar::Error, Parse);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse(error) => write!(f, "{}", error),
            Error::Resolve(error) => write!(f, "{}", error),
            Error::Misc(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for Error {}
