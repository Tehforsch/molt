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
    Parse(rust_grammar::Error, FileId),
    Resolve(ResolveError),
    Misc(String),
}

impl Error {
    fn span_and_file_id(&self) -> Option<(Span, FileId)> {
        match self {
            Error::Parse(error, file_id) => Some((error.span(), *file_id)),
            Error::Resolve(_) => None,
            Error::Misc(_) => None,
        }
    }
}

pub(crate) fn make_error_diagnostic(err: &Error) -> Diagnostic<FileId> {
    let message = format!("{}", err);
    let mut diagnostic = Diagnostic::error().with_message(&message);
    if let Some((span, file)) = err.span_and_file_id() {
        diagnostic = diagnostic.with_labels(vec![
            Label::primary(file, span.byte_range()).with_message(&message),
        ]);
    }
    diagnostic
}

pub fn emit_error<T>(input: &Input, err: Result<T, Error>) -> Result<T, Error> {
    match err {
        Ok(t) => Ok(t),
        Err(e) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();
            let diagnostic = make_error_diagnostic(&e);
            term::emit(&mut writer.lock(), &config, input, &diagnostic).unwrap();
            Err(e)
        }
    }
}

#[cfg(test)]
pub(crate) fn emit_diagnostic_str(input: &Input, diagnostic: Diagnostic<FileId>) -> String {
    use codespan_reporting::term::termcolor::Buffer;

    let mut writer = Buffer::no_color();
    let config = codespan_reporting::term::Config::default();
    term::emit(&mut writer, &config, input, &diagnostic).unwrap();
    String::from_utf8(writer.into_inner()).unwrap()
}

impl From<ResolveError> for Error {
    fn from(t: ResolveError) -> Self {
        Self::Resolve(t)
    }
}

impl Error {
    pub fn parse(t: rust_grammar::Error, file_id: FileId) -> Self {
        Self::Parse(t, file_id)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse(error, _) => write!(f, "{}", error),
            Error::Resolve(error) => write!(f, "{}", error),
            Error::Misc(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for Error {}
