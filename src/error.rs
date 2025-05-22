use std::ops::Range;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::input::{FileId, Input};

#[derive(Debug)]
pub enum ResolveError {
    MultipleCommandGiven,
    NoCommandGiven,
}

#[derive(Debug)]
pub enum Error {
    Parse(syn::Error),
    Resolve(ResolveError),
    Misc(String),
}

impl Error {
    fn span(&self) -> Option<Range<usize>> {
        match self {
            Error::Parse(error) => Some(error.span().byte_range()),
            Error::Resolve(_) => None,
            Error::Misc(_) => None,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO
        match self {
            Error::Parse(error) => write!(f, "{}", error),
            Error::Resolve(_) => write!(f, "Error during resolution."),
            Error::Misc(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for Error {}

impl From<ResolveError> for Error {
    fn from(value: ResolveError) -> Self {
        Self::Resolve(value)
    }
}

impl From<syn::Error> for Error {
    fn from(value: syn::Error) -> Self {
        Self::Parse(value)
    }
}

pub(crate) fn emit_error(input: &Input, file: FileId, err: &Error) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let message = format!("{}", err);
    let mut diagnostic = Diagnostic::error().with_message(&message);
    if let Some(span) = err.span() {
        diagnostic =
            diagnostic.with_labels(vec![Label::primary(file, span).with_message(&message)]);
    }
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

// #[cfg(test)]
// pub(crate) fn emit_errors_str<T: AsCodespanError>(
//     file: &SourceFile,
//     errs: impl Iterator<Item = T>,
// ) -> String {
//     use codespan_reporting::term::termcolor::Buffer;

//     let mut writer = Buffer::no_color();
//     let config = codespan_reporting::term::Config::default();
//     for err in errs {
//         let diagnostic = Diagnostic::error()
//             .with_message(err.message())
//             .with_labels(vec![
//                 Label::primary((), err.span()).with_message(err.message())
//             ]);
//         term::emit(&mut writer, &config, file, &diagnostic).unwrap();
//     }
//     String::from_utf8(writer.into_inner()).unwrap()
// }
