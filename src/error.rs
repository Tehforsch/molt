use std::io;

use crate::writer::Writer;
use crate::{Span, parser};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::input::{FileId, Input};
use crate::modify::ModifyError;
use crate::molt_lang::{FileStructureError, InterpreterError, ResolverError};

#[derive(Debug)]
pub enum Error {
    Parse(crate::parser::Error, FileId),
    Resolver(ResolverError, FileId),
    FileStructure(FileStructureError),
    Misc(String),
    Modify(ModifyError),
    Io(io::Error),
    Interpreter(InterpreterError),
}

impl Error {
    fn span_and_file_id(&self) -> Option<(Span, FileId)> {
        self.parse_error_and_file_id()
            .map(|(error, file_id)| (error.span().byte_range().into(), file_id))
    }

    fn hints(&self) -> Option<impl Iterator<Item = &str>> {
        self.parse_error_and_file_id()
            .map(|(error, _)| error.messages())
    }

    fn parse_error_and_file_id(&self) -> Option<(&parser::Error, FileId)> {
        match self {
            Error::Parse(error, file_id) => Some((error, *file_id)),
            Error::Resolver(ResolverError::Parse(error), file_id) => Some((error, *file_id)),
            Error::Resolver(_, _) => None,
            Error::FileStructure(_) => None,
            Error::Misc(_) => None,
            Error::Modify(_) => None,
            Error::Io(_) => None,
            Error::Interpreter(_) => None,
        }
    }
}

pub(crate) fn make_error_diagnostic(err: &Error) -> Diagnostic<FileId> {
    let message = format!("{err}");
    let mut diagnostic = Diagnostic::error().with_message(&message);
    if let Some((span, file)) = err.span_and_file_id() {
        diagnostic = diagnostic.with_labels(vec![
            Label::primary(file, span.byte_range()).with_message(&message),
        ]);
    }
    if let Some(hints) = err.hints() {
        for msg in hints {
            diagnostic = diagnostic.with_note(msg);
        }
    }
    diagnostic
}

pub fn emit_error<T>(writer: &Writer, input: &Input, err: Result<T, Error>) -> Result<T, Error> {
    match err {
        Ok(t) => Ok(t),
        Err(e) => {
            let diagnostic = make_error_diagnostic(&e);
            writer.emit_diagnostic(input, diagnostic);
            Err(e)
        }
    }
}

impl From<ModifyError> for Error {
    fn from(t: ModifyError) -> Self {
        Self::Modify(t)
    }
}

impl From<io::Error> for Error {
    fn from(t: io::Error) -> Self {
        Self::Io(t)
    }
}

impl Error {
    pub fn parse(t: crate::parser::Error, file_id: FileId) -> Self {
        Self::Parse(t, file_id)
    }

    pub fn resolve(t: crate::molt_lang::FileStructureError) -> Self {
        Self::FileStructure(t)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse(error, _) => write!(f, "{error}"),
            Error::Resolver(error, _) => write!(f, "{error}"),
            Error::FileStructure(error) => write!(f, "{error}"),
            Error::Misc(s) => write!(f, "{s}"),
            Error::Modify(s) => write!(f, "{s}"),
            Error::Io(s) => write!(f, "{s}"),
            Error::Interpreter(s) => write!(f, "{s}"),
        }
    }
}

impl std::error::Error for Error {}
