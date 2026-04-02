use std::io;

use crate::diag::Diag;
use crate::writer::Writer;

use crate::input::{FileId, Input};
use crate::modify::ModifyError;
use crate::molt_lang::{FileStructureError, InterpreterError};

#[derive(Debug)]
pub enum Error {
    Diag(Diag, FileId),
    FileStructure(FileStructureError),
    Misc(String),
    Modify(ModifyError),
    Io(io::Error),
    Interpreter(InterpreterError),
}

pub(crate) fn make_error_diagnostic(
    err: &Error,
) -> codespan_reporting::diagnostic::Diagnostic<FileId> {
    match err {
        Error::Diag(diag, file_id) => diag.clone().into_codespan(*file_id),
        _ => codespan_reporting::diagnostic::Diagnostic::error().with_message(format!("{err}")),
    }
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

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Diag(diag, _) => write!(f, "{diag}"),
            Error::FileStructure(error) => write!(f, "{error}"),
            Error::Misc(s) => write!(f, "{s}"),
            Error::Modify(s) => write!(f, "{s}"),
            Error::Io(s) => write!(f, "{s}"),
            Error::Interpreter(s) => write!(f, "{s}"),
        }
    }
}

impl std::error::Error for Error {}
