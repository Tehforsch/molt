use std::io;

use crate::diag::Diag;
use crate::writer::Writer;

use crate::input::{FileId, Input};
use crate::modify::ModifyError;
use crate::molt_lang::{FileStructureError, InterpreterError};

#[derive(Debug)]
pub enum Error {
    Diag(Vec<Diag>, FileId),
    FileStructure(FileStructureError),
    Misc(String),
    Modify(ModifyError),
    Io(io::Error),
    Interpreter(InterpreterError),
}

pub(crate) fn make_error_diagnostics(
    err: &Error,
) -> Vec<codespan_reporting::diagnostic::Diagnostic<FileId>> {
    match err {
        Error::Diag(diags, file_id) => diags
            .iter()
            .map(|diag| diag.clone().into_codespan(*file_id))
            .collect(),
        _ => {
            vec![codespan_reporting::diagnostic::Diagnostic::error().with_message(format!("{err}"))]
        }
    }
}

pub fn emit_diags(writer: &Writer, input: &Input, file_id: FileId, e: Vec<Diag>) {
    for diagnostic in e
        .into_iter()
        .map(|diag| diag.clone().into_codespan(file_id))
    {
        writer.emit_diagnostic(input, diagnostic);
    }
}

pub fn emit_error<T>(writer: &Writer, input: &Input, err: Result<T, Error>) -> Result<T, Error> {
    match err {
        Ok(t) => Ok(t),
        Err(e) => {
            for diagnostic in make_error_diagnostics(&e) {
                writer.emit_diagnostic(input, diagnostic);
            }
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
            Error::Diag(diags, _) => {
                for (i, diag) in diags.iter().enumerate() {
                    if i > 0 {
                        writeln!(f)?;
                    }
                    write!(f, "{diag}")?;
                }
                Ok(())
            }
            Error::FileStructure(error) => write!(f, "{error}"),
            Error::Misc(s) => write!(f, "{s}"),
            Error::Modify(s) => write!(f, "{s}"),
            Error::Io(s) => write!(f, "{s}"),
            Error::Interpreter(s) => write!(f, "{s}"),
        }
    }
}

impl std::error::Error for Error {}
