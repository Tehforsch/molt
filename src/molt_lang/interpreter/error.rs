use std::io;

#[derive(Debug)]
pub enum Error {
    InvalidMainFn,
    Assertion,
    AssignmentToUninitializedNode,
    EmptyStringInShell,
    Io(Box<io::Error>),
}

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self)
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Io(Box::new(value))
    }
}
