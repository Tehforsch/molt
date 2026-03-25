#[derive(Debug)]
pub enum Error {
    InvalidMainFn,
    Assertion,
    AssignmentToUninitializedNode,
}

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self)
    }
}
