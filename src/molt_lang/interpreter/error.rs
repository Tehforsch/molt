#[derive(Debug)]
pub enum Error {
    UndefinedFn(String),
    UndefinedVar(String),
    InvalidMainFn,
}

impl Error {
    pub(crate) fn undefined_fn(fn_name: &str) -> Self {
        Self::UndefinedFn(fn_name.into())
    }

    pub(crate) fn undefined_var(var_name: &str) -> Self {
        Self::UndefinedVar(var_name.into())
    }
}

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self)
    }
}
