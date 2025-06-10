#[derive(Clone)]
pub struct Config {
    pub debug_print: bool,
}

impl Default for Config {
    #[cfg(test)]
    fn default() -> Self {
        Self { debug_print: true }
    }

    #[cfg(not(test))]
    fn default() -> Self {
        Self { debug_print: false }
    }
}
