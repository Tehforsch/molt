#[derive(Clone)]
pub struct Config {
    pub debug_print: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self { debug_print: true }
    }
}
