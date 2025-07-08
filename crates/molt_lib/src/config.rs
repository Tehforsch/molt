#[derive(Clone)]
pub struct Config {
    pub debug_print: bool,
    pub cargo_fmt: bool,
}

impl Config {
    pub fn test() -> Self {
        Self {
            debug_print: true,
            cargo_fmt: false,
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            debug_print: false,
            cargo_fmt: true,
        }
    }
}
