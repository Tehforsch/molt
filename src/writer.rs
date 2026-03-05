use codespan_reporting::term::{
    self, Config,
    termcolor::{ColorChoice, StandardStream},
};

use crate::{Diagnostic, Input};

pub struct Writer {
    stream: StandardStream,
    config: Config,
}

impl Writer {
    pub fn new() -> Self {
        let stream = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();
        Self { stream, config }
    }

    pub fn emit_diagnostic(&self, input: &Input, diagnostic: Diagnostic) {
        term::emit(&mut self.stream.lock(), &self.config, input, &diagnostic).unwrap();
    }
}
