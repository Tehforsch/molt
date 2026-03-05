use codespan_reporting::term::{
    self, Config,
    termcolor::{ColorChoice, StandardStream},
};

use crate::{Diagnostic, Input};

pub struct Writer {
    stream: StandardStream,
    config: Config,
}

impl Default for Writer {
    fn default() -> Self {
        let stream = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();
        Writer { stream, config }
    }
}

impl Writer {
    pub fn emit_diagnostic(&self, input: &Input, diagnostic: Diagnostic) {
        term::emit(&mut self.stream.lock(), &self.config, input, &diagnostic).unwrap();
    }
}
