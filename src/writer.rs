use codespan_reporting::term::{
    self, Config,
    termcolor::{ColorChoice, StandardStream},
};

use crate::{Diagnostic, Input};

pub struct Writer<'src> {
    stream: StandardStream,
    config: Config,
    input: &'src Input,
}

impl<'src> Writer<'src> {
    pub fn new(input: &'src Input) -> Self {
        let stream = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();
        Self {
            stream,
            config,
            input,
        }
    }

    pub fn emit_diagnostic(&self, diagnostic: Diagnostic) {
        term::emit(
            &mut self.stream.lock(),
            &self.config,
            self.input,
            &diagnostic,
        )
        .unwrap();
    }
}
