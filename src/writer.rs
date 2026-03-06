use codespan_reporting::term::{
    self, Config,
    termcolor::{Buffer, ColorChoice, StandardStream, WriteColor},
};

use crate::{Diagnostic, Input};

enum Stream {
    Stderr(StandardStream),
    Buffer(Buffer),
}

impl Stream {
    fn as_write_color(&mut self) -> &mut dyn WriteColor {
        match self {
            Stream::Stderr(s) => s,
            Stream::Buffer(b) => b,
        }
    }
}

pub struct Writer {
    stream: std::cell::RefCell<Stream>,
    config: Config,
}

impl Default for Writer {
    fn default() -> Self {
        Writer {
            stream: std::cell::RefCell::new(Stream::Stderr(StandardStream::stderr(
                ColorChoice::Always,
            ))),
            config: Config::default(),
        }
    }
}

impl Writer {
    pub fn buffer() -> Self {
        Writer {
            stream: std::cell::RefCell::new(Stream::Buffer(Buffer::no_color())),
            config: Config::default(),
        }
    }

    pub fn emit_diagnostic(&self, input: &Input, diagnostic: Diagnostic) {
        term::emit(
            self.stream.borrow_mut().as_write_color(),
            &self.config,
            input,
            &diagnostic,
        )
        .unwrap();
    }

    pub fn write_line(&self, s: &str) {
        writeln!(self.stream.borrow_mut().as_write_color(), "{s}").unwrap();
    }

    pub fn into_string(self) -> Option<String> {
        match self.stream.into_inner() {
            Stream::Buffer(buf) => Some(String::from_utf8(buf.into_inner()).unwrap()),
            Stream::Stderr(_) => None,
        }
    }
}
