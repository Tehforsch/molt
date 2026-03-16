use crate::Span;

struct Change {
    span: Span,
    len_delta: usize,
}

pub struct ChangeBuffer {
    code: String,
    changes: Vec<Change>,
}

impl ChangeBuffer {
    pub(crate) fn new(code: String) -> Self {
        Self {
            code,
            changes: vec![],
        }
    }

    pub fn code(self) -> String {
        self.code
    }

    pub fn make_change(&mut self, span: Span, new_code: &str) {
        let range = span.byte_range();
        let len_delta = new_code.len() - (range.end - range.start);
        let span = self.translate_span(span);
        let range = span.byte_range();
        self.code.replace_range(range.clone(), new_code);
        self.changes.push(Change { span, len_delta });
    }

    fn translate_span(&self, span: Span) -> Span {
        let start = span.byte_range().start;
        let end = span.byte_range().end;
        Span::new(
            self.translate_byte_position(start),
            self.translate_byte_position(end),
        )
    }

    fn translate_byte_position(&self, pos: usize) -> usize {
        let mut pos = pos;
        for change in &self.changes {
            let range = change.span.byte_range();
            if pos >= range.end {
                pos += change.len_delta;
            }
        }
        pos
    }
}
