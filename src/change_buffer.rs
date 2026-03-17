use crate::Span;

#[derive(Debug, Clone)]
struct Change {
    span: Span,
    len_delta: i32,
}

#[derive(Debug, Clone)]
pub struct ChangeBuffer {
    code: String,
    changes: Vec<Change>,
    span: Option<Span>,
}

impl ChangeBuffer {
    pub(crate) fn new(code: String) -> Self {
        Self {
            code,
            changes: vec![],
            span: None,
        }
    }

    pub(crate) fn new_subspan(code: String, span: Span) -> Self {
        Self {
            code,
            changes: vec![],
            span: Some(span),
        }
    }

    pub fn code(self) -> String {
        match self.span {
            Some(span) => self.code[self.translate_span(span).byte_range()].into(),
            None => self.code,
        }
    }

    pub fn make_change(&mut self, span: Span, new_code: &str) {
        let range = span.byte_range();
        let len_delta: i32 = new_code.len() as i32 - (range.end - range.start) as i32;
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
                pos = (pos as i32 + change.len_delta) as usize;
            }
        }
        pos
    }
}
