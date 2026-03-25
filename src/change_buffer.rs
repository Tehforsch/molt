use std::borrow::Cow;
use std::ops::Range;

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
        let span = self.translate_span(span);
        let range = span.byte_range();
        let new_code = pad_replacement(&self.code, &range, new_code);
        let len_delta = new_code.len() as i32 - (range.end - range.start) as i32;
        self.code.replace_range(range.clone(), &new_code);
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

fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn pad_replacement<'a>(code: &str, range: &Range<usize>, new_code: &'a str) -> Cow<'a, str> {
    if new_code.is_empty() {
        return Cow::Borrowed(new_code);
    }
    let needs_leading_space = range.start > 0
        && is_ident_char(code.as_bytes()[range.start - 1])
        && is_ident_char(new_code.as_bytes()[0]);
    let needs_trailing_space = range.end < code.len()
        && is_ident_char(code.as_bytes()[range.end])
        && is_ident_char(*new_code.as_bytes().last().unwrap());
    match (needs_leading_space, needs_trailing_space) {
        (false, false) => Cow::Borrowed(new_code),
        (true, false) => Cow::Owned(format!(" {new_code}")),
        (false, true) => Cow::Owned(format!("{new_code} ")),
        (true, true) => Cow::Owned(format!(" {new_code} ")),
    }
}
