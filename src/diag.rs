use crate::Span;
use crate::input::FileId;

#[macro_export]
macro_rules! error {
    ($lit: literal) => {
        Diag::error(format!($lit))
    };
    ($lit: literal, $($expr: expr),*) => {
        Diag::error(format!($lit, $($expr),*))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum LabelKind {
    Primary,
    Secondary,
}

#[derive(Clone, Debug)]
pub(crate) struct DiagLabel {
    pub(crate) span: Span,
    pub(crate) message: String,
    pub(crate) kind: LabelKind,
}

#[derive(Clone, Debug)]
pub(crate) struct Diag {
    pub(crate) severity: Severity,
    pub(crate) message: String,
    pub(crate) labels: Vec<DiagLabel>,
    pub(crate) notes: Vec<String>,
}

impl Diag {
    pub(crate) fn error(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub(crate) fn label(mut self, span: impl Into<Span>, message: impl Into<String>) -> Self {
        self.labels.push(DiagLabel {
            span: span.into(),
            message: message.into(),
            kind: if self.labels.is_empty() {
                LabelKind::Primary
            } else {
                LabelKind::Secondary
            },
        });
        self
    }

    pub(crate) fn note(mut self, message: impl Into<String>) -> Self {
        self.notes.push(message.into());
        self
    }

    pub(crate) fn into_codespan(
        self,
        file_id: FileId,
    ) -> codespan_reporting::diagnostic::Diagnostic<FileId> {
        use codespan_reporting::diagnostic::{Diagnostic, Label};

        let mut diag = match self.severity {
            Severity::Error => Diagnostic::error(),
        };

        diag = diag.with_message(&self.message);

        let labels: Vec<_> = self
            .labels
            .iter()
            .map(|l| {
                let label = match l.kind {
                    LabelKind::Primary => Label::primary(file_id, l.span.byte_range()),
                    LabelKind::Secondary => Label::secondary(file_id, l.span.byte_range()),
                };
                label.with_message(&l.message)
            })
            .collect();

        if !labels.is_empty() {
            diag = diag.with_labels(labels);
        }

        for note in &self.notes {
            diag = diag.with_notes(vec![note.clone()]);
        }

        diag
    }
}

impl From<crate::parser::Error> for Diag {
    fn from(err: crate::parser::Error) -> Self {
        let mut diag = Diag::error(err.to_string()).label(err.span(), "here");
        for msg in err.messages().skip(1) {
            diag = diag.note(msg);
        }
        diag
    }
}

impl std::fmt::Display for Diag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
