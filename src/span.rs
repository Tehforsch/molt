use crate::{RawNodeId, Term};

#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: usize,
    end: usize,
    fake: bool,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            fake: false,
        }
    }

    pub fn byte_range(&self) -> std::ops::Range<usize> {
        if self.fake {
            panic!("Fake span");
        }
        self.start..self.end
    }

    pub fn fake() -> Self {
        Span {
            start: 0,
            end: 0,
            fake: true,
        }
    }

    pub fn contains(&self, other: &Span) -> bool {
        !self.fake && !other.fake && self.start <= other.start && other.end <= self.end
    }

    pub fn size(&self) -> usize {
        self.end - self.start
    }

    pub fn join(&self, span: impl Into<Span>) -> Span {
        let span = span.into();
        if self.fake || span.fake {
            panic!();
        }
        Span {
            start: self.start.min(span.start),
            end: self.end.max(span.end),
            fake: false,
        }
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
            fake: false,
        }
    }
}

impl From<proc_macro2::Span> for Span {
    fn from(value: proc_macro2::Span) -> Self {
        value.byte_range().into()
    }
}

pub trait WithSpan: Sized {
    fn with_span(self, span: Span) -> Spanned<Self> {
        Spanned::new(self, span)
    }

    fn term_with_span(self, span: Span) -> Spanned<Term<Self, RawNodeId>> {
        Spanned::new(self, span).into_term()
    }
}

impl<T: Sized> WithSpan for T {}

#[derive(Debug)]
pub struct Spanned<T> {
    span: Span,
    pub(crate) item: T,
}

impl<T> Spanned<T> {
    fn new(item: T, span: Span) -> Self {
        Self { span, item }
    }

    pub fn map<S>(self, f: impl Fn(T) -> S) -> Spanned<S> {
        Spanned {
            span: self.span,
            item: f(self.item),
        }
    }

    pub fn join<S>(&self, rhs: &Spanned<S>) -> Span {
        self.span.join(rhs.span)
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn item(self) -> T {
        self.item
    }

    pub fn into_term(self) -> SpannedTerm<T> {
        self.map(|item| Term::Item(item))
    }

    pub fn decompose(self) -> (Span, T) {
        (self.span, self.item)
    }
}

pub type SpannedTerm<T> = Spanned<Term<T, RawNodeId>>;

impl<T> SpannedTerm<T> {
    pub fn is_var(&self) -> bool {
        matches!(self.item, Term::Var(_))
    }

    pub fn unwrap_item(self) -> Spanned<T> {
        self.map(|item| item.unwrap_item())
    }

    pub fn map_item<S>(self, f: impl Fn(T) -> S) -> SpannedTerm<S> {
        self.map(|item| match item {
            Term::Item(t) => Term::Item(f(t)),
            Term::Var(id) => Term::Var(id),
        })
    }
}

impl<T> Spanned<Option<T>> {
    pub fn transpose(self) -> Option<Spanned<T>> {
        match self.item {
            Some(item) => Some(Spanned {
                span: self.span,
                item,
            }),
            None => None,
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}
