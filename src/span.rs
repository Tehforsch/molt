use crate::{Id, Pattern};

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

pub trait WithSpan: Sized {
    fn with_span(self, span: Span) -> Spanned<Self> {
        Spanned::new(self, span)
    }

    fn pattern_with_span(self, span: Span) -> Spanned<Pattern<Self, Id>> {
        Spanned::new(self, span).as_pattern()
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

    pub fn as_pattern(self) -> SpannedPat<T> {
        self.map(|item| Pattern::Item(item))
    }

    pub fn decompose(self) -> (Span, T) {
        (self.span, self.item)
    }
}

pub type SpannedPat<T> = Spanned<Pattern<T, Id>>;

impl<T> SpannedPat<T> {
    pub fn is_var(&self) -> bool {
        matches!(self.item, Pattern::Var(_))
    }

    pub fn unwrap_real(self) -> Spanned<T> {
        self.map(|item| item.unwrap_item())
    }

    pub fn map_real<S>(self, f: impl Fn(T) -> S) -> SpannedPat<S> {
        self.map(|item| match item {
            Pattern::Item(t) => Pattern::Item(f(t)),
            Pattern::Var(id) => Pattern::Var(id),
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
