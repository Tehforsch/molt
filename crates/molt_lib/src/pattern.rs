#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<Real, Pat> {
    Real(Real),
    Pat(Pat),
}

impl<Real, Pat> Pattern<Real, Pat> {
    pub fn unwrap_real(self) -> Real {
        match self {
            Pattern::Real(real) => real,
            Pattern::Pat(_) => panic!("unwrap called on pattern variant."),
        }
    }

    pub fn real(&self) -> Option<&Real> {
        match &self {
            Pattern::Real(t) => Some(t),
            Pattern::Pat(_) => None,
        }
    }

    fn map_real<S>(self, f: impl Fn(Real) -> S) -> Pattern<S, Pat> {
        match self {
            Pattern::Real(t) => Pattern::Real(f(t)),
            Pattern::Pat(id) => Pattern::Pat(id),
        }
    }
}

impl<Real, Pat: Copy> Pattern<Real, Pat> {
    pub fn as_ref(&self) -> Pattern<&Real, Pat> {
        match self {
            Pattern::Real(real) => Pattern::Real(real),
            Pattern::Pat(var) => Pattern::Pat(*var),
        }
    }

    pub fn as_mut(&mut self) -> Pattern<&mut Real, Pat> {
        match self {
            Pattern::Real(real) => Pattern::Real(real),
            Pattern::Pat(var) => Pattern::Pat(*var),
        }
    }
}

impl<Real, Pat> Pattern<Option<Real>, Pat> {
    fn transpose(self) -> Option<Pattern<Real, Pat>> {
        match self {
            Pattern::Real(opt) => opt.map(Pattern::Real),
            Pattern::Pat(var) => Some(Pattern::Pat(var)),
        }
    }
}
