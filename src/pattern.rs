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

impl<Real, Pat> Pattern<Real, Pat> {
    /// Get a property of a syntactic item, see `Property`
    pub fn get_property<P: Property<Real>>(&self, _: P) -> bool {
        self.real().map(P::get).unwrap_or(P::VAR_DEFAULT)
    }
}

impl<Real, Pat> Pattern<&Real, Pat> {
    /// Get a property of a syntactic item, see `Property`
    pub fn get_property_ref<P: Property<Real>>(&self, _: P) -> bool {
        self.real().map(|i| P::get(*i)).unwrap_or(P::VAR_DEFAULT)
    }
}

/// Defines a property of a syntactic item along with a default
/// that the property should take when the item is represented by a
/// molt variable.
pub trait Property<Item>: Sized {
    /// The default for molt variables.
    const VAR_DEFAULT: bool;

    /// Defines the property on the actual item.
    fn get(p: &Item) -> bool;
}
