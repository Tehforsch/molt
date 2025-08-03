/// This type represents either a concrete item in the AST, or a molt
/// variable.
///
/// This distinction is separate from the `Mode` type, which
/// represents whether a given item originated within real source
/// code, or molt source code. The two are related, since the
/// `Pattern::Var` variant can only occur within molt source code.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<Item, Var> {
    Item(Item),
    Var(Var),
}

impl<Item, Var> Pattern<Item, Var> {
    pub fn unwrap_item(self) -> Item {
        match self {
            Pattern::Item(item) => item,
            Pattern::Var(_) => panic!("unwrap called on pattern variant."),
        }
    }

    pub fn get_item(&self) -> Option<&Item> {
        match &self {
            Pattern::Item(t) => Some(t),
            Pattern::Var(_) => None,
        }
    }
}

impl<Item, Var: Copy> Pattern<Item, Var> {
    pub fn as_ref(&self) -> Pattern<&Item, Var> {
        match self {
            Pattern::Item(item) => Pattern::Item(item),
            Pattern::Var(var) => Pattern::Var(*var),
        }
    }

    pub fn as_mut(&mut self) -> Pattern<&mut Item, Var> {
        match self {
            Pattern::Item(item) => Pattern::Item(item),
            Pattern::Var(var) => Pattern::Var(*var),
        }
    }
}

impl<Item, Var> Pattern<Item, Var> {
    /// Get a property of a syntactic item, see `Property`
    pub fn get_property<P: Property<Item>>(&self, _: P) -> bool {
        self.get_item().map(P::get).unwrap_or(P::VAR_DEFAULT)
    }
}

impl<Item, Var> Pattern<&Item, Var> {
    /// Get a property of a syntactic item, see `Property`
    pub fn get_property_ref<P: Property<Item>>(&self, _: P) -> bool {
        self.get_item()
            .map(|i| P::get(*i))
            .unwrap_or(P::VAR_DEFAULT)
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
