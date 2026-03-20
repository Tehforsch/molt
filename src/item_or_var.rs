/// This type represents either a concrete item in the AST, or a molt
/// variable.
///
/// This distinction is separate from the `Mode` type, which
/// represents whether a given item originated within real source
/// code, or molt source code. The two are related, since the
/// `Var` variant can only occur within molt source code.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ItemOrVar<Item, Var> {
    Item(Item),
    Var(Var),
}

impl<Item: Clone, Var> ItemOrVar<&Item, Var> {
    pub fn cloned(self) -> ItemOrVar<Item, Var> {
        match self {
            ItemOrVar::Item(item) => ItemOrVar::Item(item.clone()),
            ItemOrVar::Var(var) => ItemOrVar::Var(var),
        }
    }
}

impl<Item, Var> ItemOrVar<Item, Var> {
    pub fn unwrap_item(self) -> Item {
        match self {
            ItemOrVar::Item(item) => item,
            ItemOrVar::Var(_) => panic!("unwrap called on `Var` variant."),
        }
    }

    pub fn get_item(&self) -> Option<&Item> {
        match &self {
            ItemOrVar::Item(t) => Some(t),
            ItemOrVar::Var(_) => None,
        }
    }
}

impl<Item, Var: Copy> ItemOrVar<Item, Var> {
    pub fn as_ref(&self) -> ItemOrVar<&Item, Var> {
        match self {
            ItemOrVar::Item(item) => ItemOrVar::Item(item),
            ItemOrVar::Var(var) => ItemOrVar::Var(*var),
        }
    }

    pub fn as_mut(&mut self) -> ItemOrVar<&mut Item, Var> {
        match self {
            ItemOrVar::Item(item) => ItemOrVar::Item(item),
            ItemOrVar::Var(var) => ItemOrVar::Var(*var),
        }
    }
}

impl<Item, Var> ItemOrVar<Item, Var> {
    /// Get a property of a syntactic item, see `Property`
    pub fn get_property<P: Property<Item>>(&self, _: P) -> bool {
        self.get_item().map(P::get).unwrap_or(P::VAR_DEFAULT)
    }
}

impl<Item, Var> ItemOrVar<&Item, Var> {
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
