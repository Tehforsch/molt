use std::collections::HashMap;

use crate::rust_grammar::Ident;

pub type ScopeIndex = usize;

pub struct Scope<T> {
    parent: Option<ScopeIndex>,
    variables: HashMap<Ident, T>,
    index: ScopeIndex,
}

impl<T> Default for Scope<T> {
    fn default() -> Self {
        Self {
            parent: None,
            variables: HashMap::default(),
            index: 0,
        }
    }
}

impl<T> Scope<T> {
    pub fn child_of(active_scope: &Scope<T>, index: usize) -> Scope<T> {
        Self {
            parent: Some(active_scope.index),
            variables: HashMap::default(),
            index,
        }
    }

    pub fn insert(&mut self, var_name: Ident, val: T) {
        self.variables.insert(var_name, val);
    }

    pub(crate) fn get(&self, name: &Ident) -> Option<&T> {
        self.variables.get(name)
    }

    pub(crate) fn parent(&self) -> Option<usize> {
        self.parent
    }
}
