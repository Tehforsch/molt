use std::marker::PhantomData;

use crate::NodeId;

#[derive(Debug)]
pub struct NoPunct;

#[derive(Debug)]
pub struct NodeList<T, P> {
    items: Vec<NodeId<T>>,
    _marker: PhantomData<P>,
}

impl<T> From<Vec<NodeId<T>>> for NodeList<T, NoPunct> {
    fn from(items: Vec<NodeId<T>>) -> Self {
        Self {
            items,
            _marker: PhantomData,
        }
    }
}

impl<T, P> NodeList<T, P> {
    pub fn empty() -> Self {
        Self {
            items: vec![],
            _marker: PhantomData,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &NodeId<T>> {
        self.items.iter()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn get(&self, idx: usize) -> Option<&NodeId<T>> {
        self.items.get(idx)
    }
}

impl<T, P> FromIterator<NodeId<T>> for NodeList<T, P> {
    fn from_iter<I: IntoIterator<Item = NodeId<T>>>(iter: I) -> Self {
        Self {
            items: iter.into_iter().collect(),
            _marker: PhantomData,
        }
    }
}

pub enum MatchingMode {
    #[allow(unused)]
    Exact,
    // ContainsAllInOrder,
    ContainsAll,
}
