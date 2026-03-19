use std::marker::PhantomData;

use crate::{NodeId, Pattern};

pub type NodeList<T, P> = Pattern<RealNodeList<T, P>, NodeId<T>>;

#[derive(Debug)]
pub struct RealNodeList<T, P> {
    items: Vec<NodeId<T>>,
    _marker: PhantomData<P>,
}

impl<T, P> RealNodeList<T, P> {
    pub fn new(items: Vec<NodeId<T>>) -> Self {
        Self {
            items,
            _marker: PhantomData,
        }
    }
}

impl<T, P> NodeList<T, P> {
    pub fn empty_real() -> Self {
        Self::Item(RealNodeList::new(vec![]))
    }
}

impl<T, P> AsRef<[NodeId<T>]> for RealNodeList<T, P> {
    fn as_ref(&self) -> &[NodeId<T>] {
        &self.items
    }
}

impl<T, P> FromIterator<NodeId<T>> for RealNodeList<T, P> {
    fn from_iter<I: IntoIterator<Item = NodeId<T>>>(iter: I) -> Self {
        Self {
            items: iter.into_iter().collect(),
            _marker: PhantomData,
        }
    }
}
