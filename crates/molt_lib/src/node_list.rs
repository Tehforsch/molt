use std::marker::PhantomData;

use crate::{NodeId, ParsingMode, Pattern};

#[derive(Debug)]
pub struct NoPunct;

pub type NodeList<T, P> = Pattern<RealNodeList<T, P>, PatNodeList<T, P>>;

#[derive(Debug)]
pub enum PatNodeList<T, P> {
    Single(Single<T, P>),
}

#[derive(Debug)]
pub struct Single<T, P> {
    item: NodeId<T>,
    mode: MatchingMode,
    _marker: PhantomData<P>,
}

#[derive(Debug)]
pub struct RealNodeList<T, P> {
    items: Vec<NodeId<T>>,
    _marker: PhantomData<P>,
}

impl<T, P> NodeList<T, P> {
    pub fn empty(mode: ParsingMode) -> Self {
        match mode {
            ParsingMode::Real => Self::Real(RealNodeList::empty()),
            ParsingMode::Pat => Self::Pat(PatNodeList::empty()),
        }
    }
}

impl<T, P> PatNodeList<T, P> {
    fn empty() -> PatNodeList<T, P> {
        todo!()
    }
}

impl<T> From<Vec<NodeId<T>>> for RealNodeList<T, NoPunct> {
    fn from(items: Vec<NodeId<T>>) -> Self {
        Self {
            items,
            _marker: PhantomData,
        }
    }
}

impl<T, P> From<RealNodeList<T, P>> for Vec<NodeId<T>> {
    fn from(val: RealNodeList<T, P>) -> Self {
        val.items
    }
}

impl<T, P> RealNodeList<T, P> {
    pub fn new(items: Vec<NodeId<T>>) -> Self {
        Self {
            items,
            _marker: PhantomData,
        }
    }

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

impl<T, P> FromIterator<NodeId<T>> for RealNodeList<T, P> {
    fn from_iter<I: IntoIterator<Item = NodeId<T>>>(iter: I) -> Self {
        Self {
            items: iter.into_iter().collect(),
            _marker: PhantomData,
        }
    }
}

#[derive(Debug)]
pub enum MatchingMode {
    #[allow(unused)]
    Exact,
    // ContainsAllInOrder,
    ContainsAll,
}
