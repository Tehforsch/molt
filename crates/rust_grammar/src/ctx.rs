use std::{cell::RefCell, marker::PhantomData, rc::Rc};

use proc_macro2::Span;

use crate::node::{Node, ToNode};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

pub(crate) struct NodeId<T> {
    _marker: PhantomData<T>,
    id: Id,
}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for NodeId<T> {}

impl<T> std::fmt::Debug for NodeId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id.0.fmt(f)
    }
}

impl<T> PartialEq for NodeId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl<T> std::hash::Hash for NodeId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<T> Eq for NodeId<T> {}

impl<T> NodeId<T> {
    pub(crate) fn untyped(self) -> Id {
        self.id
    }
}

impl Id {
    fn typed<T>(self) -> NodeId<T> {
        NodeId {
            id: self,
            _marker: PhantomData,
        }
    }
}

pub struct WithSpan<T> {
    pub span: Span,
    pub item: T,
}

impl<T> WithSpan<T> {
    pub fn map<S>(self, f: impl Fn(T) -> S) -> WithSpan<S> {
        WithSpan {
            span: self.span,
            item: f(self.item),
        }
    }
}

#[derive(Default)]
pub(crate) struct Ctx {
    nodes: Vec<Node>,
    spans: Vec<Span>,
}

impl Ctx {
    pub fn add_node(&mut self, node: WithSpan<Node>) -> Id {
        self.spans.push(node.span);
        self.nodes.push(node.item);
        Id(self.nodes.len() - 1)
    }

    pub fn add<T: ToNode>(&mut self, t: WithSpan<T>) -> NodeId<T> {
        self.add_node(t.map(|item| item.to_node())).typed()
    }

    fn iter(&self) -> impl Iterator<Item = usize> {
        0..self.nodes.len()
    }
}

pub type ParseCtx = Rc<RefCell<Ctx>>;
