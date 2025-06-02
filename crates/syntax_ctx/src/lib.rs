use std::marker::PhantomData;

pub struct Span {
    pub start: usize,
    pub end: usize,
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

pub trait ToNode<Node> {
    fn to_node(self) -> Node;
    fn from_node(node: &Node) -> Option<&Self>;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<Real, Pat> {
    Real(Real),
    Pat(Pat),
}

type InternalId = Pattern<usize, usize>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Id(InternalId);

pub struct NodeId<T> {
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
    pub fn untyped(self) -> Id {
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

pub struct Var {
    name: String,
}

pub struct Ctx<Node> {
    nodes: Vec<Node>,
    vars: Vec<Var>,
    spans: Vec<Span>,
}

impl<Node> Ctx<Node> {
    pub fn add_node(&mut self, node: WithSpan<Node>) -> Id {
        self.spans.push(node.span);
        self.nodes.push(node.item);
        Id(InternalId::Real(self.nodes.len() - 1))
    }

    pub fn add<T: ToNode<Node>>(&mut self, t: WithSpan<T>) -> NodeId<T> {
        self.add_node(t.map(|item| item.to_node())).typed()
    }

    fn iter(&self) -> impl Iterator<Item = usize> {
        0..self.nodes.len()
    }
}

impl<Node> Default for Ctx<Node> {
    fn default() -> Self {
        Self {
            nodes: vec![],
            vars: vec![],
            spans: vec![],
        }
    }
}
