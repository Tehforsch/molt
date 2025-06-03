use std::marker::PhantomData;

#[derive(Copy, Clone)]
pub struct Span {
    start: usize,
    end: usize,
    fake: bool,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            fake: false,
        }
    }

    pub fn byte_range(&self) -> std::ops::Range<usize> {
        if self.fake {
            panic!("Fake span");
        }
        self.start..self.end
    }

    pub fn fake() -> Self {
        Span {
            start: 0,
            end: 0,
            fake: true,
        }
    }

    fn join(&self, span: Span) -> Span {
        if self.fake || span.fake {
            panic!();
        }
        Span {
            start: self.start.min(span.start),
            end: self.end.max(span.end),
            fake: false,
        }
    }
}

pub struct WithSpan<T> {
    span: Span,
    item: T,
}

impl<T> WithSpan<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self { span, item }
    }

    pub fn map<S>(self, f: impl Fn(T) -> S) -> WithSpan<S> {
        WithSpan {
            span: self.span,
            item: f(self.item),
        }
    }

    pub fn join<S>(&self, rhs: &WithSpan<S>) -> Span {
        self.span.join(rhs.span)
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn take(self) -> T {
        self.item
    }
}

impl<T> std::ops::Deref for WithSpan<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> std::ops::DerefMut for WithSpan<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}

pub trait ToNode<Node> {
    fn to_node(self) -> Node;
    fn from_node(node: &Node) -> Option<&Self>;
    fn from_node_mut(node: &mut Node) -> Option<&mut Self>;
}

impl<T> ToNode<T> for T {
    fn to_node(self) -> T {
        self
    }

    fn from_node(node: &T) -> Option<&Self> {
        Some(node)
    }

    fn from_node_mut(node: &mut T) -> Option<&mut Self> {
        Some(node)
    }
}

pub trait GetKind {
    type Kind: Copy + std::fmt::Debug + PartialEq + Eq;

    fn kind(&self) -> Self::Kind;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<Real, Pat> {
    Real(Real),
    Pat(Pat),
}

impl<Real, Pat> Pattern<Real, Pat> {
    pub fn unwrap(self) -> Real {
        match self {
            Pattern::Real(real) => real,
            Pattern::Pat(_) => panic!("unwrap called on pattern variant."),
        }
    }
}

type InternalId = Pattern<usize, usize>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Id(InternalId);

pub struct NodeId<T> {
    _marker: PhantomData<T>,
    id: Id,
}

impl<T> NodeId<T> {
    // used instead of Expr::PLACEHOLDER in parsing.
    pub fn placeholder() -> Self {
        Self {
            id: Id(InternalId::Real(usize::MAX)),
            _marker: PhantomData,
        }
    }
}

pub struct NoPunct;

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

impl<T> From<NodeId<T>> for Id {
    fn from(value: NodeId<T>) -> Self {
        value.id
    }
}

impl<T, P> NodeList<T, P> {
    pub fn new(iter: impl Iterator<Item = NodeId<T>>) -> Self {
        Self {
            items: iter.collect(),
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

pub enum MatchingMode {
    #[allow(unused)]
    Exact,
    // ContainsAllInOrder,
    ContainsAll,
}

impl Id {
    fn typed<T>(self) -> NodeId<T> {
        NodeId {
            id: self,
            _marker: PhantomData,
        }
    }

    pub fn unwrap_idx(self) -> usize {
        match self.0 {
            Pattern::Real(idx) => idx,
            Pattern::Pat(idx) => idx,
        }
    }
}

pub struct Var<Node: GetKind> {
    name: String,
    kind: Node::Kind,
}

impl<Node: GetKind> PartialEq for Var<Node> {
    fn eq(&self, other: &Self) -> bool {
        debug_assert_eq!(self.kind, other.kind);
        self.name.eq(&other.name)
    }
}

#[derive(Debug)]
pub struct VarDecl {
    pub id: Id,
    pub node: Option<Id>,
}

impl<Node: GetKind> Var<Node> {
    pub fn new(name: String, kind: Node::Kind) -> Self {
        Self { name, kind }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

pub struct Ctx<Node: GetKind> {
    nodes: Vec<Node>,
    vars: Vec<Var<Node>>,
    spans: Vec<Span>,
}

impl<Node: GetKind> Ctx<Node> {
    fn add_node(&mut self, node: WithSpan<Node>) -> Id {
        self.spans.push(node.span);
        self.nodes.push(node.item);
        Id(InternalId::Real(self.nodes.len() - 1))
    }

    pub fn add<T: ToNode<Node>>(&mut self, t: WithSpan<T>) -> NodeId<T> {
        self.add_node(t.map(|item| item.to_node())).typed()
    }

    fn add_var_internal(&mut self, var: Var<Node>) -> Id {
        self.vars.push(var);
        Id(InternalId::Pat(self.vars.len() - 1))
    }

    pub fn add_var<T: ToNode<Node>>(&mut self, var: Var<Node>) -> NodeId<T> {
        let id = if let Some(id) = self.add_existing_var(&var.name) {
            id
        } else {
            self.add_var_internal(var)
        };
        id.typed()
    }

    pub fn add_existing_var(&self, var: &str) -> Option<Id> {
        self.vars
            .iter()
            .enumerate()
            .find(|(_, v)| var == v.name)
            .map(|(i, _)| Id(InternalId::Pat(i)))
    }

    pub fn get<T: ToNode<Node>>(&self, id: impl Into<Id>) -> Pattern<&T, Id> {
        let id = id.into();
        match id.0 {
            InternalId::Real(idx) => Pattern::Real(T::from_node(&self.nodes[idx]).unwrap()),
            InternalId::Pat(_) => Pattern::Pat(id),
        }
    }

    pub fn get_mut<T: ToNode<Node>>(&mut self, id: impl Into<Id>) -> Pattern<&mut T, Id> {
        let id = id.into();
        match id.0 {
            InternalId::Real(idx) => Pattern::Real(T::from_node_mut(&mut self.nodes[idx]).unwrap()),
            InternalId::Pat(_) => Pattern::Pat(id),
        }
    }

    pub fn get_real<T: ToNode<Node>>(&self, id: impl Into<Id>) -> Option<&T> {
        match self.get(id) {
            Pattern::Real(t) => Some(t),
            Pattern::Pat(_) => None,
        }
    }

    pub fn get_real_mut<T: ToNode<Node>>(&mut self, id: impl Into<Id>) -> Option<&mut T> {
        match self.get_mut(id) {
            Pattern::Real(t) => Some(t),
            Pattern::Pat(_) => None,
        }
    }

    pub fn get_var(&self, id: Id) -> &Var<Node> {
        match id.0 {
            InternalId::Real(_) => panic!(),
            InternalId::Pat(idx) => &self.vars[idx],
        }
    }

    pub fn get_kind(&self, id: Id) -> Node::Kind {
        match id.0 {
            InternalId::Real(idx) => self.nodes[idx].kind(),
            InternalId::Pat(idx) => self.vars[idx].kind,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = Id> {
        (0..self.nodes.len()).map(|id| Id(InternalId::Real(id)))
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = &Var<Node>> {
        self.vars.iter()
    }

    pub fn get_span(&self, id: impl Into<Id>) -> Span {
        match id.into().0 {
            Pattern::Real(idx) => self.spans[idx],
            Pattern::Pat(_) => panic!(),
        }
    }

    pub fn print<'a>(&'a self, id: Id, src: &'a str) -> &'a str {
        match id.0 {
            Pattern::Real(_) => {
                let span = self.get_span(id);
                &src[span.byte_range()]
            }
            Pattern::Pat(idx) => &self.vars[idx].name,
        }
    }
}

impl<Node: GetKind> Default for Ctx<Node> {
    fn default() -> Self {
        Self {
            nodes: vec![],
            vars: vec![],
            spans: vec![],
        }
    }
}
