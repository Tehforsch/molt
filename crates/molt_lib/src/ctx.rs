use std::marker::PhantomData;

use crate::{Span, Spanned, span::SpannedPat};

pub trait ToNode<Node: NodeType>: Sized {
    fn to_node(self) -> Node;
    fn from_node(node: Node) -> Option<Self>;
    fn from_node_ref(node: &Node) -> Option<&Self>;
    fn from_node_ref_mut(node: &mut Node) -> Option<&mut Self>;
    fn kind() -> Node::Kind;
}

impl<T: NodeType> ToNode<T> for T {
    fn to_node(self) -> T {
        self
    }

    fn from_node(node: T) -> Option<Self> {
        Some(node)
    }

    fn from_node_ref(node: &T) -> Option<&Self> {
        Some(node)
    }

    fn from_node_ref_mut(node: &mut T) -> Option<&mut Self> {
        Some(node)
    }

    fn kind() -> T::Kind {
        panic!()
    }
}

pub trait NodeType {
    type Kind: Copy + std::fmt::Debug;

    fn kind(&self) -> Self::Kind;
    fn is_comparable(kind_pat: Self::Kind, kind_real: Self::Kind) -> bool;
}

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

    fn map_real<S>(self, f: impl Fn(Real) -> S) -> Pattern<S, Pat> {
        match self {
            Pattern::Real(t) => Pattern::Real(f(t)),
            Pattern::Pat(id) => Pattern::Pat(id),
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

impl<Real, Pat> Pattern<Option<Real>, Pat> {
    fn transpose(self) -> Option<Pattern<Real, Pat>> {
        match self {
            Pattern::Real(opt) => opt.map(Pattern::Real),
            Pattern::Pat(var) => Some(Pattern::Pat(var)),
        }
    }
}

type InternalId = Pattern<usize, usize>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Id(InternalId);

impl Id {
    pub fn is_pat(&self) -> bool {
        match self.0 {
            Pattern::Real(_) => false,
            Pattern::Pat(_) => true,
        }
    }
}

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

impl Id {
    pub(crate) fn typed<T>(self) -> NodeId<T> {
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

pub struct Var<K> {
    name: String,
    kind: K,
}

impl<K: std::fmt::Debug + PartialEq> PartialEq for Var<K> {
    fn eq(&self, other: &Self) -> bool {
        debug_assert_eq!(self.kind, other.kind);
        self.name.eq(&other.name)
    }
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub id: Id,
    pub node: Option<Id>,
}

impl<K> Var<K> {
    pub fn new(name: String, kind: K) -> Self {
        Self { name, kind }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

pub struct Ctx<Node: NodeType> {
    nodes: Vec<Node>,
    vars: Vec<Var<Node::Kind>>,
    spans: Vec<Span>,
}

impl<Node: NodeType> Ctx<Node> {
    fn add_node(&mut self, node: Spanned<Node>) -> Id {
        self.spans.push(node.span());
        self.nodes.push(node.take());
        Id(InternalId::Real(self.nodes.len() - 1))
    }

    fn num_vars(&self) -> usize {
        self.vars.len()
    }

    pub fn add<T: ToNode<Node>>(&mut self, t: Spanned<T>) -> NodeId<T> {
        self.add_node(t.map(|item| item.to_node())).typed()
    }

    fn add_var_internal(&mut self, var: Var<Node::Kind>) -> Id {
        self.vars.push(var);
        Id(InternalId::Pat(self.vars.len() - 1))
    }

    pub fn add_var<T: ToNode<Node>>(&mut self, var: Var<Node::Kind>) -> NodeId<T> {
        let id = if let Some((id, _)) = self.get_var_by_name(&var.name) {
            id
        } else {
            self.add_var_internal(var)
        };
        id.typed()
    }

    pub fn add_pat<T: ToNode<Node>>(&mut self, item: SpannedPat<T>) -> NodeId<T> {
        match item.item {
            Pattern::Real(_) => self.add(item.unwrap_real()),
            Pattern::Pat(var) => var.typed(),
        }
    }

    pub fn get<T: ToNode<Node>>(&self, id: impl Into<Id>) -> Pattern<&T, Id> {
        let id = id.into();
        match id.0 {
            InternalId::Real(idx) => Pattern::Real(T::from_node_ref(&self.nodes[idx]).unwrap()),
            InternalId::Pat(_) => Pattern::Pat(id),
        }
    }

    fn remove<T: ToNode<Node>>(mut self, id: impl Into<Id>) -> Pattern<T, Id> {
        let id = id.into();
        match id.0 {
            InternalId::Real(idx) => Pattern::Real(T::from_node(self.nodes.remove(idx)).unwrap()),
            InternalId::Pat(_) => Pattern::Pat(id),
        }
    }

    pub fn get_mut<T: ToNode<Node>>(&mut self, id: NodeId<T>) -> Pattern<&mut T, Id> {
        let id: Id = id.into();
        match id.0 {
            InternalId::Real(idx) => {
                Pattern::Real(T::from_node_ref_mut(&mut self.nodes[idx]).unwrap())
            }
            InternalId::Pat(_) => Pattern::Pat(id),
        }
    }

    pub fn get_real<T: ToNode<Node>>(&self, id: impl Into<Id>) -> Option<&T> {
        match self.get(id) {
            Pattern::Real(t) => Some(t),
            Pattern::Pat(_) => None,
        }
    }

    fn get_real_mut<T: ToNode<Node>>(&mut self, id: NodeId<T>) -> Option<&mut T> {
        match self.get_mut(id) {
            Pattern::Real(t) => Some(t),
            Pattern::Pat(_) => None,
        }
    }

    pub fn get_var(&self, id: Id) -> &Var<Node::Kind> {
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

    fn get_var_by_name(&self, name: &str) -> Option<(Id, &Var<Node::Kind>)> {
        self.vars
            .iter()
            .enumerate()
            .find(|(_, var)| var.name == name)
            .map(|(i, var)| (Id(InternalId::Pat(i)), var))
    }

    pub fn get_kind_by_name(&self, name: &str) -> Node::Kind {
        self.get_var_by_name(name).unwrap().1.kind
    }

    pub fn get_id_by_name(&self, name: &str) -> Id {
        self.get_var_by_name(name).unwrap().0
    }

    pub fn iter(&self) -> impl Iterator<Item = Id> {
        (0..self.nodes.len()).map(|id| Id(InternalId::Real(id)))
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = &Var<Node::Kind>> {
        self.vars.iter()
    }

    pub fn get_span(&self, id: impl Into<Id>) -> Span {
        match id.into().0 {
            Pattern::Real(idx) => self.spans[idx],
            Pattern::Pat(_) => panic!(),
        }
    }

    pub fn print<'a>(&'a self, id: Id, src: &'a str) -> &'a str {
        let span = self.get_span(id);
        &src[span.byte_range()]
    }
}

impl<Node: NodeType> Default for Ctx<Node> {
    fn default() -> Self {
        Self {
            nodes: vec![],
            vars: vec![],
            spans: vec![],
        }
    }
}
