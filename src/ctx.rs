//! Defines the main types that store parsed syntax nodes.
use std::marker::PhantomData;

use crate::{
    ItemOrVar, NodeType, Span, Spanned, ToNode, rust_grammar::Ident, span::SpannedPat,
    storage::Storage,
};

type InternalId = ItemOrVar<usize, usize>;

/// Used in the parsing logic and the AST context
/// to remember whether an item or variable belongs
/// to real source code or code within molt patterns.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Mode {
    /// Represents something within the real source code.
    Real,
    /// Represents something within a pattern in the molt code.
    Molt,
}

/// The main ID type. An `Id` uniquely identifies
/// either a concrete syntax node or a pattern variable
/// for a given `Ctx`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RawNodeId(InternalId, Mode);

impl RawNodeId {
    pub(crate) fn is_var(&self) -> bool {
        match self.0 {
            ItemOrVar::Item(_) => false,
            ItemOrVar::Var(_) => true,
        }
    }

    pub(crate) fn mode(&self) -> Mode {
        self.1
    }
}

/// `NodeId<T>` is a strongly typed variant of the
/// `Id` type. Used in the AST to retain type information.
pub(crate) struct NodeId<T> {
    _marker: PhantomData<T>,
    id: RawNodeId,
}

impl<T> NodeId<T> {
    // used instead of Expr::PLACEHOLDER in parsing.
    pub(crate) fn placeholder() -> Self {
        Self {
            id: RawNodeId(InternalId::Item(usize::MAX), Mode::Real),
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

impl<T> From<NodeId<T>> for RawNodeId {
    fn from(value: NodeId<T>) -> Self {
        value.id
    }
}

impl RawNodeId {
    pub(crate) fn typed<T>(self) -> NodeId<T> {
        NodeId {
            id: self,
            _marker: PhantomData,
        }
    }

    pub(crate) fn unwrap_idx(self) -> usize {
        match self.0 {
            ItemOrVar::Item(idx) => idx,
            ItemOrVar::Var(idx) => idx,
        }
    }
}

/// The real representation of a pattern variable.
/// Contains its name and the kind of the variable.
pub(crate) struct CtxVar<K> {
    ident: Ident,
    kind: VarKind<K>,
}

#[derive(Clone, Copy, Debug)]
pub enum VarKind<K> {
    Single(K),
    List(K),
}

impl<K: Copy> CtxVar<K> {
    pub(crate) fn new(ident: Ident, kind: VarKind<K>) -> Self {
        Self { ident, kind }
    }

    pub(crate) fn ident(&self) -> &Ident {
        &self.ident
    }
}

pub(crate) struct Ctx<Node: NodeType> {
    nodes: Storage<usize, Node>,
    vars: Storage<usize, CtxVar<Node::Kind>>,
    spans: Storage<usize, Span>,
    mode: Mode,
}

impl<Node: NodeType> Ctx<Node> {
    fn add_node(&mut self, node: Spanned<Node>) -> RawNodeId {
        let span_id = self.spans.add(node.span());
        let id = self.nodes.add(node.item());
        debug_assert_eq!(span_id, id);
        RawNodeId(InternalId::Item(id), self.mode)
    }

    pub(crate) fn add<T: ToNode<Node>>(&mut self, t: Spanned<T>) -> NodeId<T> {
        self.add_node(t.map(|item| item.to_node())).typed()
    }

    fn add_var_internal(&mut self, var: CtxVar<Node::Kind>) -> RawNodeId {
        let id = self.vars.add(var);
        RawNodeId(InternalId::Var(id), self.mode)
    }

    fn add_var_untyped(&mut self, var: CtxVar<Node::Kind>) -> RawNodeId {
        if let Some((id, _)) = self.get_var_by_name(var.ident()) {
            id
        } else {
            self.add_var_internal(var)
        }
    }

    pub(crate) fn add_var<T: ToNode<Node>>(&mut self, var: CtxVar<Node::Kind>) -> NodeId<T> {
        let id = self.add_var_untyped(var);
        id.typed()
    }

    pub(crate) fn add_pat<T: ToNode<Node>>(&mut self, item: SpannedPat<T>) -> NodeId<T> {
        match item.item {
            ItemOrVar::Item(_) => self.add(item.unwrap_real()),
            ItemOrVar::Var(var) => var.typed(),
        }
    }

    pub(crate) fn get<T: ToNode<Node>>(
        &self,
        id: impl Into<RawNodeId>,
    ) -> ItemOrVar<&T, RawNodeId> {
        let id = id.into();
        debug_assert_eq!(id.mode(), self.mode);
        match id.0 {
            InternalId::Item(idx) => ItemOrVar::Item(T::from_node_ref(&self.nodes[idx]).unwrap()),
            InternalId::Var(_) => ItemOrVar::Var(id),
        }
    }

    pub(crate) fn get_mut<T: ToNode<Node>>(
        &mut self,
        id: NodeId<T>,
    ) -> ItemOrVar<&mut T, RawNodeId> {
        let id: RawNodeId = id.into();
        debug_assert_eq!(id.mode(), self.mode);
        match id.0 {
            InternalId::Item(idx) => {
                ItemOrVar::Item(T::from_node_ref_mut(&mut self.nodes[idx]).unwrap())
            }
            InternalId::Var(_) => ItemOrVar::Var(id),
        }
    }

    pub(crate) fn get_real<T: ToNode<Node>>(&self, id: impl Into<RawNodeId>) -> Option<&T> {
        match self.get(id) {
            ItemOrVar::Item(t) => Some(t),
            ItemOrVar::Var(_) => None,
        }
    }

    pub(crate) fn get_var(&self, id: RawNodeId) -> &CtxVar<Node::Kind> {
        match id.0 {
            InternalId::Item(_) => panic!(),
            InternalId::Var(idx) => &self.vars[idx],
        }
    }

    fn get_var_by_name(&self, ident: &Ident) -> Option<(RawNodeId, &CtxVar<Node::Kind>)> {
        self.vars
            .iter()
            .enumerate()
            .find(|(_, var)| var.ident() == ident)
            .map(|(i, var)| (RawNodeId(InternalId::Var(i), self.mode), var))
    }

    pub(crate) fn get_kind_by_name(&self, name: &Ident) -> VarKind<Node::Kind> {
        self.get_var_by_name(name).unwrap().1.kind
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = RawNodeId> {
        (0..self.nodes.len()).map(|id| RawNodeId(InternalId::Item(id), self.mode))
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = &CtxVar<Node::Kind>> {
        self.vars.iter()
    }

    pub(crate) fn get_span(&self, id: impl Into<RawNodeId>) -> Span {
        let id: RawNodeId = id.into();
        debug_assert_eq!(id.mode(), self.mode);
        match id.0 {
            ItemOrVar::Item(idx) => self.spans[idx],
            ItemOrVar::Var(_) => panic!(),
        }
    }

    pub(crate) fn print<'a>(&'a self, id: RawNodeId, src: &'a str) -> &'a str {
        let span = self.get_span(id);
        &src[span.byte_range()]
    }
}

impl<Node: NodeType> Ctx<Node> {
    pub(crate) fn new(mode: Mode) -> Self {
        Self {
            nodes: Storage::default(),
            vars: Storage::default(),
            spans: Storage::default(),
            mode,
        }
    }
}
