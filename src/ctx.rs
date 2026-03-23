//! Defines the main types that store parsed syntax nodes.
use std::marker::PhantomData;

use crate::{
    NodeType, Span, Spanned, Term, ToNode, rust_grammar::Ident, span::SpannedTerm, storage::Storage,
};

type InternalId = Term<usize, usize>;

/// Used in the parsing logic and the AST context
/// to remember whether we are currently parsing
/// 1. real source code that we are running on
/// 2. the molt file and its basic grammar
/// 3. a pattern within a molt file.
///
/// Note that molt variables (like $expr) can
/// only occur in the `MoltPat` mode.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Mode {
    /// We are within the real source code.
    Real,
    /// We are in the molt code.
    Molt,
    /// We are within a pattern in the molt code.
    MoltPat,
}

/// The main ID type. An `Id` uniquely identifies
/// either a concrete syntax node or a pattern variable
/// for a given `Ctx`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RawNodeId(InternalId, Mode);

impl RawNodeId {
    pub(crate) fn is_var(&self) -> bool {
        match self.0 {
            Term::Item(_) => false,
            Term::Var(_) => true,
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
            Term::Item(idx) => idx,
            Term::Var(idx) => idx,
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

impl<K> VarKind<K> {
    pub fn is_list(&self) -> bool {
        matches!(self, VarKind::List(_))
    }
}

impl<K: Copy> CtxVar<K> {
    pub(crate) fn new(ident: Ident, kind: VarKind<K>) -> Self {
        Self { ident, kind }
    }

    pub(crate) fn ident(&self) -> &Ident {
        &self.ident
    }

    pub(crate) fn kind(&self) -> &VarKind<K> {
        &self.kind
    }
}

pub(crate) struct Ctx<Node: NodeType> {
    nodes: Storage<usize, Node>,
    vars: Storage<usize, CtxVar<Node::NodeKind>>,
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

    fn add_var_internal(&mut self, var: CtxVar<Node::NodeKind>) -> RawNodeId {
        let id = self.vars.add(var);
        RawNodeId(InternalId::Var(id), self.mode)
    }

    fn add_var_untyped(&mut self, var: CtxVar<Node::NodeKind>) -> RawNodeId {
        if let Some((id, _)) = self.get_var_by_name(var.ident()) {
            id
        } else {
            self.add_var_internal(var)
        }
    }

    pub(crate) fn add_var<T: ToNode<Node>>(&mut self, var: CtxVar<Node::NodeKind>) -> NodeId<T> {
        let id = self.add_var_untyped(var);
        id.typed()
    }

    pub(crate) fn add_term<T: ToNode<Node>>(&mut self, item: SpannedTerm<T>) -> NodeId<T> {
        match item.item {
            Term::Item(_) => self.add(item.unwrap_item()),
            Term::Var(var) => var.typed(),
        }
    }

    pub(crate) fn get<T: ToNode<Node>>(&self, id: impl Into<RawNodeId>) -> Term<&T, RawNodeId> {
        let id = id.into();
        debug_assert_eq!(id.mode(), self.mode);
        match id.0 {
            InternalId::Item(idx) => Term::Item(T::from_node_ref(&self.nodes[idx]).unwrap()),
            InternalId::Var(_) => Term::Var(id),
        }
    }

    pub(crate) fn get_mut<T: ToNode<Node>>(&mut self, id: NodeId<T>) -> Term<&mut T, RawNodeId> {
        let id: RawNodeId = id.into();
        debug_assert_eq!(id.mode(), self.mode);
        match id.0 {
            InternalId::Item(idx) => {
                Term::Item(T::from_node_ref_mut(&mut self.nodes[idx]).unwrap())
            }
            InternalId::Var(_) => Term::Var(id),
        }
    }

    pub(crate) fn get_real<T: ToNode<Node>>(&self, id: impl Into<RawNodeId>) -> Option<&T> {
        match self.get(id) {
            Term::Item(t) => Some(t),
            Term::Var(_) => None,
        }
    }

    pub(crate) fn get_var(&self, id: RawNodeId) -> &CtxVar<Node::NodeKind> {
        match id.0 {
            InternalId::Item(_) => panic!(),
            InternalId::Var(idx) => &self.vars[idx],
        }
    }

    fn get_var_by_name(&self, ident: &Ident) -> Option<(RawNodeId, &CtxVar<Node::NodeKind>)> {
        self.vars
            .iter()
            .enumerate()
            .find(|(_, var)| var.ident() == ident)
            .map(|(i, var)| (RawNodeId(InternalId::Var(i), self.mode), var))
    }

    pub(crate) fn get_kind_by_name(&self, name: &Ident) -> VarKind<Node::NodeKind> {
        self.get_var_by_name(name).unwrap().1.kind
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = RawNodeId> {
        (0..self.nodes.len()).map(|id| RawNodeId(InternalId::Item(id), self.mode))
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = &CtxVar<Node::NodeKind>> {
        self.vars.iter()
    }

    pub(crate) fn get_span(&self, id: impl Into<RawNodeId>) -> Span {
        let id: RawNodeId = id.into();
        debug_assert_eq!(id.mode(), self.mode);
        match id.0 {
            Term::Item(idx) => self.spans[idx],
            Term::Var(_) => panic!(),
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
