use std::marker::PhantomData;

use crate::{
    Mode, NodeType, Pattern, Span, Spanned, ToNode, rust_grammar::Ident, span::SpannedPat,
};

type InternalId = Pattern<usize, usize>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Id(InternalId, Mode);

impl Id {
    pub(crate) fn is_var(&self) -> bool {
        match self.0 {
            Pattern::Item(_) => false,
            Pattern::Var(_) => true,
        }
    }

    pub(crate) fn mode(&self) -> Mode {
        self.1
    }
}

pub(crate) struct NodeId<T> {
    _marker: PhantomData<T>,
    id: Id,
}

impl<T> NodeId<T> {
    // used instead of Expr::PLACEHOLDER in parsing.
    pub(crate) fn placeholder() -> Self {
        Self {
            id: Id(InternalId::Item(usize::MAX), Mode::Real),
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

    pub(crate) fn unwrap_idx(self) -> usize {
        match self.0 {
            Pattern::Item(idx) => idx,
            Pattern::Var(idx) => idx,
        }
    }
}

pub(crate) struct Var<K> {
    // TODO: Pick one of name / ident or
    // replace with `NodeId<Ident>`
    name: String,
    ident: Ident,
    kind: K,
}

impl<K: std::fmt::Debug + PartialEq> PartialEq for Var<K> {
    fn eq(&self, other: &Self) -> bool {
        debug_assert_eq!(self.kind, other.kind);
        self.name.eq(&other.name)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct VarDecl {
    pub(crate) id: Id,
    pub(crate) node: Option<Id>,
}

impl<K: Copy> Var<K> {
    pub(crate) fn new(ident: Ident, kind: K) -> Self {
        Self {
            name: ident.to_string(),
            ident,
            kind,
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn kind(&self) -> K {
        self.kind
    }

    pub(crate) fn ident(&self) -> &Ident {
        &self.ident
    }
}

pub(crate) struct Ctx<Node: NodeType> {
    nodes: Vec<Node>,
    vars: Vec<Var<Node::Kind>>,
    spans: Vec<Span>,
    mode: Mode,
}

impl<Node: NodeType> Ctx<Node> {
    fn add_node(&mut self, node: Spanned<Node>) -> Id {
        self.spans.push(node.span());
        self.nodes.push(node.item());
        Id(InternalId::Item(self.nodes.len() - 1), self.mode)
    }

    pub(crate) fn add<T: ToNode<Node>>(&mut self, t: Spanned<T>) -> NodeId<T> {
        self.add_node(t.map(|item| item.to_node())).typed()
    }

    fn add_var_internal(&mut self, var: Var<Node::Kind>) -> Id {
        self.vars.push(var);
        Id(InternalId::Var(self.vars.len() - 1), self.mode)
    }

    pub(crate) fn add_var<T: ToNode<Node>>(&mut self, var: Var<Node::Kind>) -> NodeId<T> {
        let id = if let Some((id, _)) = self.get_var_by_name(&var.name) {
            id
        } else {
            self.add_var_internal(var)
        };
        id.typed()
    }

    pub(crate) fn add_pat<T: ToNode<Node>>(&mut self, item: SpannedPat<T>) -> NodeId<T> {
        match item.item {
            Pattern::Item(_) => self.add(item.unwrap_real()),
            Pattern::Var(var) => var.typed(),
        }
    }

    pub(crate) fn get<T: ToNode<Node>>(&self, id: impl Into<Id>) -> Pattern<&T, Id> {
        let id = id.into();
        match id.0 {
            InternalId::Item(idx) => Pattern::Item(T::from_node_ref(&self.nodes[idx]).unwrap()),
            InternalId::Var(_) => Pattern::Var(id),
        }
    }

    pub(crate) fn get_mut<T: ToNode<Node>>(&mut self, id: NodeId<T>) -> Pattern<&mut T, Id> {
        let id: Id = id.into();
        match id.0 {
            InternalId::Item(idx) => {
                Pattern::Item(T::from_node_ref_mut(&mut self.nodes[idx]).unwrap())
            }
            InternalId::Var(_) => Pattern::Var(id),
        }
    }

    pub(crate) fn get_real<T: ToNode<Node>>(&self, id: impl Into<Id>) -> Option<&T> {
        match self.get(id) {
            Pattern::Item(t) => Some(t),
            Pattern::Var(_) => None,
        }
    }

    pub(crate) fn get_var(&self, id: Id) -> &Var<Node::Kind> {
        match id.0 {
            InternalId::Item(_) => panic!(),
            InternalId::Var(idx) => &self.vars[idx],
        }
    }

    fn get_var_by_name(&self, name: &str) -> Option<(Id, &Var<Node::Kind>)> {
        self.vars
            .iter()
            .enumerate()
            .find(|(_, var)| var.name == name)
            .map(|(i, var)| (Id(InternalId::Var(i), self.mode), var))
    }

    pub(crate) fn get_kind_by_name(&self, name: &str) -> Node::Kind {
        self.get_var_by_name(name).unwrap().1.kind
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = Id> {
        (0..self.nodes.len()).map(|id| Id(InternalId::Item(id), self.mode))
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = &Var<Node::Kind>> {
        self.vars.iter()
    }

    pub(crate) fn iter_vars_ids(&self) -> impl Iterator<Item = (Id, &Var<Node::Kind>)> {
        self.vars
            .iter()
            .enumerate()
            .map(|(i, v)| (Id(InternalId::Var(i), self.mode), v))
    }

    pub(crate) fn get_span(&self, id: impl Into<Id>) -> Span {
        match id.into().0 {
            Pattern::Item(idx) => self.spans[idx],
            Pattern::Var(_) => panic!(),
        }
    }

    pub(crate) fn print<'a>(&'a self, id: Id, src: &'a str) -> &'a str {
        let span = self.get_span(id);
        &src[span.byte_range()]
    }
}

impl<Node: NodeType> Ctx<Node> {
    pub(crate) fn new(mode: Mode) -> Self {
        Self {
            nodes: vec![],
            vars: vec![],
            spans: vec![],
            mode,
        }
    }
}
