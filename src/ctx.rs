use std::marker::PhantomData;

use crate::{
    molt_grammar::{Var, VarId},
    node::{Kind, Node, Pattern, ToNode},
};
use rust_grammar::{Mode, Span, Spanned};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct Id(InternalId);

// TODO: This distinction exists only to make sure we index into the
// correct context everywhere. We can get rid of it on release builds.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum InternalId {
    AstNode(usize),
    PatNode(usize),
    Var(usize),
}

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
        self.id.fmt(f)
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

#[derive(Default)]
pub(crate) struct AstCtx {
    ctx: Ctx,
}

#[derive(Default)]
pub(crate) struct PatCtx {
    ctx: Ctx,
}

#[derive(Default)]
pub(crate) struct Ctx {
    vars: Vec<Var>,
    nodes: Vec<Node>,
    kinds: Vec<Kind>,
    spans: Vec<Span>,
}

impl Ctx {
    fn add_var_internal(&mut self, var: Var) -> Id {
        self.vars.push(var);
        Id(InternalId::Var(self.vars.len() - 1))
    }

    pub(crate) fn add_var(&mut self, var: Var) -> VarId {
        if let Some(var) = self
            .vars
            .iter()
            .enumerate()
            .find(|(_, v)| var.ident() == v.ident())
            .map(|(i, _)| Id(InternalId::Var(i)))
        {
            VarId(var)
        } else {
            VarId(self.add_var_internal(var))
        }
    }

    pub(crate) fn add_var_typed<T: ToNode>(&mut self, var: Var) -> NodeId<T> {
        // TODO: Check kind is correct once we track
        // all the var kinds.
        self.add_var(var).0.typed()
    }

    fn add_node_internal(&mut self, node: Spanned<Node>) -> usize {
        self.spans.push(node.span);
        self.kinds.push(node.item.kind());
        self.nodes.push(node.item);
        self.nodes.len() - 1
    }

    pub(crate) fn add_node(&mut self, node: Spanned<Node>, mode: Mode) -> Id {
        let id = self.add_node_internal(node);
        match mode {
            Mode::Molt => Id(InternalId::PatNode(id)),
            Mode::Rust => Id(InternalId::AstNode(id)),
        }
    }

    pub(crate) fn add<T: ToNode>(&mut self, t: Spanned<T>, mode: Mode) -> NodeId<T> {
        self.add_node(t.map(|item| item.to_node()), mode).typed()
    }

    fn iter(&self) -> impl Iterator<Item = usize> {
        0..self.nodes.len()
    }
}

impl AstCtx {
    pub(crate) fn new(ctx: Ctx) -> Self {
        assert!(ctx.vars.is_empty());
        Self { ctx }
    }

    pub(crate) fn add<T: ToNode>(&mut self, t: Spanned<T>) -> NodeId<T> {
        Id(InternalId::AstNode(
            self.ctx.add_node_internal(t.map(|item| item.to_node())),
        ))
        .typed()
    }

    pub(crate) fn get_node(&self, id: Id) -> &Node {
        match id.0 {
            InternalId::AstNode(idx) => &self.ctx.nodes[idx],
            InternalId::PatNode(_) => unreachable!(),
            InternalId::Var(_) => unreachable!(),
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = Id> {
        self.ctx.iter().map(|id| Id(InternalId::AstNode(id)))
    }
}

impl PatCtx {
    pub(crate) fn new(ctx: Ctx) -> Self {
        Self { ctx }
    }

    pub(crate) fn get_node(&self, id: Id) -> &Node {
        match id.0 {
            InternalId::PatNode(idx) => &self.ctx.nodes[idx],
            InternalId::AstNode(_) => unreachable!(),
            InternalId::Var(_) => unreachable!(),
        }
    }

    pub(crate) fn get_pat_node(&self, id: Id) -> Pattern<&Node> {
        match id.0 {
            InternalId::PatNode(idx) => Pattern::Exact(&self.ctx.nodes[idx]),
            InternalId::Var(_) => return Pattern::Pattern(VarId::new(id)),
            _ => unreachable!(),
        }
    }

    pub(crate) fn get_var(&self, id: VarId) -> &Var {
        match id.0.0 {
            InternalId::PatNode(_) => unreachable!(),
            InternalId::AstNode(_) => unreachable!(),
            InternalId::Var(var) => &self.ctx.vars[var],
        }
    }
}

