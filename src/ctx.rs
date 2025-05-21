use std::marker::PhantomData;

use crate::{
    convert::Convert,
    grammar::{AsNode, Node},
    mangle::{FromPlaceholder, Pattern},
    spec::SynVar,
};

#[derive(Copy, Clone, Debug)]
pub(crate) struct Id(InternalId);

// TODO: This distinction exists only to make sure we index into the
// correct context everywhere. We can get rid of it on release builds.
#[derive(Copy, Clone, Debug)]
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
        Self {
            _marker: PhantomData,
            id: self.id,
        }
    }
}

impl<T> Copy for NodeId<T> {}

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

pub(crate) trait ConvertCtx {
    fn add_convert<S: AsNode, T: Convert<S> + FromPlaceholder>(&mut self, t: T) -> NodeId<S>;
    fn convert<S, T: Convert<S>>(&mut self, t: T) -> S;
}

#[derive(Default)]
pub(crate) struct AstCtx {
    ctx: Ctx,
}

#[derive(Default)]
pub(crate) struct PatCtx {
    ctx: Ctx,
    vars: Vec<SynVar>,
}

#[derive(Default)]
struct Ctx {
    nodes: Vec<Node>,
}

impl Ctx {
    fn add_node(&mut self, node: Node) -> usize {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    fn iter(&self) -> impl Iterator<Item = usize> {
        0..self.nodes.len()
    }
}

impl AstCtx {
    pub(crate) fn add<T: AsNode>(&mut self, t: T) -> NodeId<T> {
        Id(InternalId::AstNode(self.ctx.add_node(t.as_node()))).typed()
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

impl ConvertCtx for AstCtx {
    fn convert<S, T: Convert<S>>(&mut self, t: T) -> S {
        t.convert(self)
    }

    fn add_convert<S: AsNode, T: Convert<S> + FromPlaceholder>(&mut self, t: T) -> NodeId<S> {
        let s = t.convert(self);
        self.add(s)
    }
}

impl PatCtx {
    fn add_var_internal(&mut self, var: SynVar) -> Id {
        self.vars.push(var);
        Id(InternalId::Var(self.vars.len() - 1))
    }

    fn add_var<T: AsNode>(&mut self, var: SynVar) -> NodeId<T> {
        if self.vars.contains(&var) {
            todo!("Merge duplicates")
        }
        self.add_var_internal(var).typed()
    }

    pub(crate) fn add<T: AsNode>(&mut self, t: T) -> NodeId<T> {
        Id(InternalId::PatNode(self.ctx.add_node(t.as_node()))).typed()
    }

    pub(crate) fn add_node(&mut self, node: Node) -> Id {
        Id(InternalId::PatNode(self.ctx.add_node(node)))
    }

    pub(crate) fn get_pattern(&self, id: Id) -> Pattern<&Node> {
        match id.0 {
            InternalId::PatNode(node) => Pattern::Exact(&self.ctx.nodes[node]),
            InternalId::Var(var) => Pattern::Pattern(self.vars[var].clone()),
            InternalId::AstNode(_) => unreachable!(),
        }
    }

    pub(crate) fn get_node(&self, id: Id) -> &Node {
        match id.0 {
            InternalId::PatNode(idx) => &self.ctx.nodes[idx],
            InternalId::AstNode(_) => unreachable!(),
            InternalId::Var(_) => unreachable!(),
        }
    }
}

impl ConvertCtx for PatCtx {
    fn convert<S, T: Convert<S>>(&mut self, t: T) -> S {
        t.convert(self)
    }

    fn add_convert<S: AsNode, T: Convert<S> + FromPlaceholder>(&mut self, t: T) -> NodeId<S> {
        match T::from_placeholder(t) {
            Pattern::Exact(t) => {
                let s = t.convert(self);
                self.add(s)
            }
            Pattern::Pattern(syn_var) => self.add_var(syn_var),
        }
    }
}

pub(crate) struct MatchCtx {
    pub pat_ctx: PatCtx,
    pub ast_ctx: AstCtx,
}

impl MatchCtx {
    pub(crate) fn new(pat_ctx: PatCtx, ast_ctx: AstCtx) -> Self {
        Self { pat_ctx, ast_ctx }
    }

    pub(crate) fn get<T: AsNode>(&self, id: NodeId<T>) -> Pattern<&T> {
        let node = match id.id.0 {
            InternalId::AstNode(idx) => &self.ast_ctx.ctx.nodes[idx],
            InternalId::PatNode(idx) => &self.pat_ctx.ctx.nodes[idx],
            InternalId::Var(idx) => return Pattern::Pattern(self.pat_ctx.vars[idx].clone()),
        };
        Pattern::Exact(T::from_node(node).unwrap())
    }

    pub(crate) fn get_node(&self, id: Id) -> Option<&Node> {
        match id.0 {
            InternalId::AstNode(idx) => Some(&self.ast_ctx.ctx.nodes[idx]),
            InternalId::PatNode(idx) => Some(&self.pat_ctx.ctx.nodes[idx]),
            InternalId::Var(_) => None,
        }
    }
}
