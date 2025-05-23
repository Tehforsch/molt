use std::marker::PhantomData;

use crate::parser::{Mode, Node, Pattern, Span, ToNode, Var, VarDecl, VarId};

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

pub(crate) struct NodeList<T> {
    pub items: Vec<NodeId<T>>,
    pub matching_mode: MatchingMode,
}

impl<T> NodeList<T> {
    pub(crate) fn iter(&self) -> impl Iterator<Item = &NodeId<T>> {
        self.items.iter()
    }

    pub(crate) fn len(&self) -> usize {
        self.items.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    #[allow(unused)]
    pub(crate) fn get(&self, idx: usize) -> Option<&NodeId<T>> {
        self.items.get(idx)
    }
}

pub(crate) enum MatchingMode {
    #[allow(unused)]
    Exact,
    // ContainsAllInOrder,
    ContainsAll,
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
    vars: Vec<VarId>,
}

#[derive(Default)]
pub(crate) struct Ctx {
    nodes: Vec<Node>,
}

impl Ctx {
    fn add_node(&mut self, node: Node) -> usize {
        self.nodes.push(node);
        self.nodes.len() - 1
    }

    pub(crate) fn add<T: ToNode>(&mut self, t: T, mode: Mode) -> NodeId<T> {
        Id(InternalId::AstNode(self.add_node(t.to_node()))).typed()
    }

    fn iter(&self) -> impl Iterator<Item = usize> {
        0..self.nodes.len()
    }
}

impl AstCtx {
    pub(crate) fn new(ctx: Ctx) -> Self {
        Self { ctx }
    }

    pub(crate) fn add<T: ToNode>(&mut self, t: T) -> NodeId<T> {
        Id(InternalId::AstNode(self.ctx.add_node(t.to_node()))).typed()
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
        Self { ctx, vars: vec![] }
    }

    fn add_var_internal(&mut self, var: VarId) -> Id {
        self.vars.push(var);
        Id(InternalId::Var(self.vars.len() - 1))
    }

    fn add_var<T: ToNode>(&mut self, var: VarId) -> NodeId<T> {
        let id = if let Some(var) = self
            .vars
            .iter()
            .enumerate()
            .find(|(_, v)| var == **v)
            .map(|(i, _)| Id(InternalId::Var(i)))
        {
            var
        } else {
            self.add_var_internal(var)
        };
        id.typed()
    }

    pub(crate) fn add<T: ToNode>(&mut self, t: T) -> NodeId<T> {
        Id(InternalId::PatNode(self.ctx.add_node(t.to_node()))).typed()
    }

    pub(crate) fn add_node(&mut self, node: Node) -> Id {
        Id(InternalId::PatNode(self.ctx.add_node(node)))
    }

    pub(crate) fn get_node(&self, id: Id) -> &Node {
        match id.0 {
            InternalId::PatNode(idx) => &self.ctx.nodes[idx],
            InternalId::AstNode(_) => unreachable!(),
            InternalId::Var(_) => unreachable!(),
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

    pub(crate) fn get<T: ToNode>(&self, id: NodeId<T>) -> Pattern<&T> {
        let node = match id.id.0 {
            InternalId::AstNode(idx) => &self.ast_ctx.ctx.nodes[idx],
            InternalId::PatNode(idx) => &self.pat_ctx.ctx.nodes[idx],
            InternalId::Var(idx) => return Pattern::Pattern(self.pat_ctx.vars[idx]),
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

    pub(crate) fn get_pattern(&self, id: Id) -> Pattern<&Node> {
        match id.0 {
            InternalId::PatNode(node) => Pattern::Exact(&self.pat_ctx.ctx.nodes[node]),
            InternalId::Var(var) => Pattern::Pattern(self.pat_ctx.vars[var].clone()),
            InternalId::AstNode(node) => Pattern::Exact(&self.ast_ctx.ctx.nodes[node]),
        }
    }

    pub(crate) fn get_span(&self, id: Id) -> Option<Span> {
        todo!()
    }

    pub(crate) fn get_var(&self, var: VarId) -> Var {
        todo!()
    }

    #[cfg(feature = "debug-print")]
    pub(crate) fn dump(&self) {
        todo!()
        // use crate::grammar::CustomDebug;
        // println!("--------------------------------");
        // for idx in self.ast_ctx.ctx.iter() {
        //     let node = self.ast_ctx.get_node(Id(InternalId::AstNode(idx)));
        //     let kind_str = format!("{}", node.kind());
        //     println!("AstNode({:02}): {:13} = {}", idx, kind_str, node.deb(self));
        // }
        // println!("--------------------------------");
        // for idx in self.pat_ctx.ctx.iter() {
        //     let node = self.pat_ctx.get_node(Id(InternalId::PatNode(idx)));
        //     let kind_str = format!("{}", node.kind());
        //     println!("PatNode({:02}): {:13} = {}", idx, kind_str, node.deb(self));
        // }
        // println!("--------------------------------");
        // for (idx, var) in self.pat_ctx.vars.iter().enumerate() {
        //     println!("PatVar({:02}): {:14} = {}", idx, "", var.name);
        // }
    }
}
