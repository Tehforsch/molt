use std::marker::PhantomData;

use crate::{
    convert::Convert,
    grammar::{Node, ToNode},
    mangle::{Pattern, Unmangle},
    spec::SynVar,
    CustomDebug,
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
    fn convert<S, T: Convert<S>>(&mut self, t: T) -> S;
    fn add_convert<S: ToNode, T: Convert<S> + Unmangle>(&mut self, t: T) -> NodeId<S>;

    fn add_convert_list<S: ToNode, T: Convert<S> + Unmangle>(
        &mut self,
        t: impl IntoIterator<Item = T>,
    ) -> NodeList<S> {
        let items = t.into_iter().map(|item| self.add_convert(item)).collect();
        NodeList {
            items,
            matching_mode: MatchingMode::ContainsAll,
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

impl ConvertCtx for AstCtx {
    fn convert<S, T: Convert<S>>(&mut self, t: T) -> S {
        t.convert(self)
    }

    fn add_convert<S: ToNode, T: Convert<S> + Unmangle>(&mut self, t: T) -> NodeId<S> {
        let s = t.convert(self);
        self.add(s)
    }
}

impl PatCtx {
    fn add_var_internal(&mut self, var: SynVar) -> Id {
        self.vars.push(var);
        Id(InternalId::Var(self.vars.len() - 1))
    }

    fn add_var<T: ToNode>(&mut self, var: SynVar) -> NodeId<T> {
        if self.vars.contains(&var) {
            todo!("Merge duplicates")
        }
        self.add_var_internal(var).typed()
    }

    pub(crate) fn add<T: ToNode>(&mut self, t: T) -> NodeId<T> {
        Id(InternalId::PatNode(self.ctx.add_node(t.to_node()))).typed()
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

    fn add_convert<S: ToNode, T: Convert<S> + Unmangle>(&mut self, t: T) -> NodeId<S> {
        match T::unmangle(t) {
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

    pub(crate) fn get<T: ToNode>(&self, id: NodeId<T>) -> Pattern<&T> {
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

    pub(crate) fn dump(&self) {
        println!("--------------------------------");
        for idx in self.ast_ctx.ctx.iter() {
            let node = self.ast_ctx.get_node(Id(InternalId::AstNode(idx)));
            let kind_str = format!("{}", node.kind());
            println!("AstNode({:02}): {:13} = {}", idx, kind_str, node.deb(self));
        }
        println!("--------------------------------");
        for idx in self.pat_ctx.ctx.iter() {
            let node = self.pat_ctx.get_node(Id(InternalId::PatNode(idx)));
            let kind_str = format!("{}", node.kind());
            println!("PatNode({:02}): {:13} = {}", idx, kind_str, node.deb(self));
        }
        println!("--------------------------------");
        for (idx, var) in self.pat_ctx.vars.iter().enumerate() {
            println!("PatVar({:02}): {:14} = {}", idx, "", var.name);
        }
    }
}
