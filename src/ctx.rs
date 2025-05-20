use std::marker::PhantomData;

use crate::{
    convert::Convert,
    grammar::{AsNode, Node},
    mangle::{FromPlaceholder, Pattern},
    spec::SynVar,
};

#[derive(Copy, Clone)]
pub(crate) enum Id {
    Var(usize),
    Node(usize),
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

pub(crate) struct AstCtx(Ctx);

pub(crate) type PatCtx = Ctx;

#[derive(Default)]
pub(crate) struct Ctx {
    nodes: Vec<Node>,
    vars: Vec<SynVar>,
}

impl Ctx {
    fn add_var_internal(&mut self, var: SynVar) -> Id {
        self.vars.push(var);
        Id::Var(self.vars.len() - 1)
    }

    pub(crate) fn add_node(&mut self, node: Node) -> Id {
        self.nodes.push(node);
        Id::Node(self.nodes.len() - 1)
    }

    pub(crate) fn add_var<T: AsNode>(&mut self, var: SynVar) -> NodeId<T> {
        if self.vars.contains(&var) {
            todo!("Merge duplicates")
        }
        self.add_var_internal(var).typed()
    }

    pub(crate) fn add<T: AsNode>(&mut self, t: T) -> NodeId<T> {
        self.add_node(t.as_node()).typed()
    }

    pub(crate) fn add_convert<S: AsNode, T: Convert<S> + FromPlaceholder>(
        &mut self,
        t: T,
    ) -> NodeId<S> {
        match T::from_placeholder(t) {
            Pattern::Exact(t) => {
                let s = t.convert(self);
                self.add(s)
            }
            Pattern::Pattern(syn_var) => self.add_var(syn_var),
        }
    }

    pub(crate) fn get_node(&self, id: Id) -> Option<&Node> {
        match id {
            Id::Var(_) => None,
            Id::Node(id) => Some(&self.nodes[id]),
        }
    }

    pub(crate) fn get<T: AsNode>(&self, id: NodeId<T>) -> Option<&T> {
        self.get_node(id.id).map(|node| T::from_node(node).unwrap())
    }

    // pub(crate) fn get_mut<T: AsNode>(&mut self, id: NodeId<T>) -> &T {
    //     T::from_node_mut(&mut self.nodes[id.id.0]).unwrap()
    // }

    pub(crate) fn typed<T: AsNode>(&self, id: Id) -> NodeId<T> {
        // TODO: safety checks
        id.typed()
    }
}

impl AstCtx {
    pub(crate) fn new(ctx: Ctx) -> Self {
        Self(ctx)
    }

    pub(crate) fn get<T: AsNode>(&self, id: NodeId<T>) -> &T {
        // We unwrap here, since there are no SynVars on a full AST.
        self.0.get(id).unwrap()
    }

    pub(crate) fn typed<T: AsNode>(&self, id: Id) -> NodeId<T> {
        self.0.typed(id)
    }
}

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
