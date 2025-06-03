use std::collections::HashSet;

use crate::{Ctx, GetKind, Id, NodeId, Pattern, ToNode};

#[derive(Default)]
pub struct Dependencies {
    pub vars: HashSet<Id>,
}

impl Dependencies {
    pub fn new<Node: GetKind + GetDependencies<Node>>(id: Id, ctx: &Ctx<Node>) -> Self {
        let mut deps = Self {
            vars: HashSet::default(),
        };
        match ctx.get::<Node>(id) {
            Pattern::Real(node) => node.get_dependencies(ctx, &mut deps),
            Pattern::Pat(var_id) => {
                deps.vars.insert(var_id);
            }
        }
        deps
    }
}

pub trait GetDependencies<Node: GetKind> {
    fn get_dependencies(&self, ctx: &Ctx<Node>, deps: &mut Dependencies);
}

impl<Node: GetKind, T: GetDependencies<Node> + ToNode<Node>> GetDependencies<Node> for NodeId<T> {
    fn get_dependencies(&self, ctx: &Ctx<Node>, deps: &mut Dependencies) {
        match ctx.get::<T>(*self) {
            Pattern::Real(t) => t.get_dependencies(ctx, deps),
            Pattern::Pat(var_id) => {
                deps.vars.insert(var_id);
            }
        }
    }
}

impl<Node: GetKind, T: GetDependencies<Node>> GetDependencies<Node> for Option<T> {
    fn get_dependencies(&self, ctx: &Ctx<Node>, deps: &mut Dependencies) {
        if let Some(t) = self {
            t.get_dependencies(ctx, deps)
        }
    }
}
