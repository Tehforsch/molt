use std::collections::{HashMap, HashSet};

use molt_lib::{Ctx, Id};
use rust_grammar::{Ident, Node};

use crate::{
    Error, PatCtx,
    molt_grammar::{Command, MoltFile, VarId},
};

#[derive(Debug, thiserror::Error)]
pub enum ResolveError {
    #[error("Multiple commands given.")]
    MultipleCommandGiven,
    #[error("No commands given.")]
    NoCommandGiven,
    #[error("Unresolvable pattern.")]
    Unresolvable,
}

#[derive(Default)]
pub(crate) struct Dependencies {
    pub vars: HashSet<VarId>,
}

impl Dependencies {
    fn new(id: Id, ctx: &Ctx<Node>) -> Self {
        let mut deps = Self {
            vars: HashSet::default(),
        };
        todo!()
        // match ctx.get(id) {
        //     Pattern::Real(node) => node.get_dependencies(ctx, deps),
        //     Pattern::Pat(var_id) => {
        //         deps.vars.insert(var_id);
        //     }
        // }
        // deps
    }
}

pub(crate) trait GetDependencies {
    fn get_dependencies(&self, ctx: &PatCtx, deps: &mut Dependencies);
}

impl GetDependencies for Id {
    fn get_dependencies(&self, _: &PatCtx, _: &mut Dependencies) {}
}

impl GetDependencies for Ident {
    fn get_dependencies(&self, _: &PatCtx, _: &mut Dependencies) {}
}

impl MoltFile {
    pub(crate) fn get_command(&mut self) -> Result<Command, Error> {
        assert!(self.sorted);
        if self.commands.is_empty() {
            // Topological sorting ensures that this
            // variable contains no other variables,
            // so it is most likely the variable
            // we want to match for.
            if let Some(var) = self.vars.last() {
                Ok(Command::Match(var.id))
            } else {
                Err(ResolveError::NoCommandGiven.into())
            }
        } else if self.commands.len() > 1 {
            return Err(ResolveError::MultipleCommandGiven.into());
        } else {
            Ok(self.commands.remove(0))
        }
    }

    pub(crate) fn sort_vars(&mut self, pat_ctx: &PatCtx) -> Result<(), ResolveError> {
        // Topologically sort the variable declarations according to
        // their dependencies (i.e. which variables they reference)
        let mut deps_map: HashMap<_, _> = self
            .vars
            .iter()
            .map(|var| {
                (
                    var.id,
                    match var.node {
                        Some(node) => Dependencies::new(node, pat_ctx),
                        None => Dependencies::default(),
                    },
                )
            })
            .collect();
        let mut unsorted: Vec<_> = self.vars.drain(..).collect();
        let mut sorted = vec![];
        while !unsorted.is_empty() {
            let solvable = unsorted
                .iter()
                .enumerate()
                .find(|(_, var)| deps_map[&var.id].vars.is_empty());
            if let Some((index, _)) = solvable {
                let var = unsorted.remove(index);
                for deps in deps_map.values_mut() {
                    deps.vars.remove(&var.id);
                }
                sorted.push(var);
            } else {
                return Err(ResolveError::Unresolvable);
            }
        }
        self.vars = sorted;
        self.sorted = true;
        Ok(())
    }
}
