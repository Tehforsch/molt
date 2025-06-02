use std::collections::HashMap;

use molt_lib::Dependencies;

use crate::{
    Error, PatCtx,
    molt_grammar::{Command, MoltFile},
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
