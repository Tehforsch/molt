mod match_ctx;

use std::collections::HashMap;

use crate::cmp_syn::CmpSyn;
use crate::match_pattern::match_ctx::MatchCtx;
use crate::molt_lang::RuntimeCtx;
use crate::rule::{DoesNotRequireRule, RequiresRule};
use crate::rust_grammar::Node;
use crate::{
    NodeId, NodeList, NodeType, RawNodeId, Term,
    rule::{Rule, RuleKey, Rules},
};

pub(crate) struct NoMatch;
pub(crate) type IsMatch<T = ()> = Result<T, NoMatch>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Binding {
    pub(crate) id: Option<RawNodeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ListBinding {
    pub(crate) ids: Option<Vec<RawNodeId>>,
}

#[derive(Debug)]
pub(crate) struct Match {
    bindings: HashMap<RawNodeId, Binding>,
    list_bindings: HashMap<RawNodeId, ListBinding>,
}

impl Match {
    pub(crate) fn get_binding(&self, var: RawNodeId) -> &Binding {
        &self.bindings[&var]
    }

    pub(crate) fn get_list_binding(&self, var: RawNodeId) -> &ListBinding {
        &self.list_bindings[&var]
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = RawNodeId> {
        self.bindings.keys().copied()
    }

    pub(crate) fn iter_list_vars(&self) -> impl Iterator<Item = RawNodeId> {
        self.list_bindings.keys().copied()
    }
}

// This exists to appease borrowck.
// It is the owner of the `MatchCtx` which
// we use in `Matcher`. If the `Matcher` owns the
// `MatchCtx` itself, the borrowchecker will complain
// about aliasing (since it is now not hidden under a
// reference anymore and therefore mutable)
pub(crate) struct MatcherBuilder<'a, Node: NodeType> {
    rules: &'a Rules,
    ctx: MatchCtx<'a, Node>,
    bindings: HashMap<RawNodeId, Binding>,
    list_bindings: HashMap<RawNodeId, ListBinding>,
}

pub(crate) struct Matcher<'a, Node: NodeType> {
    rules: &'a Rules,
    ctx: &'a MatchCtx<'a, Node>,
    bindings: HashMap<RawNodeId, Binding>,
    list_bindings: HashMap<RawNodeId, ListBinding>,
}

impl<'a> Matcher<'a, Node> {
    pub(crate) fn from_interpreter_ctx(
        context: &'a RuntimeCtx<'a>,
        ctx: &'a crate::Ctx<Node>,
        rules: &'a Rules,
    ) -> MatcherBuilder<'a, Node> {
        let ctx = MatchCtx::from_interpreter_ctx(context, ctx);
        MatcherBuilder {
            rules,
            bindings: HashMap::default(),
            list_bindings: HashMap::default(),
            ctx,
        }
    }
}

impl<'a, Node: NodeType> MatcherBuilder<'a, Node> {
    /// Add a single variable to the matching context.
    pub fn add_var(&mut self, var: RawNodeId, bound_to: Option<RawNodeId>) {
        let _previous_entry = self.bindings.insert(var, Binding { id: bound_to });
        debug_assert!(_previous_entry.is_none());
    }

    /// Add a list variable to the matching context.
    pub fn add_list_var(&mut self, var: RawNodeId, bound_to: Option<Vec<RawNodeId>>) {
        let _previous_entry = self
            .list_bindings
            .insert(var, ListBinding { ids: bound_to });
        debug_assert!(_previous_entry.is_none());
    }

    pub fn get_matches(self, molt: RawNodeId, real: RawNodeId) -> Option<Match> {
        let mut matcher = Matcher {
            rules: self.rules,
            ctx: &self.ctx,
            bindings: self.bindings,
            list_bindings: self.list_bindings,
        };
        matcher.cmp_ids(real, molt).ok().map(|_| Match {
            bindings: matcher.bindings,
            list_bindings: matcher.list_bindings,
        })
    }
}

impl<'a, Node: NodeType> Matcher<'a, Node> {
    fn bind(&mut self, key: RawNodeId, id: RawNodeId) -> IsMatch {
        if self.ctx.config().debug_print {
            println!(
                "\tBind ${} to {}",
                &self.ctx.get_var(key).ident(),
                self.ctx.print(id)
            );
        }
        let binding = self.bindings.get_mut(&key).unwrap();
        if let Some(previous_match) = binding.id {
            // We encountered this variable again, so compare it to the
            // previous match for consistency.
            self.cmp_ids(id, previous_match)
        } else {
            binding.id = Some(id);
            IsMatch::Ok(())
        }
    }

    fn bind_list<T: CmpSyn<Node>>(&mut self, key: RawNodeId, ids: &[NodeId<T>]) -> IsMatch {
        let binding = self.list_bindings.get_mut(&key).unwrap();
        if let Some(previous_ids) = binding.ids.clone() {
            self.eq(ids.len(), previous_ids.len())?;
            for (new, prev) in ids.iter().zip(previous_ids.iter()) {
                self.cmp_ids((*new).into(), *prev)?;
            }
            IsMatch::Ok(())
        } else {
            binding.ids = Some(ids.iter().map(|id| (*id).into()).collect());
            IsMatch::Ok(())
        }
    }

    fn cmp_ids(&mut self, real_id: RawNodeId, id: RawNodeId) -> IsMatch {
        if self.ctx.config().debug_print {
            println!(
                "Compare \n\t{}\n\t{}",
                self.ctx.print(real_id).replace("\n", " "),
                self.ctx.print(id),
            );
        }

        match self.ctx.get::<Node>(id) {
            Term::Var(var) => self.bind(var, real_id),
            Term::Item(node) => {
                let item = self.ctx.real_ctx.get(real_id).unwrap_item();
                self.cmp_syn::<Node, Node>(item, node)
            }
        }
    }

    fn check(&mut self, val: bool) -> IsMatch {
        match val {
            true => IsMatch::Ok(()),
            false => IsMatch::Err(NoMatch),
        }
    }

    pub(super) fn eq<T: PartialEq>(&mut self, t1: T, t2: T) -> IsMatch {
        self.check(t1 == t2)
    }

    pub(super) fn no_match(&mut self) -> IsMatch {
        IsMatch::Err(NoMatch)
    }

    pub(super) fn cmp_nodes<T, S: CmpSyn<Node, T, R>, R>(
        &mut self,
        real: NodeId<T>,
        molt: NodeId<S>,
    ) -> IsMatch {
        self.cmp_ids(real.into(), molt.into())
    }

    pub(super) fn cmp_lists<T: CmpSyn<Node>, P>(
        &mut self,
        ts1: &NodeList<T, P>,
        ts2: &NodeList<T, P>,
    ) -> IsMatch {
        match (ts1, ts2) {
            (Term::Item(ts1), Term::Item(ts2)) => self.cmp_lists_real(ts1.as_ref(), ts2.as_ref()),
            (Term::Item(real_list), Term::Var(var)) => {
                self.bind_list((*var).into(), real_list.as_ref())
            }
            (Term::Var(_), _) => unreachable!(),
        }
    }

    fn cmp_lists_real<T: CmpSyn<Node>>(&mut self, ts1: &[NodeId<T>], ts2: &[NodeId<T>]) -> IsMatch {
        self.eq(ts1.len(), ts2.len())?;
        for (item1, item2) in ts1.iter().zip(ts2.iter()) {
            self.cmp_nodes(*item1, *item2)?;
        }
        IsMatch::Ok(())
    }

    /// Compare two types that do not have a rule to toggle their behavior.
    pub(super) fn cmp_syn<T: CmpSyn<Node, S, DoesNotRequireRule>, S>(
        &mut self,
        t1: &T,
        t2: &S,
    ) -> IsMatch {
        t1.cmp_syn(self, t2)
    }

    /// Compare two types that have a rule to toggle their behavior.
    pub(super) fn cmp_syn_with_rule<T: CmpSyn<Node, S, RequiresRule>, S>(
        &mut self,
        t1: &T,
        t2: &S,
        rule: impl Into<RuleKey>,
    ) -> IsMatch {
        if self.should_compare(rule.into()) {
            t1.cmp_syn(self, t2)
        } else {
            IsMatch::Ok(())
        }
    }

    /// Compare two types and explicitly ignore whether a rule is required or not.
    pub(super) fn cmp_syn_ignore_rule<R, T: CmpSyn<Node, S, R>, S>(
        &mut self,
        t1: &T,
        t2: &S,
    ) -> IsMatch {
        t1.cmp_syn(self, t2)
    }

    pub(crate) fn should_compare(&mut self, rule: RuleKey) -> bool {
        match self.rules[rule] {
            Rule::Ignore => false,
            Rule::Strict => true,
        }
    }
}
