mod match_ctx;

use std::collections::HashMap;

use crate::cmp_syn::CmpSyn;
use crate::match_pattern::match_ctx::MatchCtx;
use crate::molt_lang::RuntimeCtx;
use crate::rule::{DoesNotRequireRule, RequiresRule};
use crate::rust_grammar::Node;
use crate::{
    Id, ItemOrVar, NodeId, NodeList, NodeType,
    rule::{Rule, RuleKey, Rules},
};

pub(crate) struct NoMatch;
pub(crate) type IsMatch<T = ()> = Result<T, NoMatch>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Binding {
    pub(crate) id: Option<Id>,
}

#[derive(Debug)]
pub(crate) struct Match {
    bindings: HashMap<Id, Binding>,
}

impl Match {
    pub(crate) fn get_binding(&self, var: Id) -> &Binding {
        &self.bindings[&var]
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = Id> {
        self.bindings.keys().copied()
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
    bindings: HashMap<Id, Binding>,
}

pub(crate) struct Matcher<'a, Node: NodeType> {
    rules: &'a Rules,
    ctx: &'a MatchCtx<'a, Node>,
    bindings: HashMap<Id, Binding>,
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
            ctx,
        }
    }
}

impl<'a, Node: NodeType> MatcherBuilder<'a, Node> {
    /// Add the variable to the matching context.
    pub fn add_var(&mut self, var: Id, bound_to: Option<Id>) {
        let _previous_entry = self.bindings.insert(var, Binding { id: bound_to });
        debug_assert!(_previous_entry.is_none());
    }

    pub fn get_matches(self, molt: Id, real: Id) -> Option<Match> {
        let mut matcher = Matcher {
            rules: self.rules,
            ctx: &self.ctx,
            bindings: self.bindings,
        };
        matcher.cmp_ids(real, molt).ok().map(|_| Match {
            bindings: matcher.bindings,
        })
    }
}

impl<'a, Node: NodeType> Matcher<'a, Node> {
    fn bind(&mut self, key: Id, id: Id) -> IsMatch {
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

    fn cmp_ids(&mut self, real_id: Id, id: Id) -> IsMatch {
        if self.ctx.config().debug_print {
            println!(
                "Compare \n\t{}\n\t{}",
                self.ctx.print(real_id).replace("\n", " "),
                self.ctx.print(id),
            );
        }

        match self.ctx.get::<Node>(id) {
            ItemOrVar::Var(var) => self.bind(var, real_id),
            ItemOrVar::Item(node) => {
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

    pub(super) fn cmp_nodes<T: CmpSyn<Node, T, R>, R>(
        &mut self,
        real: NodeId<T>,
        molt: NodeId<T>,
    ) -> IsMatch {
        self.cmp_ids(real.into(), molt.into())
    }

    pub(super) fn cmp_lists<T: CmpSyn<Node>, P>(
        &mut self,
        ts1: &NodeList<T, P>,
        ts2: &NodeList<T, P>,
    ) -> IsMatch {
        match (ts1, ts2) {
            (ItemOrVar::Item(ts1), ItemOrVar::Item(ts2)) => {
                self.cmp_lists_real(ts1.as_ref(), ts2.as_ref())
            }
            (ItemOrVar::Item(_), ItemOrVar::Var(_)) => {
                todo!()
            }
            (ItemOrVar::Var(_), _) => unreachable!(),
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
