use std::collections::HashMap;

use crate::cmp_syn::CmpSyn;
use crate::match_ctx::MatchCtx;
use crate::node_list::List;
use crate::rule::{DoesNotRequireRule, RequiresRule};
use crate::{
    Id, NodeId, NodeList, NodeType, PatNodeList, Pattern, RealNodeList, Single, SingleMatchingMode,
    VarDecl,
    rule::{Rule, RuleKey, Rules},
};

pub struct NoMatch;
pub type IsMatch<T = ()> = Result<T, NoMatch>;

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub molt: Option<Id>,
    pub real: Option<Id>,
}

impl Binding {
    fn new(molt: Option<Id>) -> Self {
        Self { molt, real: None }
    }
}

#[derive(Debug)]
pub struct Match {
    bindings: HashMap<Id, Binding>,
}

impl Match {
    pub fn get_binding(&self, var: Id) -> &Binding {
        &self.bindings[&var]
    }

    pub fn iter_vars(&self) -> impl Iterator<Item = Id> {
        self.bindings.keys().cloned()
    }
}

pub struct Matcher<'a, Node: NodeType> {
    rules: &'a Rules,
    ctx: &'a MatchCtx<'a, Node>,
    bindings: HashMap<Id, Binding>,
}

impl<'a, Node: NodeType> Matcher<'a, Node> {
    fn new_root(vars: &[VarDecl], rules: &'a Rules, ctx: &'a MatchCtx<'a, Node>) -> Self {
        Self {
            rules,
            bindings: vars
                .iter()
                .map(|var| (var.id, Binding::new(var.node)))
                .collect(),
            ctx,
        }
    }

    fn add_binding(&mut self, key: Id, real_id: Id) -> IsMatch {
        if self.ctx.config().debug_print {
            println!(
                "\tBind ${} to {}",
                &self.ctx.get_var(key).name(),
                self.ctx.print(real_id)
            );
        }
        let binding = self.bindings.get_mut(&key).unwrap();
        if let Some(real_id_2) = binding.real {
            self.cmp_ids(real_id, real_id_2)
        } else {
            binding.real = Some(real_id);
            if let Some(molt_id) = binding.molt {
                self.cmp_ids(real_id, molt_id)
            } else {
                IsMatch::Ok(())
            }
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
            Pattern::Var(var) => self.add_binding(var, real_id),
            Pattern::Item(pat) => {
                self.cmp_syn::<Node, Node>(self.ctx.real_ctx.get(real_id).unwrap_item(), pat)
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
            (Pattern::Item(ts1), Pattern::Item(ts2)) => {
                self.cmp_lists_real(ts1.items(), ts2.items())
            }
            (Pattern::Item(ts1), Pattern::Var(ts2)) => match ts2 {
                PatNodeList::Single(single) => self.cmp_lists_single(ts1, single),
                PatNodeList::List(list) => self.cmp_lists_list(ts1, list),
            },
            (Pattern::Var(_), _) => unreachable!(),
        }
    }

    fn cmp_lists_real<T: CmpSyn<Node>>(&mut self, ts1: &[NodeId<T>], ts2: &[NodeId<T>]) -> IsMatch {
        self.eq(ts1.len(), ts2.len())?;
        for (item1, item2) in ts1.iter().zip(ts2.iter()) {
            self.cmp_nodes(*item1, *item2)?;
        }
        IsMatch::Ok(())
    }

    fn cmp_lists_single<T: CmpSyn<Node>, P>(
        &mut self,
        ts1: &RealNodeList<T, P>,
        ts2: &Single<T, P>,
    ) -> IsMatch {
        match ts2.mode() {
            SingleMatchingMode::Any => todo!(),
            SingleMatchingMode::All => {
                for item1 in ts1.iter() {
                    self.cmp_ids((*item1).into(), ts2.item().into())?;
                }
            }
        }
        IsMatch::Ok(())
    }

    fn cmp_lists_list<T: CmpSyn<Node>, P>(
        &mut self,
        _: &RealNodeList<T, P>,
        _: &List<T, P>,
    ) -> IsMatch {
        // TODO: Figure out what to do with this.
        todo!()
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

    pub fn should_compare(&mut self, rule: RuleKey) -> bool {
        match self.rules[rule] {
            Rule::Ignore => false,
            Rule::Strict => true,
        }
    }
}

pub fn match_pattern<N: NodeType + CmpSyn<N>>(
    ctx: &MatchCtx<N>,
    vars: &[VarDecl],
    molt_var: Id,
    real: Id,
    rules: &Rules,
) -> Vec<Match> {
    let mut match_ = Matcher::new_root(vars, rules, ctx);
    dbg!(molt_var, real);
    match match_.add_binding(molt_var, real) {
        Ok(_) => vec![Match {
            bindings: match_.bindings,
        }],
        Err(_) => vec![],
    }
}
