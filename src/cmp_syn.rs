use crate::match_pattern::{IsMatch, Matcher};
use crate::rule::DoesNotRequireRule;
use crate::{NodeId, NodeType};

/// This trait describes how a syntactic type is compared
/// to itself (or another, similar, type `T`).
///
/// Syntactic comparison is transitive. This property is important
/// because during pattern matching, we will sometimes encounter a
/// given variable twice, for example:
/// ```ignore
/// let ty: Type = Foo;
/// let outer: Type = ($ty, $ty);
/// ```
/// If we compare a concrete type `(Foo, Foo)` to this pattern, we
/// will bind `$ty` to the first `Foo` and then encounter another
/// `Foo` which has to be compared to `$ty` yet again. At this point,
/// `$ty` is bound both to the pattern given by its definition and to
/// the concrete type `Foo` which was bound to it in the comparison of
/// the first tuple item. Transitivity guarantees that comparing to
/// either of the two (the pattern or the concrete type) is
/// sufficient, since the other comparison is guaranteed to yield the
/// same result.
///
/// The `Rule` generic exists to distinguish types that
/// can be compared without checking if the corresponding
/// rule is enabled to those that need this check.
///
/// The `Matcher::cmp_syn` method only exists for the former
/// types while a more specific method exists for the latter.
/// This structure exists to make sure one does not accidentally
/// forget to check for the correct rule in custom impls of this
/// trait.
pub trait CmpSyn<Node: NodeType, T = Self, Rule = DoesNotRequireRule> {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &T) -> IsMatch;
}

impl<Node: NodeType, T: CmpSyn<Node, T, R>, R> CmpSyn<Node, NodeId<T>, R> for NodeId<T> {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch {
        ctx.cmp_nodes(*self, *pat)
    }
}

impl<Node: NodeType, T: CmpSyn<Node>> CmpSyn<Node> for Option<T> {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch {
        match (self, pat) {
            (None, None) => Ok(()),
            (Some(s1), Some(s2)) => ctx.cmp_syn(s1, s2),
            _ => ctx.no_match(),
        }
    }
}

impl<Node: NodeType, T1: CmpSyn<Node>, T2: CmpSyn<Node>> CmpSyn<Node> for (T1, T2) {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch {
        ctx.cmp_syn(&self.0, &pat.0)?;
        ctx.cmp_syn(&self.1, &pat.1)
    }
}

impl<Node: NodeType, T1: CmpSyn<Node>, T2: CmpSyn<Node>, T3: CmpSyn<Node>> CmpSyn<Node>
    for (T1, T2, T3)
{
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch {
        ctx.cmp_syn(&self.0, &pat.0)?;
        ctx.cmp_syn(&self.1, &pat.1)?;
        ctx.cmp_syn(&self.2, &pat.2)
    }
}

impl<Node: NodeType, T: CmpSyn<Node>> CmpSyn<Node> for Box<T> {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch {
        ctx.cmp_syn(self, pat)
    }
}

impl<Node: NodeType, T: CmpSyn<Node>> CmpSyn<Node> for Vec<T> {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch {
        // These should be replaced by NodeList wherever possible
        // but we'll leave the ones that havent been exchanged yet
        // exact.
        ctx.eq(self.len(), pat.len())?;
        for (s1, s2) in self.iter().zip(pat.iter()) {
            ctx.cmp_syn(s1, s2)?;
        }
        IsMatch::Ok(())
    }
}

#[macro_export]
macro_rules! impl_cmp_syn_with_partial_eq {
    ($ty: ty) => {
        impl<Node: NodeType> $crate::CmpSyn<Node> for $ty {
            fn cmp_syn(&self, ctx: &mut $crate::Matcher<Node>, pat: &Self) -> IsMatch {
                ctx.eq(self, pat)
            }
        }
    };
    ($ty: ty, $closure: expr) => {
        impl<Node: NodeType> $crate::CmpSyn<Node> for $ty {
            fn cmp_syn(&self, ctx: &mut $crate::Matcher, pat: &Self) -> IsMatch {
                ctx.check($closure(self, pat))
            }
        }
    };
}

impl_cmp_syn_with_partial_eq!(bool);
impl_cmp_syn_with_partial_eq!(u32);
impl_cmp_syn_with_partial_eq!(usize);

// The following impls exist because of the orphan rule.
// I can't do them within `rust_grammar` since they are
// simply reexported types from `proc_macro2`.
//
// (TODO: remove this since the crate change made this
// possible.)
impl<Node: NodeType> CmpSyn<Node> for proc_macro2::Literal {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch {
        ctx.eq(&self.to_string(), &pat.to_string())
    }
}

impl<Node: NodeType> CmpSyn<Node> for proc_macro2::TokenStream {
    fn cmp_syn(&self, match_: &mut Matcher<Node>, other: &Self) -> IsMatch {
        // Needed for macros and verbatim items.
        // This impl isnt perfect but good enough
        // for my purposes so far.
        match_.eq(self.to_string(), other.to_string())
    }
}

impl<Node: NodeType> CmpSyn<Node> for proc_macro2::Ident {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch {
        ctx.eq(self.to_string(), pat.to_string())
    }
}

impl<Node: NodeType> CmpSyn<Node> for proc_macro2::Span {
    fn cmp_syn(&self, _: &mut Matcher<Node>, _: &Self) -> IsMatch {
        IsMatch::Ok(())
    }
}
