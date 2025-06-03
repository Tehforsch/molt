use crate::{NodeId, match_pattern::Match};

pub trait CmpSyn {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self);
}

impl<T: CmpSyn> CmpSyn for NodeId<T> {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_nodes(*self, *pat)
    }
}

impl<T: CmpSyn> CmpSyn for Option<T> {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        match (self, pat) {
            (None, None) => {}
            (Some(s1), Some(s2)) => ctx.cmp_syn(s1, s2),
            _ => ctx.no_match(),
        }
    }
}

impl<T1: CmpSyn, T2: CmpSyn> CmpSyn for (T1, T2) {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_syn(&self.0, &pat.0);
        ctx.cmp_syn(&self.1, &pat.1);
    }
}

impl<T1: CmpSyn, T2: CmpSyn, T3: CmpSyn> CmpSyn for (T1, T2, T3) {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_syn(&self.0, &pat.0);
        ctx.cmp_syn(&self.1, &pat.1);
        ctx.cmp_syn(&self.2, &pat.2);
    }
}

impl<T: CmpSyn> CmpSyn for Box<T> {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_syn(&*self, &*pat);
    }
}

impl<T: CmpSyn> CmpSyn for Vec<T> {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        // These should be replaced by NodeList wherever possible
        // but we'll leave the ones that havent been exchanged yet
        // exact.
        ctx.eq(self.len(), pat.len());
        for (s1, s2) in self.iter().zip(pat.iter()) {
            ctx.cmp_syn(s1, s2);
        }
    }
}

#[macro_export]
macro_rules! impl_cmp_syn_with_partial_eq {
    ($ty: ty) => {
        impl $crate::CmpSyn for $ty {
            fn cmp_syn(&self, ctx: &mut $crate::Match, pat: &Self) {
                ctx.eq(self, pat)
            }
        }
    };
    ($ty: ty, $closure: expr) => {
        impl $crate::CmpSyn for $ty {
            fn cmp_syn(&self, ctx: &mut $crate::Match, pat: &Self) {
                ctx.check($closure(self, pat))
            }
        }
    };
}

impl_cmp_syn_with_partial_eq!(bool);
impl_cmp_syn_with_partial_eq!(u32);
impl_cmp_syn_with_partial_eq!(usize);
// TODO: Remove? This is needed for shebang on File
impl_cmp_syn_with_partial_eq!(String);

// The following impls exist because of the orphan rule.
// I can't do them within `rust_grammar` since they are
// simply reexported types from `proc_macro2`.
impl CmpSyn for proc_macro2::Literal {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.cmp_syn(&self.to_string(), &pat.to_string())
    }
}

impl CmpSyn for proc_macro2::TokenStream {
    fn cmp_syn(&self, _: &mut Match, _: &Self) {
        // only needed for verbatim items
        unreachable!()
    }
}

impl CmpSyn for proc_macro2::Ident {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.eq(self.to_string(), pat.to_string())
    }
}

impl CmpSyn for proc_macro2::Span {
    fn cmp_syn(&self, _: &mut Match, _: &Self) {}
}
