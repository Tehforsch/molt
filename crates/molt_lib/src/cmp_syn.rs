use crate::match_pattern::Match;

pub trait CmpSyn {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self);
}

impl CmpSyn for proc_macro2::Literal {
    fn cmp_syn(&self, _: &mut Match, _: &Self) {
        // only needed for verbatim lits
        unreachable!()
    }
}
