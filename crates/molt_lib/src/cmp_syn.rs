use crate::match_pattern::Match;

pub(crate) trait CmpSyn {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self);
}
