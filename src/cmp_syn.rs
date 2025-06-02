use rust_grammar::Ident;

use crate::match_pattern::Match;

pub(crate) trait CmpSyn {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self);
}

impl CmpSyn for Ident {
    fn cmp_syn(&self, ctx: &mut Match, pat: &Self) {
        ctx.eq(self.to_string(), pat.to_string())
    }
}
