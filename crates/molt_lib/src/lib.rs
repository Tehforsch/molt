#![allow(unused)]
mod cmp_syn;
mod config;
mod ctx;
mod match_ctx;
mod match_pattern;
mod node_list;

pub use cmp_syn::CmpSyn;
pub use config::Config;
pub use ctx::{
    Ctx, GetKind, Id, NodeId, Pattern, Span, Spanned, SpannedPat, ToNode, Var, VarDecl, WithSpan,
};
pub use match_ctx::{MatchCtx, MatchPatternData};
pub use match_pattern::{Binding, Match, Matcher, PatType, match_pattern};
pub use node_list::{
    List, ListMatchingMode, NoPunct, NodeList, PatNodeList, RealNodeList, Set, SetMatchingMode,
    Single, SingleMatchingMode,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParsingMode {
    Real,
    Pat,
}
