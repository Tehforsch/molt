#![allow(unused)]
mod cmp_syn;
mod config;
mod ctx;
mod match_ctx;
mod match_pattern;
mod node_list;

pub use ctx::{
    Ctx, GetKind, Id, NodeId, Pattern, Span, Spanned, SpannedPat, ToNode, Var, VarDecl, WithSpan,
};

pub use node_list::{
    MatchingMode, NoPunct, NodeList, PatNodeList, RealNodeList, Single, SingleMatchingMode,
};

pub use config::Config;

pub use cmp_syn::CmpSyn;
pub use match_ctx::MatchCtx;
pub use match_pattern::{Binding, Match, PatType, match_pattern};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParsingMode {
    Real,
    Pat,
}
