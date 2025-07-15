mod cmp_syn;
mod config;
mod ctx;
mod match_ctx;
mod match_pattern;
mod node;
mod node_list;
mod pattern;
mod span;

pub use cmp_syn::CmpSyn;
pub use config::*;
pub use ctx::{Ctx, Id, NodeId, Var, VarDecl};
pub use match_ctx::{MatchCtx, MatchPatternData};
pub use match_pattern::{Binding, Match, Matcher, PatType, match_pattern};
pub use node::{KindType, NodeType, ToNode};
pub use node_list::{
    List, ListMatchingMode, NoPunct, NodeList, PatNodeList, RealNodeList, SetMatchingMode, Single,
    SingleMatchingMode,
};
pub use pattern::Pattern;
pub use span::{Span, Spanned, SpannedPat, WithSpan};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParsingMode {
    Real,
    Pat,
}
