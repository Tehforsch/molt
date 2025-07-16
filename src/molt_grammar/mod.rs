mod parse;

use crate::rust_grammar::{Kind, TokenStream, Type};
use molt_lib::{
    Id, NodeId, Span, VarDecl,
    rule::{Rule, RuleKey},
};
use parse::UnresolvedVarDecls;

#[derive(Debug, Clone)]
pub struct TokenVar {
    pub span: Span,
    pub name: String,
}

#[derive(Debug)]
pub struct UnresolvedTypeAnnotation {
    pub var_name: String,
    pub type_: TokenStream,
}

#[derive(Debug)]
pub struct TypeAnnotation {
    pub var_name: String,
    pub type_: NodeId<Type>,
}

#[derive(Debug)]
pub(crate) struct Ruleset {
    pub rule: Rule,
    pub keys: Vec<RuleKey>,
}

pub(crate) struct UnresolvedMoltFile {
    pub vars: Vec<UnresolvedVarDecl>,
    pub commands: Vec<Command<TokenVar>>,
    pub type_annotations: Vec<UnresolvedTypeAnnotation>,
    pub rules: Vec<Ruleset>,
}

#[derive(Debug)]
pub struct UnresolvedVarDecl {
    pub var: TokenVar,
    pub kind: Kind,
    pub tokens: Option<TokenStream>,
}

enum Decl {
    Var(UnresolvedVarDecls),
    Command(Command<TokenVar>),
    TypeAnnotation(UnresolvedTypeAnnotation),
    Ruleset(Ruleset),
}

#[derive(Debug)]
pub(crate) struct MoltFile {
    pub vars: Vec<VarDecl>,
    pub command: Command<Id>,
    pub type_annotations: Vec<TypeAnnotation>,
    pub rulesets: Vec<Ruleset>,
}

#[derive(Debug, Clone)]
pub(crate) enum Command<T> {
    Match(MatchCommand<T>),
    Transform(TransformCommand<T>),
}

#[derive(Clone, Debug)]
pub struct MatchCommand<T> {
    pub match_: Option<T>,
    pub print: Option<T>,
}

#[derive(Clone, Debug)]
pub struct TransformCommand<T> {
    pub transforms: Vec<(T, T)>, // Vec of (input, output) pairs
    pub match_: Option<T>,
}

impl<T> Command<T> {
    pub fn map<S>(self, f: impl Fn(T) -> S + Clone) -> Command<S> {
        match self {
            Command::Match(MatchCommand { match_, print }) => Command::Match(MatchCommand {
                match_: match_.map(f.clone()),
                print: print.map(f),
            }),
            Command::Transform(TransformCommand { transforms, match_ }) => {
                Command::Transform(TransformCommand {
                    transforms: transforms
                        .into_iter()
                        .map(|(input, output)| (f.clone()(input), f.clone()(output)))
                        .collect(),
                    match_: match_.map(f),
                })
            }
        }
    }

    pub(crate) fn iter_var_names(&self) -> Box<dyn Iterator<Item = &T> + '_> {
        match self {
            Command::Match(MatchCommand { match_, print }) => Box::new(match_.iter().chain(print)),
            Command::Transform(TransformCommand { transforms, match_ }) => Box::new(
                transforms
                    .iter()
                    .flat_map(|(input, output)| [input, output])
                    .chain(match_),
            ),
        }
    }
}
