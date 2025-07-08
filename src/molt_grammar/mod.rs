mod parse;

use molt_lib::{Id, NodeId, Span, VarDecl};
use parse::UnresolvedVarDecls;
use rust_grammar::parse::ParseStream;
use rust_grammar::parse::discouraged::Speculative;
use rust_grammar::{
    Arm, Expr, FieldNamed, FieldUnnamed, Ident, Item, Kind, Lit, PatMulti, Stmt, TokenStream, Type,
};

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

pub(crate) struct UnresolvedMoltFile {
    pub vars: Vec<UnresolvedVarDecl>,
    pub commands: Vec<Command<TokenVar>>,
    pub type_annotations: Vec<UnresolvedTypeAnnotation>,
}

#[derive(Debug)]
pub struct UnresolvedVarDecl {
    pub var: TokenVar,
    pub kind: UserKind,
    pub tokens: Option<TokenStream>,
}

pub(crate) enum Decl {
    VarDecl(UnresolvedVarDecls),
    Command(Command<TokenVar>),
    TypeAnnotation(UnresolvedTypeAnnotation),
}

#[derive(Debug)]
pub(crate) struct MoltFile {
    pub vars: Vec<VarDecl>,
    pub command: Command<Id>,
    pub type_annotations: Vec<TypeAnnotation>,
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

macro_rules! define_user_kind {
    ($(($variant_name: ident, $kind_name: ident, $ty: ty $(, $expr: expr)?)),*$(,)?) => {
        #[derive(Debug, Clone, Copy)]
        pub enum UserKind {
            $( $variant_name, )*
        }

        impl From<UserKind> for Kind {
            fn from(val: UserKind) -> Kind {
                match val {
                    $(
                        UserKind::$variant_name => Kind::$kind_name,
                    )*
                }
            }
        }

        mod kind_kws {
            $(
                rust_grammar::custom_keyword!($variant_name);
            )*
        }

        impl rust_grammar::parse::Parse for UserKind {
            fn parse(input:rust_grammar::parse:: ParseStream) -> rust_grammar::Result<Self> {
                $(
                    if input.peek(kind_kws::$variant_name) {
                        let _: kind_kws::$variant_name = input.parse()?;
                        return Ok(UserKind::$variant_name);
                    }
                )*
                Err(input.error("Invalid kind."))
            }
        }

        pub(crate) fn parse_node_with_kind(parser: rust_grammar::parse::ParseStream, kind: UserKind) -> rust_grammar::Result<Id> {
                $(
                    parse_impl! {
                        parser, kind, $variant_name, $ty $(,$expr)*
                    }
                )*
                unreachable!()
        }
    }
}

macro_rules! parse_impl {
    ($parser: ident, $kind: ident, $variant_name: ident, $ty: ty, $expr: expr) => {
        if let UserKind::$variant_name = $kind {
            return $expr($parser);
        }
    };
    ($parser: ident, $kind: ident, $variant_name: ident, $ty: ty) => {
        if let UserKind::$variant_name = $kind {
            let parsed: NodeId<$ty> = $parser.parse()?;
            return Ok(parsed.into());
        }
    };
}

define_user_kind! {
    (Lit, Lit, Lit),
    (Item, Item, Item),
    (Type, Type, Type),
    (Expr, Expr, Expr),
    (Stmt, Stmt, Stmt),
    (Ident, Ident, Ident),
    (Arm, Arm, Arm),
    (Pat, Pat, Pat, |parser: ParseStream| {
        parser.parse_id::<PatMulti>().map(|id| id.into())
    }),
    // Let's see how this works out in practice.
    // We speculatively parse a named field and
    // if it doesn't work out, we parse an unnamed field.
    (Field, Field, Field, |parser: ParseStream| {
        let fork = parser.fork();
        if let Ok(field) = fork.parse_id::<FieldNamed>() {
            parser.advance_to(&fork); // probably unnecessary
            Ok(field.into())
        }
        else {
            Ok(parser.parse_id::<FieldUnnamed>()?.into())
        }
    }),
}
