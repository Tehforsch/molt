mod parse;

use molt_lib::{Id, NodeId, VarDecl};
use rust_grammar::{Expr, Item, Kind, Lit, Node, Stmt, TokenStream, parse::ParseStream};

pub(crate) struct UnresolvedMoltFile {
    pub vars: Vec<UnresolvedVarDecl>,
    pub commands: Vec<Command<String>>,
}

pub struct UnresolvedVarDecl {
    pub name: String,
    pub kind: UserKind,
    pub tokens: Option<TokenStream>,
}

pub(crate) enum Decl {
    VarDecl(UnresolvedVarDecl),
    Command(Command<String>),
}

#[derive(Debug)]
pub(crate) struct MoltFile {
    pub vars: Vec<VarDecl>,
    pub command: Command<Id>,
}

#[derive(Debug)]
pub(crate) enum Command<T> {
    Match(T),
}

impl<T> Command<T> {
    pub fn map<S>(self, f: impl Fn(T) -> S) -> Command<S> {
        match self {
            Command::Match(t) => Command::Match(f(t)),
        }
    }
}

macro_rules! define_user_kind {
    ($(($variant_name: ident, $ty: ty $(, $expr: expr)?)),*$(,)?) => {
        #[derive(Debug, Clone, Copy)]
        pub enum UserKind {
            $( $variant_name, )*
        }

        impl From<UserKind> for Kind {
            fn from(val: UserKind) -> Kind {
                match val {
                    $(
                        UserKind::$variant_name => Kind::$variant_name,
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
            return Ok($parser
                .add($parser.parse_span::<$ty>()?.map(|t| Node::$variant_name(t)))
                .into());
        }
    };
}

define_user_kind! {
    (Lit, Lit),
    (Item, Item),
    (Expr, Expr, |parser: ParseStream| {
        let expr: NodeId<Expr> = parser.parse()?;
        Ok(expr.into())
    }),
    (Stmt, Stmt, |parser: ParseStream| {
        let stmt: NodeId<Stmt> = parser.parse()?;
        Ok(stmt.into())
    }),
}
