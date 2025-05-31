use derive_macro::{CmpSyn, GetDependencies};

use crate::cmp_syn::CmpSyn;
use crate::ctx::MatchCtx;
use crate::match_pattern::Match;

use super::rust_grammar::{Attribute, Ident, Item, Lit};
use super::{Parse, ParseStream, Spanned};
use super::{Var, VarId};

pub(crate) enum Pattern<E> {
    Exact(E),
    Pattern(VarId),
}

pub(crate) trait ToNode {
    fn to_node(self) -> Node;
    fn from_node(node: &Node) -> Option<&Self>;
}

pub(crate) trait GetKind {
    fn get_kind() -> Kind;
}

macro_rules! define_node_and_kind {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub(crate) enum Kind {
            $(
                $variant_name,
            )*
        }

        #[derive(GetDependencies, CmpSyn)]
        pub(crate) enum Node {
            $(
                $variant_name($ty),
            )*
        }

        $(
            impl GetKind for $ty {
                fn get_kind() -> Kind {
                    Kind::$variant_name
                }
            }
        )*

        impl Kind {
            pub(crate) fn from_str(s: &str) -> Self {
                $(
                    if s == stringify!($variant_name) {
                        return Self::$variant_name;
                    }
                )*
                panic!();
            }

        }

        impl std::fmt::Display for Kind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$variant_name => write!(f, stringify!($variant_name)),
                    )*
                }
            }
        }

        $(
            impl ToNode for $ty {
                fn to_node(self) -> Node {
                    Node::$variant_name(self)
                }

                fn from_node(node: &Node) -> Option<&Self> {
                    if let Node::$variant_name(item) = node {
                        Some(item)
                    } else {
                        None
                    }
                }
            }
        )*

        impl Node {
            pub(crate) fn kind(&self) -> Kind {
                match self {
                    $(
                        Self::$variant_name(_) => Kind::$variant_name,
                    )*
                }
            }
        }
    }
}

macro_rules! define_user_kind {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        #[derive(Debug, Clone, Copy)]
        pub enum UserKind {
            $( $variant_name, )*
        }

        impl UserKind {
            pub fn to_kind(self) -> Kind {
                match self {
                    $(
                        Self::$variant_name => Kind::$variant_name,
                    )*
                }
            }
        }

        mod kind_kws {
            $(
                syn::custom_keyword!($variant_name);
            )*
        }

        impl Parse for UserKind {
            fn parse(input: ParseStream) -> super::Result<Self> {
                $(
                    if input.peek(kind_kws::$variant_name) {
                        let _: kind_kws::$variant_name = input.parse()?;
                        return Ok(UserKind::$variant_name);
                    }
                )*
                Err(input.error("Invalid kind."))
            }
        }

        impl Node {
            pub(crate) fn parse_with_kind(parser: ParseStream, kind: UserKind) -> super::Result<Spanned<Self>> {
                match kind {
                    $(
                        UserKind::$variant_name => {
                            Ok(parser.parse_spanned::<$ty>()?.map(|t| Node::$variant_name(t)))
                        },
                    )*
                }
            }
        }
    }
}

define_node_and_kind! {
    (Ident, Ident),
    (Lit, Lit),
    (Item, Item),
    (Attr, Attribute),
    // (Expr, Expr),
    // (Signature, Signature),
    // (FnArg, FnArg),
}

define_user_kind! {
    (Ident, Ident),
    (Lit, Lit),
    (Item, Item),
}

impl ToNode for Node {
    fn to_node(self) -> Node {
        self
    }

    fn from_node(node: &Node) -> Option<&Self> {
        Some(node)
    }
}

impl<'a, T> Pattern<&'a T> {
    pub(crate) fn map<S>(&self, f: impl Fn(&'a T) -> &'a S) -> Pattern<&'a S> {
        match self {
            Pattern::Exact(t) => Pattern::Exact(f(t)),
            Pattern::Pattern(var_id) => Pattern::Pattern(*var_id),
        }
    }

    pub(crate) fn as_exact(&self) -> Option<&'a T> {
        match self {
            Pattern::Exact(t) => Some(t),
            Pattern::Pattern(_) => None,
        }
    }
}
