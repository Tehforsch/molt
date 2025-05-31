use syn::Lit;

use derive_macro::GetDependencies;

use crate::ctx::MatchCtx;
use crate::match_pattern::{CmpDirect, Match};

use super::rust_grammar::{Ident, Item};
use super::{Parse, ParseStream};
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

pub(crate) trait CustomDebug {
    fn deb(&self, ctx: &MatchCtx) -> String;
}

macro_rules! define_node_and_kind {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub(crate) enum Kind {
            $(
                $variant_name,
            )*
        }

        #[derive(GetDependencies)]
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

        mod kind_kws {
            $(
                syn::custom_keyword!($variant_name);
            )*
        }

        impl Parse for Kind {
            fn parse(input: ParseStream) -> super::Result<Self> {
                $(
                    if input.peek(kind_kws::$variant_name) {
                        let _: kind_kws::$variant_name = input.parse()?;
                        return Ok(Kind::$variant_name);
                    }
                )*
                Err(syn::Error::new(input.span(), format!("Invalid kind.")))
            }
        }

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

            pub(crate) fn cmp_equal_kinds(ctx: &mut Match, ast: &Self, pat: &Self) {
                assert_eq!(ast.kind(), pat.kind());
                match ast {
                    $(
                        Node::$variant_name(s) => s.cmp_direct(ctx, <$ty>::from_node(pat).unwrap()),
                    )*
                }
            }

            pub(crate) fn parse_with_kind(parser: ParseStream, kind: Kind) -> super::Result<Self> {
                match kind {
                    $(
                        Kind::$variant_name => Ok(Node::$variant_name(parser.parse::<$ty>()?)),
                    )*
                }
            }
        }

        impl CustomDebug for Node {
            fn deb(&self, ctx: &MatchCtx) -> String {
                match self {
                    $(
                        Self::$variant_name(s) => s.deb(ctx),
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
    // (Expr, Expr),
    // (Lit, Lit),
    // (Signature, Signature),
    // (FnArg, FnArg),
}

impl ToNode for Node {
    fn to_node(self) -> Node {
        self
    }

    fn from_node(node: &Node) -> Option<&Self> {
        Some(node)
    }
}

impl CustomDebug for Lit {
    fn deb(&self, ctx: &MatchCtx) -> String {
        todo!()
    }
}

impl CustomDebug for Ident {
    fn deb(&self, ctx: &MatchCtx) -> String {
        todo!()
    }
}

impl CustomDebug for VarId {
    fn deb(&self, ctx: &MatchCtx) -> String {
        todo!()
    }
}

impl CustomDebug for Var {
    fn deb(&self, ctx: &MatchCtx) -> String {
        todo!()
    }
}

impl CustomDebug for Item {
    fn deb(&self, ctx: &MatchCtx) -> String {
        todo!()
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
