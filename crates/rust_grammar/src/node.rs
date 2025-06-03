use derive_macro::{CmpSyn, GetDependencies};
use molt_lib::{NodeId, ToNode};

use crate::{
    parse::{Parse, ParseStream, Result},
    Expr, Item, Lit,
};

macro_rules! define_node_and_kind {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub enum Kind {
            $(
                $variant_name,
            )*
        }

        #[derive(CmpSyn, GetDependencies)]
        pub enum Node {
            $(
                $variant_name($ty),
            )*
        }

        impl molt_lib::GetKind for Node {
            type Kind = Kind;

            fn kind(&self) -> Kind {
                match self {
                    $(
                        Self::$variant_name(_) => Kind::$variant_name,
                    )*
                }
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
            impl ToNode<Node> for $ty {
                fn to_node(self) -> Node {
                    Node::$variant_name(self)
                }

                fn from_node(node: &Node) -> Option<&Self> {
                    #[allow(irrefutable_let_patterns)]
                    if let Node::$variant_name(item) = node {
                        Some(item)
                    } else {
                        None
                    }
                }

                fn from_node_mut(node: &mut Node) -> Option<&mut Self> {
                    #[allow(irrefutable_let_patterns)]
                    if let Node::$variant_name(item) = node {
                        Some(item)
                    } else {
                        None
                    }
                }
            }
        )*

        impl Node {
            pub fn parse_with_kind(parser: crate::parse::ParseStream, kind: Kind) -> crate::parse::Result<Self> {
                match kind {
                    $(
                        Kind::$variant_name => {
                            Ok(Node::$variant_name(parser.parse::<$ty>()?))
                        },
                    )*
                }
            }
        }
    }
}

define_node_and_kind! {
    (Lit, Lit),
    (Item, Item),
    (Expr, Expr),
    // (Attr, Attribute),
    // (Signature, Signature),
    // (FnArg, FnArg),
}

impl<T: Parse + ToNode<Node>> Parse for NodeId<T> {
    fn parse(parser: ParseStream) -> Result<Self> {
        let t = parser.parse_with_span()?;
        let id = parser.ctx().add(t);
        Ok(id)
    }
}
