use crate::{
    ctx::NodeId,
    parse::{Parse, ParseStream, Result},
    Ident, Lit,
};

pub(crate) trait ToNode {
    fn to_node(self) -> Node;
    fn from_node(node: &Node) -> Option<&Self>;
}

pub struct Var {
    ident: Ident,
}

pub enum Node {
    Var(Var),
    Real(AstNode),
}

macro_rules! define_node_and_kind {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub(crate) enum Kind {
            $(
                $variant_name,
            )*
        }

        pub(crate) enum AstNode {
            $(
                $variant_name($ty),
            )*
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
                    Node::Real(AstNode::$variant_name(self))
                }

                fn from_node(node: &Node) -> Option<&Self> {
                    if let Node::Real(AstNode::$variant_name(item)) = node {
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
                        Self::Real(AstNode::$variant_name(_)) => Kind::$variant_name,
                    )*
                    // Should we leave this? What if we want to get the kind of a var?
                    _ => unimplemented!(),
                }
            }
        }

        impl Node {
            pub(crate) fn parse_with_kind(parser: crate::parse::ParseStream, kind: Kind) -> crate::parse::Result<Self> {
                match kind {
                    $(
                        Kind::$variant_name => {
                            Ok(Node::Real(AstNode::$variant_name(parser.parse::<$ty>()?)))
                        },
                    )*
                }
            }
        }
    }
}

define_node_and_kind! {
    (Lit, Lit),
    // (Lit, Lit),
    // (Item, Item),
    // (Attr, Attribute),
    // (Expr, Expr),
    // (Signature, Signature),
    // (FnArg, FnArg),
}

impl<T: Parse + ToNode> Parse for NodeId<T> {
    fn parse(parser: ParseStream) -> Result<Self> {
        let t = parser.parse_with_span()?;
        let id = parser.ctx().add(t);
        Ok(id)
    }
}
