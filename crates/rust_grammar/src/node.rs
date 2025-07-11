use derive_macro::CmpSyn;
use molt_lib::ToNode;

use crate::expr::Arm;
use crate::pat::Pat;
use crate::{Expr, Field, Ident, Item, Lit, Stmt, Type, Visibility};

macro_rules! define_node_and_kind {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub enum Kind {
            $(
                $variant_name,
            )*
        }

        #[derive(CmpSyn)]
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

                fn from_node(node: Node) -> Option<Self> {
                    #[allow(irrefutable_let_patterns)]
                    if let Node::$variant_name(item) = node {
                        Some(item)
                    } else {
                        None
                    }
                }

                fn from_node_ref(node: &Node) -> Option<&Self> {
                    #[allow(irrefutable_let_patterns)]
                    if let Node::$variant_name(item) = node {
                        Some(item)
                    } else {
                        None
                    }
                }

                fn from_node_ref_mut(node: &mut Node) -> Option<&mut Self> {
                    #[allow(irrefutable_let_patterns)]
                    if let Node::$variant_name(item) = node {
                        Some(item)
                    } else {
                        None
                    }
                }

                fn kind() -> Kind {
                    Kind::$variant_name
                }
            }
        )*
    }
}

define_node_and_kind! {
    (Lit, Lit),
    (Item, Item),
    (Expr, Expr),
    (Stmt, Stmt),
    (Type, Type),
    (Field, Field),
    (Ident, Ident),
    (Arm, Arm),
    (Pat, Pat),
    (Visibility, Visibility),
}
