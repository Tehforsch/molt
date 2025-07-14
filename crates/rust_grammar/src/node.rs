use derive_macro::CmpSyn;
use molt_lib::{Id, ToNode};

use crate::expr::Arm;
use crate::parse::discouraged::Speculative;
use crate::parse::{ParseNode, ParseStream};
use crate::pat::Pat;
use crate::{
    Expr, Field, FieldNamed, FieldUnnamed, Ident, Item, Lit, PatMulti, Stmt, Type, Visibility,
};

macro_rules! define_node_and_kind {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        #[derive(CmpSyn)]
        pub enum Node {
            $(
                $variant_name($ty),
            )*
        }

        impl molt_lib::GetKind for Node {
            type Kind = UserKind;

            fn kind(&self) -> UserKind {
                match self {
                    $(
                        Self::$variant_name(_) => UserKind::$variant_name,
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

                fn kind() -> UserKind {
                    UserKind::$variant_name
                }
            }
        )*
    }
}

macro_rules! define_user_kind {
    ($(($variant_name: ident, $kind_name: ident, $parse_ty: ty)),*$(,)?) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub enum UserKind {
            $( $variant_name, )*
        }

        impl std::fmt::Display for UserKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$variant_name => write!(f, stringify!($variant_name)),
                    )*
                }
            }
        }

        mod kind_kws {
            $(
                crate::custom_keyword!($variant_name);
            )*
        }

        impl crate::parse::Parse for UserKind {
            fn parse(input:crate::parse:: ParseStream) -> crate::Result<Self> {
                $(
                    if input.peek(kind_kws::$variant_name) {
                        let _: kind_kws::$variant_name = input.parse()?;
                        return Ok(UserKind::$variant_name);
                    }
                )*
                Err(input.error("Invalid kind."))
            }
        }

        pub fn parse_node_with_kind(input: crate::parse::ParseStream, kind: UserKind) -> crate::Result<Id> {
            $(
                if let UserKind::$variant_name = kind {
                    let parsed = input.parse_id::<$parse_ty>()?;
                    return Ok(parsed.into());
                }
            )*
            unreachable!()
        }
    }
}

define_node_and_kind! {
    (Arm, Arm),
    (Expr, Expr),
    (Field, Field),
    (Ident, Ident),
    (Item, Item),
    (Lit, Lit),
    (Pat, Pat),
    (Stmt, Stmt),
    (Type, Type),
    (Visibility, Visibility),
}

define_user_kind! {
    (Arm, Arm, Arm),
    (Expr, Expr, Expr),
    (Field, Field, Field),
    (Ident, Ident, Ident),
    (Item, Item, Item),
    (Lit, Lit, Lit),
    (Pat, Pat, PatMulti),
    (Stmt, Stmt, Stmt),
    (Type, Type, Type),
    (Visibility, Visibility, Visibility),
}

impl ParseNode for Field {
    type Target = Field;

    fn parse_node(input: ParseStream) -> crate::Result<Self::Target> {
        // Let's see how this works out in practice.
        // We speculatively parse a named field and
        // if it doesn't work out, we parse an unnamed field.
        let fork = input.fork();
        if let Ok(field) = fork.parse_node::<FieldNamed>() {
            input.advance_to(&fork);
            Ok(field)
        } else {
            Ok(input.parse_node::<FieldUnnamed>()?)
        }
    }
}
