use derive_macro::CmpSyn;
use molt_lib::{Id, NodeId, ToNode};

use crate::expr::Arm;
use crate::parse::ParseStream;
use crate::parse::discouraged::Speculative;
use crate::pat::Pat;
use crate::{
    Expr, Field, FieldNamed, FieldUnnamed, Ident, Item, Lit, PatMulti, Stmt, Type, Visibility,
};

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

        pub fn parse_node_with_kind(parser: crate::parse::ParseStream, kind: UserKind) -> crate::Result<Id> {
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
    (Visibility, Visibility, Visibility),
}
