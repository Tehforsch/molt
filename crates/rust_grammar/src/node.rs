use derive_macro::CmpSyn;
use molt_lib::{Id, KindType, Pattern, ToNode};

use crate::expr::Arm;
use crate::item::ImplItem;
use crate::parse::discouraged::Speculative;
use crate::parse::{ParseNode, ParseStream};
use crate::pat::Pat;
use crate::{
    Expr, Field, FieldNamed, FieldUnnamed, Ident, Item, Lit, PatMulti, Stmt, Type, Visibility,
};

macro_rules! define_node {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        #[derive(CmpSyn)]
        pub enum Node {
            $(
                $variant_name($ty),
            )*
        }

        #[derive(Clone, Copy, Debug)]
        pub enum NodeKind {
            $(
                $variant_name,
            )*
        }

        impl molt_lib::NodeType for Node {
            type Kind = Kind;
            type NodeKind = NodeKind;

            fn node_kind(&self) -> NodeKind {
                match self {
                    $(
                        Self::$variant_name(_) => NodeKind::$variant_name,
                    )*
                }
            }

            fn is_of_kind(&self, pat_kind: Self::Kind) -> bool {
                is_of_kind(self, pat_kind)
            }
        }

        impl From<NodeKind> for Kind {
            fn from(val: NodeKind) -> Kind {
                match val {
                    $(
                        NodeKind::$variant_name => Kind::$variant_name,
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

                fn node_kind() -> NodeKind {
                    NodeKind::$variant_name
                }
            }
        )*
    }
}

macro_rules! define_kind {
    ($(($variant_name: ident, $kind_name: ident, $parse_ty: ty $(,$sub_ty: ty)?)),* $(,)?) => {
        #[derive(Copy, Clone, Debug)]
        pub enum Kind {
            $( $variant_name, )*
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

        impl KindType<NodeKind> for Kind {
            fn is_comparable_to(&self, node_kind: NodeKind) -> bool {
                match self {
                    $(
                        Kind::$variant_name => matches!(node_kind, NodeKind::$kind_name),
                    )*
                }
            }

            fn into_node_kind(self) -> NodeKind {
                match self {
                    $(
                        Kind::$variant_name => NodeKind::$kind_name,
                    )*
                }
            }
        }

        mod kind_kws {
            $(
                crate::custom_keyword!($variant_name);
            )*
        }

        impl crate::parse::Parse for Kind {
            fn parse(input:crate::parse:: ParseStream) -> crate::Result<Self> {
                $(
                    if input.peek(kind_kws::$variant_name) {
                        let _: kind_kws::$variant_name = input.parse()?;
                        return Ok(Kind::$variant_name);
                    }
                )*
                Err(input.error("Invalid kind."))
            }
        }

        fn is_of_kind(node: &Node, pat_kind: Kind) -> bool {
            $(
                match_impl!(node, pat_kind, $variant_name, $parse_ty $(,$sub_ty)?);
            )*
            unreachable!()
        }

        pub fn parse_node_with_kind(input: crate::parse::ParseStream, kind: Kind) -> crate::Result<Id> {
            $(
                parse_impl!(input, kind, $variant_name, $parse_ty $(,$sub_ty)?);
            )*
            unreachable!()
        }
    }
}

macro_rules! parse_impl {
    ($input: ident, $kind: ident, $variant_name: ident, $parse_ty: ty) => {
        if let Kind::$variant_name = $kind {
            let parsed = $input.parse_id::<$parse_ty>()?;
            return Ok(parsed.into());
        }
    };
    ($input: ident, $kind: ident, $variant_name: ident, $parse_ty: ty, $sub_ty: ty) => {
        if let Kind::$variant_name = $kind {
            let parsed = $input.parse_spanned_pat::<$parse_ty>()?;
            match &*parsed {
                Pattern::Real(real) => {
                    if !<$sub_ty>::is_of_sub_kind(real) {
                        return Err($input.error(format!("Expected {}", <$sub_ty>::expected_str())));
                    }
                }
                Pattern::Pat(_) => {
                    // TODO: Do we need to do anything here?
                }
            }
            return Ok($input.add_pat(parsed).into());
        }
    };
}

macro_rules! match_impl {
    ($node: ident, $pat_kind: ident, $variant_name: ident, $parse_ty: ty) => {
        if let Kind::$variant_name = $pat_kind {
            return matches!($node, Node::$variant_name(_));
        }
    };
    ($node: ident, $pat_kind: ident, $variant_name: ident, $parse_ty: ty, $sub_ty: ty) => {
        if let Kind::$variant_name = $pat_kind {
            if let Some(t) = <$parse_ty>::from_node_ref($node) {
                return <$sub_ty>::is_of_sub_kind(t);
            } else {
                return false;
            }
        }
    };
}

define_node! {
    (Arm, Arm),
    (Expr, Expr),
    (Field, Field),
    (Ident, Ident),
    (Item, Item),
    (ImplItem, ImplItem),
    (Lit, Lit),
    (Pat, Pat),
    (Stmt, Stmt),
    (Type, Type),
    (Visibility, Visibility),
}

define_kind! {
    (Arm, Arm, Arm),
    (Expr, Expr, Expr),
    (Field, Field, Field),
    (Ident, Ident, Ident),
    (Item, Item, Item),
    (ImplItem, ImplItem, ImplItem),
    (Fn, Item, Item, Fn),
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

trait SubKind {
    type Super;

    fn is_of_sub_kind(sup: &Self::Super) -> bool;
    fn expected_str() -> &'static str;
}

struct Fn;

impl SubKind for Fn {
    type Super = Item;

    fn is_of_sub_kind(sup: &Item) -> bool {
        matches!(sup, Item::Fn(_))
    }

    fn expected_str() -> &'static str {
        "function"
    }
}
