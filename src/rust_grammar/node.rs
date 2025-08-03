use crate::match_pattern::IsMatch;
use crate::rust_grammar::generics::Generics;
use crate::{CmpSyn, Id, KindType, Matcher, Pattern, ToNode};

use crate::parser::parse::discouraged::Speculative;
use crate::parser::parse::{ParseNode, ParseStream};
use crate::rust_grammar::expr::Arm;
use crate::rust_grammar::item::ImplItem;
use crate::rust_grammar::pat::Pat;
use crate::rust_grammar::{
    Expr, Field, FieldNamed, FieldUnnamed, Ident, Item, Lit, PatMulti, Stmt, Type, Vis,
};

macro_rules! define_node {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        pub enum Node {
            $(
                $variant_name($ty),
            )*
        }

        impl Node {
            /// Compare each enum variant with itself. This is not the
            /// full `CmpSyn` implementation, because off-diagonal
            /// comparisons exists (such as `Item` and `ImplItem`.)
            /// These are checked in the real `CmpSyn` impl below.
            fn cmp_syn_diagonal(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch<bool> {
                match self {
                    $(
                        Node::$variant_name(t1) => {
                            if let Node::$variant_name(t2) = pat {
                                ctx.cmp_syn_ignore_rule(t1, t2)?;
                                return Ok(true)
                            }
                        }
                    )*
                }
                Ok(false)
            }
        }

        #[derive(Clone, Copy, Debug)]
        pub enum NodeKind {
            $(
                $variant_name,
            )*
        }

        impl crate::NodeType for Node {
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

        impl crate::parser::parse::Parse for Kind {
            fn parse(input:crate::parser::parse:: ParseStream) -> crate::parser::Result<Self> {
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

        pub fn parse_node_with_kind(input: crate::parser::parse::ParseStream, kind: Kind) -> crate::parser::Result<Id> {
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
            let id = $input.parse_id::<$parse_ty>()?;
            match $input.ctx().get(id) {
                Pattern::Real(node) => {
                    if !<$sub_ty>::is_of_sub_kind(&node) {
                        return Err($input.error(format!("Expected {}", <$sub_ty>::expected_str())));
                    }
                }
                Pattern::Pat(_) => {
                    // TODO: Do we need to do anything here?
                }
            }
            return Ok(id.into());
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
            return <$sub_ty>::is_of_sub_kind($node);
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
    (Vis, Vis),
    (Generics, Generics),
}

define_kind! {
    (Arm, Arm, Arm),
    (Expr, Expr, Expr),
    (Field, Field, Field),
    (Ident, Ident, Ident),
    (Item, Item, Item),
    (ImplItem, ImplItem, ImplItem),
    (Lit, Lit, Lit),
    (Pat, Pat, PatMulti),
    (Stmt, Stmt, Stmt),
    (Type, Type, Type),
    (Vis, Vis, Vis),
    (Generics, Generics, Generics),
    // Subtypes
    (Fn, Item, Item, Fn),
    (Mod, Item, Item, Mod),
}

impl CmpSyn<Node> for Node {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, pat: &Self) -> IsMatch {
        if self.cmp_syn_diagonal(ctx, pat)? {
            // Two nodes with equal variants have already
            // been compared. We're done.
            return IsMatch::Ok(());
        }
        // Explicitly keep this exhaustive to make sure
        // we don't miss this impl when we add a new variant.
        match (self, pat) {
            (Node::Item(item), Node::ImplItem(impl_item))
            | (Node::ImplItem(impl_item), Node::Item(item)) => ctx.cmp_syn(impl_item, item),
            (Node::Arm(_), _)
            | (Node::Expr(_), _)
            | (Node::Field(_), _)
            | (Node::Ident(_), _)
            | (Node::Lit(_), _)
            | (Node::Pat(_), _)
            | (Node::Stmt(_), _)
            | (Node::Type(_), _)
            | (Node::Item(_), _)
            | (Node::ImplItem(_), _)
            | (Node::Generics(_), _)
            | (Node::Vis(_), _) => ctx.no_match(),
        }
    }
}

impl ParseNode for Field {
    type Target = Field;

    fn parse_node(input: ParseStream) -> crate::parser::Result<Self::Target> {
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
    fn is_of_sub_kind(node: &Node) -> bool;
    fn expected_str() -> &'static str;
}

struct Fn;

impl SubKind for Fn {
    fn is_of_sub_kind(node: &Node) -> bool {
        matches!(
            node,
            Node::Item(Item::Fn(_)) | Node::ImplItem(ImplItem::Fn(_))
        )
    }

    fn expected_str() -> &'static str {
        "function"
    }
}

struct Mod;

impl SubKind for Mod {
    fn is_of_sub_kind(node: &Node) -> bool {
        matches!(node, Node::Item(Item::Mod(_)))
    }

    fn expected_str() -> &'static str {
        "module"
    }
}
