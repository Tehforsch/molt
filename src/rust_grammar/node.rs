use crate::ctx::VarKind;
use crate::match_pattern::IsMatch;
use crate::node::Kinds;
use crate::rust_grammar::generics::Generics;
use crate::{CmpSyn, Matcher, RawNodeId, ToNode};

use crate::parser::parse::discouraged::Speculative;
use crate::parser::parse::{ParseStream, ParseTerm};
use crate::rust_grammar::expr::Arm;
use crate::rust_grammar::item::{ImplItem, ImplItemFn, ItemFn};
use crate::rust_grammar::pat::Pat;
use crate::rust_grammar::{
    Expr, Field, FieldNamed, FieldUnnamed, Ident, Item, Lit, PatMulti, Stmt, Type, Vis,
};

macro_rules! define_node {
    ($(($variant_name: ident, $ty: ty, $parse_ty: ty)),* $(,)?) => {
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
            fn cmp_syn_diagonal(&self, ctx: &mut Matcher<Node>, term: &Self) -> IsMatch<bool> {
                match self {
                    $(
                        Node::$variant_name(t1) => {
                            if let Node::$variant_name(t2) = term {
                                ctx.cmp_syn_ignore_rule(t1, t2)?;
                                return Ok(true)
                            }
                        }
                    )*
                }
                Ok(false)
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub enum NodeKind {
            $(
                $variant_name,
            )*
        }

        impl std::fmt::Display for NodeKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$variant_name => write!(f, stringify!($variant_name)),
                    )*
                }
            }
        }

        impl crate::NodeType for Node {
            type NodeKind = NodeKind;

            fn node_kind(&self) -> NodeKind {
                match self {
                    $(
                        Self::$variant_name(_) => NodeKind::$variant_name,
                    )*
                }
            }
        }

        impl NodeKind {
            pub fn is_comparable_to(self, other: NodeKind) -> bool {
                self == other
            }
        }

        $(
            impl ToNode<Node> for $ty {
                fn to_node(self) -> Node {
                    Node::$variant_name(self)
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

        pub fn parse_node_with_kinds(input: crate::parser::parse::ParseStream, kinds: &Kinds<NodeKind>) -> crate::parser::Result<RawNodeId> {
             // TODO! Speculatively parse all? For now this just assumes they
            // all parse the same.
            let kind = kinds.get_first();
            match kind {
                $(
                    NodeKind::$variant_name => {
                        let parsed = input.parse_id::<$parse_ty>()?;
                        Ok(parsed.into())
                    }
                )*
            }
        }
    }
}

macro_rules! define_kind_kws {
    ($(($kind_kw: ident, [$($node_kind: ident),* $(,)?])),* $(,)?) => {
        mod kind_kws {
            $(
                crate::custom_keyword!($kind_kw);
            )*
        }

        impl NodeKind {
            pub(crate) fn peek(lookahead: &mut crate::parser::lookahead::Lookahead1) -> bool {
                $(
                    if lookahead.peek(kind_kws::$kind_kw) {
                        return true
                    }
                )*
                false
            }
        }

        impl crate::parser::parse::Parse for Kinds<NodeKind> {
            fn parse(input:crate::parser::parse:: ParseStream) -> crate::parser::Result<Self> {
                $(
                    if input.peek(kind_kws::$kind_kw) {
                        let _: kind_kws::$kind_kw = input.parse()?;
                        return Ok(Kinds::new(vec![
                            $(
                                NodeKind::$node_kind
                            ),*
                        ]));
                    }
                )*
                Err(input.error("Invalid kind."))
            }
        }

        pub fn add_field_defs_for_node_types(defs: &mut crate::molt_lang::TypeDefinitionsBuilder) {
            $(
                $(
                    defs.add::<$node_kind>();
                )*
            )*
        }
    }
}

define_node! {
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
    (ItemFn, ItemFn, ItemFn),
    (ImplItemFn, ImplItemFn, ImplItemFn),
}

define_kind_kws! {
    (Arm, [Arm]),
    (Expr, [Expr]),
    (Field, [Field]),
    (Ident, [Ident]),
    (Item, [Item]),
    (ImplItem, [ImplItem]),
    (Lit, [Lit]),
    (Pat, [Pat]),
    (Stmt, [Stmt]),
    (Type, [Type]),
    (Vis, [Vis]),
    (Generics, [Generics]),
    (Fn, [ItemFn, ImplItemFn]),
}

impl CmpSyn<Node> for Node {
    fn cmp_syn(&self, ctx: &mut Matcher<Node>, term: &Self) -> IsMatch {
        if self.cmp_syn_diagonal(ctx, term)? {
            // Two nodes with equal variants have already
            // been compared. We're done.
            return IsMatch::Ok(());
        }
        // Explicitly keep this exhaustive to make sure
        // we don't miss this impl when we add a new variant.
        match (self, term) {
            (Node::Item(item), Node::ImplItem(impl_item)) => ctx.cmp_syn(impl_item, item),
            (Node::ImplItem(impl_item), Node::Item(item)) => ctx.cmp_syn(impl_item, item),
            // TODO: Check: Does flipping the args here cause errors with bindings?
            (Node::ItemFn(f1), Node::ImplItemFn(f2)) => ctx.cmp_syn(f2, f1),
            (Node::ImplItemFn(f1), Node::ItemFn(f2)) => ctx.cmp_syn(f1, f2),
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
            | (Node::ItemFn(_), _)
            | (Node::ImplItemFn(_), _)
            | (Node::Vis(_), _) => ctx.no_match(),
        }
    }
}

impl ParseTerm for Field {
    type Target = Field;

    fn parse_item(input: ParseStream) -> crate::parser::Result<Self::Target> {
        // Let's see how this works out in practice.
        // We speculatively parse a named field and
        // if it doesn't work out, we parse an unnamed field.
        let fork = input.fork();
        if let Ok(field) = FieldNamed::parse_item(input) {
            input.advance_to(&fork);
            Ok(field)
        } else {
            Ok(FieldUnnamed::parse_item(input)?)
        }
    }
}

impl VarKind<Kinds<NodeKind>> {
    pub(crate) fn is_comparable_to(&self, kind: VarKind<Kinds<NodeKind>>) -> bool {
        match (self, kind) {
            (VarKind::Single(k1), VarKind::Single(k2)) => k1.is_comparable_to(&k2),
            (VarKind::List(k1), VarKind::List(k2)) => k1.is_comparable_to(&k2),
            (_, _) => false,
        }
    }
}
