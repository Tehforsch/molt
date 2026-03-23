//! Defines the `Node` and `Kind` types.  The `Node` is the top level type
//! stored in the Ctx. It represents all syntactic elements of the grammar which
//! molt can pattern match on. In other words, every item that eventually
//! becomes stored in a `NodeId` has its representation in the `Node` enum.
//!
//! The `Kind` represents various syntactic kinds which the user can specify in
//! a molt file (e.g. the `Expr` in `let x: Expr = ...`). Some `Kind` variants
//! may match multiple `Node` variants. For example, a function may appear in
//! top level position (`Item` node variant) or within an `impl` block
//! (`ImplItem` node variant), however, in most cases, the user does not want to
//! differentiate between these. To this end, the `NodeType` trait defines a
//! `kinds_are_comparable` function which returns whether two `Kind` should be
//! compared by the matching algorithm.
//!
//! The `NodeKind` type exists as a representation of the discriminant of the
//! `Node` enum, so that each variant of `Node` is represented by exactly one
//! `NodeKind`.

use crate::CmpSyn;

#[allow(unused)]
pub struct Kind<NodeKind> {
    kinds: Vec<NodeKind>,
}

pub trait ToNode<Node: NodeType>: Sized {
    fn to_node(self) -> Node;
    fn from_node_ref(node: &Node) -> Option<&Self>;
    fn from_node_ref_mut(node: &mut Node) -> Option<&mut Self>;
    fn node_kind() -> Node::NodeKind;
    fn kind() -> Kind<Node::NodeKind> {
        Kind {
            kinds: vec![Self::node_kind()],
        }
    }
}

impl<T: NodeType> ToNode<T> for T {
    fn to_node(self) -> T {
        self
    }

    fn from_node_ref(node: &T) -> Option<&Self> {
        Some(node)
    }

    fn from_node_ref_mut(node: &mut T) -> Option<&mut Self> {
        Some(node)
    }

    fn node_kind() -> T::NodeKind {
        panic!()
    }
}

pub trait NodeType: CmpSyn<Self> + Sized {
    type NodeKind: Copy + std::fmt::Debug + PartialEq;

    fn node_kind(&self) -> Self::NodeKind;
    fn is_of_kind(&self, pat_kind: Self::NodeKind) -> bool {
        self.node_kind() == pat_kind
    }
}
