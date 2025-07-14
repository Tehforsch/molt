//! Defines the `Node` and `Kind` types.  The `Node` is the top level
//! type stored in the Ctx. It represents all syntactic elements of
//! the grammar which molt can pattern match on. In other words, every
//! item that eventually becomes stored in a `NodeId` has its
//! representation in the `Node` enum.
//!
//! The `Kind` represents various syntactic kinds which the user can
//! specify in a molt file (e.g. the `Expr` in `let x: Expr = ...`).
//! There are `Kind` variants that have no equivalent `Node`
//! variant. For example, a function may be represented by the `Fn`
//! kind and the `Item` kind, but is internally stored in a `Item`
//! Node variant.  Moreover, some `Kind` variants may match multiple
//! `Node` variants. For example, a function may appear in top level
//! position (`Item` node variant) or within an `impl` block
//! (`ImplItem` node variant), however the user most likely does not
//! want to differentiate these. To this end, the `NodeType` trait
//! defines a `kinds_are_comparable` function which returns whether two
//! `Kind` should be compared by the matching algorithm.
//!
//! For simplicity, each node type is represented by at least one
//! `Kind` variant. This is expressed by the `Node::node_kind` method
//! which returns a `NodeKind` struct which can be converted to the
//! corresponding `Kind`.

pub trait ToNode<Node: NodeType>: Sized {
    fn to_node(self) -> Node;
    fn from_node(node: Node) -> Option<Self>;
    fn from_node_ref(node: &Node) -> Option<&Self>;
    fn from_node_ref_mut(node: &mut Node) -> Option<&mut Self>;
    fn node_kind() -> Node::NodeKind;
}

impl<T: NodeType> ToNode<T> for T {
    fn to_node(self) -> T {
        self
    }

    fn from_node(node: T) -> Option<Self> {
        Some(node)
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

pub trait NodeType {
    type NodeKind: Copy + std::fmt::Debug + Into<Self::Kind>;
    type Kind: Copy + KindType<Self::NodeKind>;

    fn node_kind(&self) -> Self::NodeKind;
    fn is_of_kind(&self, pat_kind: Self::Kind) -> bool;
}

pub trait KindType<NodeKind> {
    fn is_comparable_to(&self, node_kind: NodeKind) -> bool;
    fn into_node_kind(self) -> NodeKind;
}
