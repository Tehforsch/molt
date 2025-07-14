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
//! defines a `is_comparable` function which returns whether two
//! `Kind` should be compared by the matching algorithm.
//!
//! For simplicity, we define at least one `Kind` per `Node`
//! variant. This mapping is given by the `NodeType::kind` method.

pub trait ToNode<Node: NodeType>: Sized {
    fn to_node(self) -> Node;
    fn from_node(node: Node) -> Option<Self>;
    fn from_node_ref(node: &Node) -> Option<&Self>;
    fn from_node_ref_mut(node: &mut Node) -> Option<&mut Self>;
    fn kind() -> Node::Kind;
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

    fn kind() -> T::Kind {
        panic!()
    }
}

pub trait NodeType {
    type Kind: Copy + std::fmt::Debug;

    fn kind(&self) -> Self::Kind;
    fn is_comparable(kind_pat: Self::Kind, kind_real: Self::Kind) -> bool;
}
