//! Defines the `Node` and `NodeKind` types.  The `Node` is the top level type
//! stored in the Ctx. It represents all syntactic elements of the grammar which
//! molt can pattern match on. In other words, every item that eventually
//! becomes stored in a `NodeId` has its representation in the `Node` enum.
//!
//! The `NodeKind` type is the discriminant of the `Node`, so it uniquely
//! identifies a variant of `Node` without the data.
//!
//! Molt variables are statically typed. Molt variables referring to syntactic
//! expressions are typed not with a single `NodeKind` but with an arbitrary
//! number of them. This is represented by the `Kinds` type. This is done in
//! order to support the fact that the user typically does not distinguish
//! between functions in an `impl` block and those outside of one, but those two
//! are represented by different types and have slightly different fields. To
//! allow the user to represent all of them with a single concept (a `Fn` kind),
//! we map `Fn` to the type Kind([`ItemFn`, `ImplItemFn`]).

use crate::CmpSyn;

pub trait ToNode<Node: NodeType>: Sized {
    fn to_node(self) -> Node;
    fn from_node_ref(node: &Node) -> Option<&Self>;
    fn from_node_ref_mut(node: &mut Node) -> Option<&mut Self>;
    fn node_kind() -> Node::NodeKind;
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
    fn is_of_kind(&self, pat_kinds: &Kinds<Self::NodeKind>) -> bool {
        pat_kinds.kinds.contains(&self.node_kind())
    }
}

/// Represents a list of possible kinds that a variable may have
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Kinds<NodeKind> {
    kinds: Vec<NodeKind>,
}

impl<NodeKind: PartialOrd + Ord + Clone + Copy + PartialEq + Eq> Kinds<NodeKind> {
    pub(crate) fn is_comparable_to(&self, k2: &Kinds<NodeKind>) -> bool {
        // Think about this. Probably the order is not correct
        if self.kinds.len() == 1 {
            k2.kinds.iter().any(|k| *k == self.kinds[0])
        } else if k2.kinds.len() == 1 {
            self.kinds.iter().any(|k| k2.kinds[0] == *k)
        } else {
            todo!()
        }
    }

    pub(crate) fn single(node_kind: NodeKind) -> Kinds<NodeKind> {
        Self::new(vec![node_kind])
    }

    pub(crate) fn new(mut node_kinds: Vec<NodeKind>) -> Kinds<NodeKind> {
        // Sort to prevent type mismatch due to different
        // orderings.
        node_kinds.sort();
        Self { kinds: node_kinds }
    }

    pub(crate) fn get_first(&self) -> NodeKind {
        self.kinds[0]
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &NodeKind> {
        self.kinds.iter()
    }

    pub(crate) fn is_superset_of(&self, k2: &Kinds<NodeKind>) -> bool {
        k2.iter().all(|k2| self.kinds.iter().any(|kind| kind == k2))
    }
}

