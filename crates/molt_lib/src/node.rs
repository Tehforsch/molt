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
