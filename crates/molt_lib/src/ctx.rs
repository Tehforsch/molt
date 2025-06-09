use std::marker::PhantomData;

#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: usize,
    end: usize,
    fake: bool,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            fake: false,
        }
    }

    pub fn byte_range(&self) -> std::ops::Range<usize> {
        if self.fake {
            panic!("Fake span");
        }
        self.start..self.end
    }

    pub fn fake() -> Self {
        Span {
            start: 0,
            end: 0,
            fake: true,
        }
    }

    pub fn join(&self, span: impl Into<Span>) -> Span {
        let span = span.into();
        if self.fake || span.fake {
            panic!();
        }
        Span {
            start: self.start.min(span.start),
            end: self.end.max(span.end),
            fake: false,
        }
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
            fake: false,
        }
    }
}

pub trait WithSpan: Sized {
    fn with_span(self, span: Span) -> Spanned<Self> {
        Spanned::new(self, span)
    }

    fn pattern_with_span(self, span: Span) -> Spanned<Pattern<Self, Id>> {
        Spanned::new(self, span).as_pattern()
    }
}

impl<T: Sized> WithSpan for T {}

#[derive(Debug)]
pub struct Spanned<T> {
    span: Span,
    item: T,
}

pub type SpannedPat<T> = Spanned<Pattern<T, Id>>;

impl<T> Spanned<T> {
    fn new(item: T, span: Span) -> Self {
        Self { span, item }
    }

    pub fn map<S>(self, f: impl Fn(T) -> S) -> Spanned<S> {
        Spanned {
            span: self.span,
            item: f(self.item),
        }
    }

    pub fn join<S>(&self, rhs: &Spanned<S>) -> Span {
        self.span.join(rhs.span)
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn take(self) -> T {
        self.item
    }

    pub fn as_pattern(self) -> SpannedPat<T> {
        self.map(|item| Pattern::Real(item))
    }

    pub fn decompose(self) -> (Span, T) {
        (self.span, self.item)
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = span
    }
}

impl<T> SpannedPat<T> {
    pub fn is_var(&self) -> bool {
        matches!(self.item, Pattern::Pat(_))
    }

    pub fn real(&self) -> Option<&T> {
        self.item.real()
    }

    pub fn unwrap_real(self) -> Spanned<T> {
        self.map(|item| item.unwrap_real())
    }

    pub fn unwrap_var(self) -> NodeId<T> {
        match self.item {
            Pattern::Pat(t) => t.typed(),
            Pattern::Real(_) => panic!("unwrap_real called on real variant."),
        }
    }

    pub fn do_if_real(&mut self, f: impl Fn(&mut T)) {
        match &mut self.item {
            Pattern::Real(t) => f(t),
            Pattern::Pat(_) => {}
        }
    }

    pub fn map_real<S>(self, f: impl Fn(T) -> S) -> SpannedPat<S> {
        self.map(|item| match item {
            Pattern::Real(t) => Pattern::Real(f(t)),
            Pattern::Pat(id) => Pattern::Pat(id),
        })
    }
}

impl<T> SpannedPat<Option<T>> {
    pub fn transpose(self) -> Option<SpannedPat<T>> {
        match self.item.transpose() {
            Some(pat) => Some(Spanned {
                span: self.span,
                item: pat,
            }),
            None => None,
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}

pub trait ToNode<Node: GetKind>: Sized {
    fn to_node(self) -> Node;
    fn from_node(node: Node) -> Option<Self>;
    fn from_node_ref(node: &Node) -> Option<&Self>;
    fn from_node_ref_mut(node: &mut Node) -> Option<&mut Self>;
    fn kind() -> Node::Kind;
}

impl<T: GetKind> ToNode<T> for T {
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

pub trait GetKind {
    type Kind: Copy + std::fmt::Debug + PartialEq + Eq;

    fn kind(&self) -> Self::Kind;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Pattern<Real, Pat> {
    Real(Real),
    Pat(Pat),
}

impl<Real, Pat> Pattern<Real, Pat> {
    pub fn unwrap_real(self) -> Real {
        match self {
            Pattern::Real(real) => real,
            Pattern::Pat(_) => panic!("unwrap called on pattern variant."),
        }
    }

    pub fn real(&self) -> Option<&Real> {
        match &self {
            Pattern::Real(t) => Some(t),
            Pattern::Pat(_) => None,
        }
    }

    pub fn map_real<S>(self, f: impl Fn(Real) -> S) -> Pattern<S, Pat> {
        match self {
            Pattern::Real(t) => Pattern::Real(f(t)),
            Pattern::Pat(id) => Pattern::Pat(id),
        }
    }
}

impl<Real, Pat: Copy> Pattern<Real, Pat> {
    pub fn as_ref(&self) -> Pattern<&Real, Pat> {
        match self {
            Pattern::Real(real) => Pattern::Real(&real),
            Pattern::Pat(var) => Pattern::Pat(*var),
        }
    }
}

impl<Real, Pat> Pattern<Option<Real>, Pat> {
    pub fn transpose(self) -> Option<Pattern<Real, Pat>> {
        match self {
            Pattern::Real(opt) => opt.map(Pattern::Real),
            Pattern::Pat(var) => Some(Pattern::Pat(var)),
        }
    }
}

type InternalId = Pattern<usize, usize>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Id(InternalId);

impl Id {
    pub fn is_pat(&self) -> bool {
        match self.0 {
            Pattern::Real(_) => false,
            Pattern::Pat(_) => true,
        }
    }
}

pub struct NodeId<T> {
    _marker: PhantomData<T>,
    id: Id,
}

impl<T> NodeId<T> {
    // used instead of Expr::PLACEHOLDER in parsing.
    pub fn placeholder() -> Self {
        Self {
            id: Id(InternalId::Real(usize::MAX)),
            _marker: PhantomData,
        }
    }
}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for NodeId<T> {}

impl<T> std::fmt::Debug for NodeId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id.0.fmt(f)
    }
}

impl<T> PartialEq for NodeId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl<T> std::hash::Hash for NodeId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<T> Eq for NodeId<T> {}

impl<T> From<NodeId<T>> for Id {
    fn from(value: NodeId<T>) -> Self {
        value.id
    }
}

impl Id {
    fn typed<T>(self) -> NodeId<T> {
        NodeId {
            id: self,
            _marker: PhantomData,
        }
    }

    pub fn unwrap_idx(self) -> usize {
        match self.0 {
            Pattern::Real(idx) => idx,
            Pattern::Pat(idx) => idx,
        }
    }
}

pub struct Var<Node: GetKind> {
    name: String,
    kind: Node::Kind,
}

impl<Node: GetKind> PartialEq for Var<Node> {
    fn eq(&self, other: &Self) -> bool {
        debug_assert_eq!(self.kind, other.kind);
        self.name.eq(&other.name)
    }
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub id: Id,
    pub node: Option<Id>,
}

impl<Node: GetKind> Var<Node> {
    pub fn new(name: String, kind: Node::Kind) -> Self {
        Self { name, kind }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

pub struct Ctx<Node: GetKind> {
    nodes: Vec<Node>,
    vars: Vec<Var<Node>>,
    spans: Vec<Span>,
}

impl<Node: GetKind> Ctx<Node> {
    fn add_node(&mut self, node: Spanned<Node>) -> Id {
        self.spans.push(node.span);
        self.nodes.push(node.item);
        Id(InternalId::Real(self.nodes.len() - 1))
    }

    pub fn add<T: ToNode<Node>>(&mut self, t: Spanned<T>) -> NodeId<T> {
        self.add_node(t.map(|item| item.to_node())).typed()
    }

    fn add_var_internal(&mut self, var: Var<Node>) -> Id {
        self.vars.push(var);
        Id(InternalId::Pat(self.vars.len() - 1))
    }

    pub fn add_var<T: ToNode<Node>>(&mut self, var: Var<Node>) -> NodeId<T> {
        let id = if let Some(id) = self.add_existing_var(&var.name) {
            id
        } else {
            self.add_var_internal(var)
        };
        id.typed()
    }

    pub fn add_pat<T: ToNode<Node>>(&mut self, item: SpannedPat<T>) -> NodeId<T> {
        match item.item {
            Pattern::Real(_) => self.add(item.unwrap_real()),
            Pattern::Pat(var) => var.typed(),
        }
    }

    pub fn add_existing_var(&self, var: &str) -> Option<Id> {
        self.vars
            .iter()
            .enumerate()
            .find(|(_, v)| var == v.name)
            .map(|(i, _)| Id(InternalId::Pat(i)))
    }

    pub fn get<T: ToNode<Node>>(&self, id: impl Into<Id>) -> Pattern<&T, Id> {
        let id = id.into();
        match id.0 {
            InternalId::Real(idx) => Pattern::Real(T::from_node_ref(&self.nodes[idx]).unwrap()),
            InternalId::Pat(_) => Pattern::Pat(id),
        }
    }

    pub fn get_mut<T: ToNode<Node>>(&mut self, id: NodeId<T>) -> Pattern<&mut T, Id> {
        let id: Id = id.into();
        match id.0 {
            InternalId::Real(idx) => {
                Pattern::Real(T::from_node_ref_mut(&mut self.nodes[idx]).unwrap())
            }
            InternalId::Pat(_) => Pattern::Pat(id),
        }
    }

    pub fn get_real<T: ToNode<Node>>(&self, id: impl Into<Id>) -> Option<&T> {
        match self.get(id) {
            Pattern::Real(t) => Some(t),
            Pattern::Pat(_) => None,
        }
    }

    pub fn get_real_mut<T: ToNode<Node>>(&mut self, id: NodeId<T>) -> Option<&mut T> {
        match self.get_mut(id) {
            Pattern::Real(t) => Some(t),
            Pattern::Pat(_) => None,
        }
    }

    pub fn change_node<T: ToNode<Node>, S: ToNode<Node>>(
        &mut self,
        id: NodeId<T>,
        f: impl Fn(T) -> Node,
    ) -> Option<NodeId<S>> {
        let id: Id = id.into();
        let idx = match id.0 {
            Pattern::Real(idx) => idx,
            Pattern::Pat(_) => return None,
        };
        let node = self.nodes.remove(idx);
        let t = T::from_node(node).unwrap();
        let new_node = f(t);
        let id = if new_node.kind() == S::kind() {
            assert_eq!(S::kind(), new_node.kind());
            Some(id.typed())
        } else {
            None
        };
        self.nodes.insert(idx, new_node);
        id
    }

    pub fn get_var(&self, id: Id) -> &Var<Node> {
        match id.0 {
            InternalId::Real(_) => panic!(),
            InternalId::Pat(idx) => &self.vars[idx],
        }
    }

    pub fn get_kind(&self, id: Id) -> Node::Kind {
        match id.0 {
            InternalId::Real(idx) => self.nodes[idx].kind(),
            InternalId::Pat(idx) => self.vars[idx].kind,
        }
    }

    fn get_var_by_name(&self, name: &str) -> (Id, &Var<Node>) {
        self.vars
            .iter()
            .enumerate()
            .find(|(_, var)| var.name == name)
            .map(|(i, var)| (Id(InternalId::Pat(i)), var))
            // Var resolution should take care of
            // any undefined variables in any molt
            // expression, so we will always find
            // a matching variable for any identifier
            // we encounter.
            .unwrap()
    }

    pub fn get_kind_by_name(&self, name: &str) -> Node::Kind {
        self.get_var_by_name(name).1.kind
    }

    pub fn get_id_by_name(&self, name: &str) -> Id {
        self.get_var_by_name(name).0
    }

    pub fn iter(&self) -> impl Iterator<Item = Id> {
        (0..self.nodes.len()).map(|id| Id(InternalId::Real(id)))
    }

    pub(crate) fn iter_vars(&self) -> impl Iterator<Item = &Var<Node>> {
        self.vars.iter()
    }

    pub fn get_span(&self, id: impl Into<Id>) -> Span {
        match id.into().0 {
            Pattern::Real(idx) => self.spans[idx],
            Pattern::Pat(_) => panic!(),
        }
    }

    pub fn print<'a>(&'a self, id: Id, src: &'a str) -> &'a str {
        let span = self.get_span(id);
        &src[span.byte_range()]
    }
}

impl<Node: GetKind> Default for Ctx<Node> {
    fn default() -> Self {
        Self {
            nodes: vec![],
            vars: vec![],
            spans: vec![],
        }
    }
}
