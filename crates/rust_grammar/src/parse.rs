#[path = "discouraged.rs"]
pub mod discouraged;

use std::cell::{Cell, Ref, RefCell, RefMut};
use std::fmt::{self, Debug, Display};
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::rc::Rc;
use std::str::FromStr;

use discouraged::Speculative;
use molt_lib::{
    Ctx, GetKind, Id, List, ListMatchingMode, NodeId, NodeList, ParsingMode, PatNodeList, Pattern,
    RealNodeList, Single, SingleMatchingMode, Spanned, SpannedPat, ToNode, Var, WithSpan,
};
use proc_macro2::{Delimiter, Group, Literal, Punct, Span, TokenStream, TokenTree};

use crate::buffer::{Cursor, TokenBuffer};
use crate::ext::IdentExt;
use crate::node::Node;
use crate::punctuated::Punctuated;
use crate::token::{Bracket, Paren, Token};
use crate::{Ident, Kind, error, lookahead};

pub type ParseCtx = Rc<RefCell<Ctx<Node>>>;

pub use crate::error::{Error, Result};
pub use crate::lookahead::{Lookahead1, Peek};

/// Parsing interface implemented by types that can be parsed in a default
/// way from a token stream. This trait is for types that aren't represented
/// as nodes but stored within the AST structs directly.
pub trait Parse: Sized {
    fn parse(input: ParseStream) -> Result<Self>;
}

/// Parsing interface implemented by types that can be parsed in a default
/// way from a token stream. This trait is for types that are represented
/// as nodes.
pub trait ParseNode {
    type Target: ToNode<Node>;

    /// Parse a Self::Target given the input. Most types implementing
    /// this trait only need to implement this method and can defer
    /// parsing molt variables do the default implementation of
    /// `parse_pat`.  However, some types (recursive types like
    /// expressions and patterns) may need to override the
    /// implementation of the `parse_pat` implementation and check
    /// against molt variables manually.
    fn parse_node(input: ParseStream) -> Result<Self::Target>;

    /// Parse a pattern (either a concrete syntax element or a
    /// molt variable).
    fn parse_pat(input: ParseStream) -> Result<Pattern<Self::Target, Id>> {
        if let Some(var) = input.parse_var() {
            return var;
        }
        Ok(Pattern::Real(Self::parse_node(input)?))
    }
}

/// Similar to `Peek`, this trait allows peeking at a token of
/// a given type. The difference between the trait is that this
/// trait and the corresponding `ParseStream::peek_pat` method
/// also check for molt variables of the `Kind` corresponding to the
/// associated `Target` type.
pub trait PeekPat {
    type Target: ToNode<Node>;

    fn peek(cursor: Cursor) -> bool;
}

pub(crate) fn peek_pat<T: PeekPat>(cursor: Cursor, ctx: &Ctx<Node>) -> bool {
    T::peek(cursor) || peek_var(cursor, ctx, T::Target::kind())
}

pub trait ParseList {
    type Item: ToNode<Node>;
    type ParseItem: ParseNode<Target = Self::Item>;
    type Punct: Parse;

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Self::Item>>>;
}

pub(crate) fn parse_punctuated_list_real<T: ParseNode, P: Parse>(
    input: ParseStream,
) -> Result<Vec<NodeId<T::Target>>> {
    Ok(
        Punctuated::<NodeId<T::Target>, P>::parse_terminated_with(input, |input| {
            input.parse_id::<T>()
        })?
        .into_iter()
        .collect(),
    )
}

pub enum ListOrItem<T, P> {
    Item(SpannedPat<T>),
    List(NodeList<T, P>),
}

/// Parsing trait to parse something that may be either a single item
/// or a list of items. (Example: parsing parenthesized expressions
/// (1) and tuples (1, 2, 3)).  Along with the corresponding
/// `ParseStream::parse_list_or_item` method, this trait takes care of
/// proper handling of molt variables while parsing these types of
/// structures.
pub trait ParseListOrItem {
    type Target: ToNode<Node>;
    type Punct;

    fn parse_list_or_item(input: ParseStream) -> Result<ListOrItem<Self::Target, Self::Punct>>;
}

fn parse_single<T: ParseListOrItem>(input: ParseStream) -> Result<NodeId<T::Target>> {
    let fork = input.fork();
    let list_or_item = T::parse_list_or_item(input)?;
    match list_or_item {
        ListOrItem::Item(item) => Ok(input.add_pat(item)),
        ListOrItem::List(_) => Err(fork.error("Expected a single item, found a list.")),
    }
}

fn parse_list<T: ParseListOrItem>(input: ParseStream) -> Result<Vec<NodeId<T::Target>>> {
    let list_or_item = T::parse_list_or_item(input)?;
    Ok(match list_or_item {
        ListOrItem::Item(item) => {
            vec![input.add_pat(item)]
        }
        ListOrItem::List(list) => list.unwrap_real().into(),
    })
}

fn peek_var(cursor: Cursor, ctx: &Ctx<Node>, kind: Kind) -> bool {
    if let Some((punct, _)) = cursor.punct() {
        if punct.as_char() == '$' {
            if let Some((ident, _)) = cursor.skip().and_then(|cursor| cursor.ident()) {
                return kind_matches(ctx, &ident, kind);
            }
        }
    }
    false
}

impl<T: ToNode<Node> + ParseNode<Target = T>> Parse for NodeId<T> {
    fn parse(input: ParseStream) -> Result<NodeId<T>> {
        input.parse_id::<T>()
    }
}

impl Parse for SingleMatchingMode {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![?]) {
            let _: Token![?] = input.parse()?;
            Ok(SingleMatchingMode::Any)
        } else if lookahead.peek(Token![*]) {
            let _: Token![*] = input.parse()?;
            Ok(SingleMatchingMode::All)
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for ListMatchingMode {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![!]) {
            let _: Token![!] = input.parse()?;
            Ok(ListMatchingMode::Exact)
        } else if lookahead.peek(Token![?]) {
            let _: Token![?] = input.parse()?;
            Ok(ListMatchingMode::ContainsAll)
        } else {
            Err(lookahead.error())
        }
    }
}

/// Input to a Syn parser function.
///
/// See the methods of this type under the documentation of [`ParseBuffer`]. For
/// an overview of parsing in Syn, refer to the [module documentation].
///
/// [module documentation]: self
pub type ParseStream<'a> = &'a ParseBuffer<'a>;

/// Cursor position within a buffered token stream.
///
/// This type is more commonly used through the type alias [`ParseStream`] which
/// is an alias for `&ParseBuffer`.
///
/// `ParseStream` is the input type for all parser functions in Syn. They have
/// the signature `fn(ParseStream) -> Result<T>`.
///
/// ## Calling a parser function
///
/// There is no public way to construct a `ParseBuffer`. Instead, if you are
/// looking to invoke a parser function that requires `ParseStream` as input,
/// you will need to go through one of the public parsing entry points.
///
/// - The [`parse_macro_input!`] macro if parsing input of a procedural macro;
/// - One of [the `syn::parse*` functions][syn-parse]; or
/// - A method of the [`Parser`] trait.
///
/// [`parse_macro_input!`]: crate::parse_macro_input!
/// [syn-parse]: self#the-synparse-functions
pub struct ParseBuffer<'a> {
    scope: Span,
    // Instead of Cell<Cursor<'a>> so that ParseBuffer<'a> is covariant in 'a.
    // The rest of the code in this module needs to be careful that only a
    // cursor derived from this `cell` is ever assigned to this `cell`.
    //
    // Cell<Cursor<'a>> cannot be covariant in 'a because then we could take a
    // ParseBuffer<'a>, upcast to ParseBuffer<'short> for some lifetime shorter
    // than 'a, and then assign a Cursor<'short> into the Cell.
    //
    // By extension, it would not be safe to expose an API that accepts a
    // Cursor<'a> and trusts that it lives as long as the cursor currently in
    // the cell.
    cell: Cell<Cursor<'static>>,
    marker: PhantomData<Cursor<'a>>,
    unexpected: Cell<Option<Rc<Cell<Unexpected>>>>,
    ctx: ParseCtx,
    mode: ParsingMode,
}

impl<'a> Drop for ParseBuffer<'a> {
    fn drop(&mut self) {
        if let Some((unexpected_span, delimiter)) = span_of_unexpected_ignoring_nones(self.cursor())
        {
            let (inner, old_span) = inner_unexpected(self);
            if old_span.is_none() {
                inner.set(Unexpected::Some(unexpected_span, delimiter));
            }
        }
    }
}

impl<'a> Display for ParseBuffer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.cursor().token_stream(), f)
    }
}

impl<'a> Debug for ParseBuffer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.cursor().token_stream(), f)
    }
}

impl<'a> UnwindSafe for ParseBuffer<'a> {}
impl<'a> RefUnwindSafe for ParseBuffer<'a> {}

/// Cursor state associated with speculative parsing.
///
/// This type is the input of the closure provided to [`ParseStream::step`].
pub struct StepCursor<'c, 'a> {
    scope: Span,
    // This field is covariant in 'c.
    cursor: Cursor<'c>,
    // This field is contravariant in 'c. Together these make StepCursor
    // invariant in 'c. Also covariant in 'a. The user cannot cast 'c to a
    // different lifetime but can upcast into a StepCursor with a shorter
    // lifetime 'a.
    //
    // As long as we only ever construct a StepCursor for which 'c outlives 'a,
    // this means if ever a StepCursor<'c, 'a> exists we are guaranteed that 'c
    // outlives 'a.
    marker: PhantomData<fn(Cursor<'c>) -> Cursor<'a>>,
    pub ctx: ParseCtx,
}

impl<'c, 'a> Deref for StepCursor<'c, 'a> {
    type Target = Cursor<'c>;

    fn deref(&self) -> &Self::Target {
        &self.cursor
    }
}

impl<'c, 'a> Clone for StepCursor<'c, 'a> {
    fn clone(&self) -> Self {
        Self {
            scope: self.scope,
            cursor: self.cursor,
            marker: self.marker,
            ctx: self.ctx.clone(),
        }
    }
}

impl<'c, 'a> StepCursor<'c, 'a> {
    /// Triggers an error at the current position of the parse stream.
    ///
    /// The `ParseStream::step` invocation will return this same error without
    /// advancing the stream state.
    pub fn error<T: Display>(self, message: T) -> Error {
        error::new_at(self.scope, self.cursor, message)
    }
}

pub(crate) fn advance_step_cursor<'c, 'a>(proof: StepCursor<'c, 'a>, to: Cursor<'c>) -> Cursor<'a> {
    // Refer to the comments within the StepCursor definition. We use the
    // fact that a StepCursor<'c, 'a> exists as proof that 'c outlives 'a.
    // Cursor is covariant in its lifetime parameter so we can cast a
    // Cursor<'c> to one with the shorter lifetime Cursor<'a>.
    let _ = proof;
    unsafe { mem::transmute::<Cursor<'c>, Cursor<'a>>(to) }
}

pub(crate) fn new_parse_buffer(
    scope: Span,
    cursor: Cursor,
    unexpected: Rc<Cell<Unexpected>>,
    ctx: ParseCtx,
    mode: ParsingMode,
) -> ParseBuffer {
    ParseBuffer {
        scope,
        // See comment on `cell` in the struct definition.
        cell: Cell::new(unsafe { mem::transmute::<Cursor, Cursor<'static>>(cursor) }),
        marker: PhantomData,
        unexpected: Cell::new(Some(unexpected)),
        ctx,
        mode,
    }
}

#[derive(Default)]
pub(crate) enum Unexpected {
    #[default]
    None,
    Some(Span, Delimiter),
    Chain(Rc<Cell<Unexpected>>),
}

impl Clone for Unexpected {
    fn clone(&self) -> Self {
        match self {
            Unexpected::None => Unexpected::None,
            Unexpected::Some(span, delimiter) => Unexpected::Some(*span, *delimiter),
            Unexpected::Chain(next) => Unexpected::Chain(next.clone()),
        }
    }
}

// We call this on Cell<Unexpected> and Cell<Option<T>> where temporarily
// swapping in a None is cheap.
fn cell_clone<T: Default + Clone>(cell: &Cell<T>) -> T {
    let prev = cell.take();
    let ret = prev.clone();
    cell.set(prev);
    ret
}

fn inner_unexpected(buffer: &ParseBuffer) -> (Rc<Cell<Unexpected>>, Option<(Span, Delimiter)>) {
    let mut unexpected = get_unexpected(buffer);
    loop {
        match cell_clone(&unexpected) {
            Unexpected::None => return (unexpected, None),
            Unexpected::Some(span, delimiter) => return (unexpected, Some((span, delimiter))),
            Unexpected::Chain(next) => unexpected = next,
        }
    }
}

pub(crate) fn get_unexpected(buffer: &ParseBuffer) -> Rc<Cell<Unexpected>> {
    cell_clone(&buffer.unexpected).unwrap()
}

fn span_of_unexpected_ignoring_nones(mut cursor: Cursor) -> Option<(Span, Delimiter)> {
    if cursor.eof() {
        return None;
    }
    while let Some((inner, _span, rest)) = cursor.group(Delimiter::None) {
        if let Some(unexpected) = span_of_unexpected_ignoring_nones(inner) {
            return Some(unexpected);
        }
        cursor = rest;
    }
    if cursor.eof() {
        None
    } else {
        Some((cursor.span(), cursor.scope_delimiter()))
    }
}

fn kind_matches(ctx: &Ctx<Node>, ident: &Ident, kind: crate::Kind) -> bool {
    Node::is_comparable(ctx.get_kind_by_name(&ident.to_string()), kind)
}

impl<'a> ParseBuffer<'a> {
    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    pub(crate) fn call<T>(&'a self, function: fn(ParseStream<'a>) -> Result<T>) -> Result<T> {
        function(self)
    }

    pub fn peek<T: Peek>(&self, token: T) -> bool {
        let _ = token;
        T::Token::peek(self.cursor())
    }

    pub(crate) fn peek2<T: Peek>(&self, token: T) -> bool {
        fn peek2(buffer: &ParseBuffer, peek: fn(Cursor) -> bool) -> bool {
            buffer.cursor().skip().is_some_and(peek)
        }

        let _ = token;
        peek2(self, T::Token::peek)
    }

    pub(crate) fn peek3<T: Peek>(&self, token: T) -> bool {
        fn peek3(buffer: &ParseBuffer, peek: fn(Cursor) -> bool) -> bool {
            buffer
                .cursor()
                .skip()
                .and_then(Cursor::skip)
                .is_some_and(peek)
        }

        let _ = token;
        peek3(self, T::Token::peek)
    }

    pub(crate) fn parse_terminated<T, P>(
        &'a self,
        parser: fn(ParseStream<'a>) -> Result<T>,
        separator: P,
    ) -> Result<Punctuated<T, P::Token>>
    where
        P: Peek,
        P::Token: Parse,
    {
        let _ = separator;
        Punctuated::parse_terminated_with(self, parser)
    }

    pub fn is_empty(&self) -> bool {
        self.cursor().eof()
    }

    pub fn lookahead1(&self) -> Lookahead1<'a> {
        lookahead::new(self.scope, self.cursor(), self.ctx.clone())
    }

    pub fn fork(&self) -> Self {
        ParseBuffer {
            scope: self.scope,
            cell: self.cell.clone(),
            marker: PhantomData,
            // Not the parent's unexpected. Nothing cares whether the clone
            // parses all the way unless we `advance_to`.
            unexpected: Cell::new(Some(Rc::new(Cell::new(Unexpected::None)))),
            ctx: self.ctx.clone(),
            mode: self.mode,
        }
    }

    pub fn error<T: Display>(&self, message: T) -> Error {
        error::new_at(self.scope, self.cursor(), message)
    }

    pub fn step<F, R>(&self, function: F) -> Result<R>
    where
        F: for<'c> FnOnce(StepCursor<'c, 'a>) -> Result<(R, Cursor<'c>)>,
    {
        // Since the user's function is required to work for any 'c, we know
        // that the Cursor<'c> they return is either derived from the input
        // StepCursor<'c, 'a> or from a Cursor<'static>.
        //
        // It would not be legal to write this function without the invariant
        // lifetime 'c in StepCursor<'c, 'a>. If this function were written only
        // in terms of 'a, the user could take our ParseBuffer<'a>, upcast it to
        // a ParseBuffer<'short> which some shorter lifetime than 'a, invoke
        // `step` on their ParseBuffer<'short> with a closure that returns
        // Cursor<'short>, and we would wrongly write that Cursor<'short> into
        // the Cell intended to hold Cursor<'a>.
        //
        // In some cases it may be necessary for R to contain a Cursor<'a>.
        // Within Syn we solve this using `advance_step_cursor` which uses the
        // existence of a StepCursor<'c, 'a> as proof that it is safe to cast
        // from Cursor<'c> to Cursor<'a>. If needed outside of Syn, it would be
        // safe to expose that API as a method on StepCursor.
        let (node, rest) = function(StepCursor {
            scope: self.scope,
            cursor: self.cell.get(),
            marker: PhantomData,
            ctx: self.ctx.clone(),
        })?;
        self.cell.set(rest);
        Ok(node)
    }

    pub(crate) fn span(&self) -> Span {
        let cursor = self.cursor();
        if cursor.eof() {
            self.scope
        } else {
            crate::buffer::open_span_of_group(cursor)
        }
    }

    pub fn cursor(&self) -> Cursor<'a> {
        self.cell.get()
    }

    fn check_unexpected(&self) -> Result<()> {
        match inner_unexpected(self).1 {
            Some((span, delimiter)) => Err(err_unexpected_token(span, delimiter)),
            None => Ok(()),
        }
    }

    #[allow(unused)]
    fn dbg<T: Debug + ToNode<Node>>(&self, t: NodeId<T>) {
        println!("{:?}", self.ctx.borrow().get::<T>(t));
    }

    #[allow(unused)]
    fn show(&self) {
        println!("{}", self.cursor().token_stream());
    }

    pub(crate) fn ctx(&self) -> Ref<'_, Ctx<Node>> {
        self.ctx.borrow()
    }

    pub(crate) fn ctx_mut(&self) -> RefMut<'_, Ctx<Node>> {
        self.ctx.borrow_mut()
    }

    pub fn parse_id<T: ParseNode>(&self) -> Result<NodeId<T::Target>> {
        let pat = self.parse_spanned_pat::<T>()?;
        Ok(self.add_pat(pat))
    }

    pub fn parse_node<T: ParseNode>(&self) -> Result<T::Target> {
        T::parse_node(self)
    }

    pub(crate) fn parse_spanned<T: Parse>(&self) -> Result<Spanned<T>> {
        self.call_spanned(T::parse)
    }

    pub(crate) fn parse_spanned_pat<T: ParseNode + ?Sized>(&self) -> Result<SpannedPat<T::Target>> {
        self.call_spanned(T::parse_pat)
    }

    pub(crate) fn parse_span_with<T: Parse, S: ToNode<Node>>(
        &self,
        f: impl Fn(T) -> S,
    ) -> Result<Spanned<Pattern<S, Id>>> {
        let item: Spanned<T> = self.parse_spanned()?;
        Ok(item.map(f).as_pattern())
    }

    pub fn call_spanned<T>(
        &self,
        f: impl for<'b> Fn(&'b ParseBuffer<'b>) -> Result<T>,
    ) -> Result<Spanned<T>> {
        let marker = self.marker();
        let t = f(self)?;
        Ok(self.make_spanned(marker, t))
    }

    fn add_var<T: ToNode<Node>>(&self, var: Var<<Node as GetKind>::Kind>) -> NodeId<T> {
        self.ctx.borrow_mut().add_var(var)
    }

    pub(crate) fn span_from_marker(&self, marker: PosMarker) -> molt_lib::Span {
        let end = self.cursor().prev_span().byte_range().end;
        // While parsing some items (notably visibilities),
        // we might end up parsing nothing (if no visibility
        // is given). With the naive approach, we would end up
        // returning an invalid span (with end < start), so we
        // add a special case here:
        if end < marker.start {
            molt_lib::Span::new(marker.start, marker.start)
        } else {
            molt_lib::Span::new(marker.start, end)
        }
    }

    pub(crate) fn marker(&self) -> PosMarker {
        let start = self.cursor().span().byte_range().start;
        PosMarker { start }
    }

    pub(crate) fn make_spanned<T>(&self, marker: PosMarker, t: T) -> Spanned<T> {
        t.with_span(self.span_from_marker(marker))
    }

    pub(crate) fn add<T: ToNode<Node>>(&self, t: Spanned<T>) -> NodeId<T> {
        self.ctx.borrow_mut().add(t)
    }

    pub(crate) fn add_pat<T: ToNode<Node>>(&self, item: SpannedPat<T>) -> NodeId<T> {
        self.ctx.borrow_mut().add_pat(item)
    }

    pub(crate) fn peek_var<T: ToNode<Node>>(&self) -> bool {
        peek_var(self.cursor(), &self.ctx.borrow(), T::kind())
    }

    pub(crate) fn peek_pat<T: PeekPat>(&self) -> bool {
        peek_pat::<T>(self.cursor(), &self.ctx.borrow())
    }

    pub(crate) fn parse_var<T: ToNode<Node>>(&self) -> Option<Result<Pattern<T, Id>>> {
        let transposed = || -> Result<Option<Pattern<T, Id>>> {
            let ahead = self.fork();
            if ahead.peek(Token![$]) {
                let _: Token![$] = ahead.parse()?;
                let ident: Ident = Ident::parse_any(&ahead)?;
                if !kind_matches(&ahead.ctx.borrow(), &ident, T::kind()) {
                    return Ok(None);
                }
                self.advance_to(&ahead);
                let id = self
                    .add_var::<T>(Var::new(ident.to_string(), T::kind()))
                    .into();
                Ok(Some(Pattern::Pat(id)))
            } else {
                Ok(None)
            }
        };
        transposed().transpose()
    }

    fn parse_list_var<T, P>(
        &self,
        parse_list: impl for<'b> FnOnce(&'b ParseBuffer<'b>) -> Result<Vec<NodeId<T>>>,
        parse_single: impl for<'b> FnOnce(&'b ParseBuffer<'b>) -> Result<NodeId<T>>,
    ) -> Result<PatNodeList<T, P>> {
        let _: Token![$] = self.parse()?;
        let lookahead = self.lookahead1();
        if lookahead.peek(Ident::peek_any) {
            // No need to check for the kind here, since that is
            // already done in peek_list_var
            todo!("Implement list kinds")
        } else if lookahead.peek(Paren) {
            let content;
            let _ = parenthesized!(content in self);
            let item = parse_single(&content)?;
            let mode = self.parse()?;
            Ok(PatNodeList::Single(Single::new(item, mode)))
        } else if lookahead.peek(Bracket) {
            let content;
            let _ = bracketed!(content in self);
            let items = parse_list(&content)?;
            let mode = self.parse()?;
            Ok(PatNodeList::List(List::new(items, mode)))
        } else {
            Err(lookahead.error())
        }
    }

    fn peek_list_var(&self, _: Kind) -> bool {
        if self.peek(Token![$]) {
            if self.peek2(Ident::peek_any) {
                false
                // todo!("Implement list kinds")
                // return kind_matches(&ahead.ctx.borrow(), &ident, T::kind())
            } else {
                true
            }
        } else {
            false
        }
    }

    pub(crate) fn parse_list<T: ParseList>(&self) -> Result<NodeList<T::Item, T::Punct>> {
        if self.peek_list_var(T::Item::kind()) {
            Ok(NodeList::Pat(self.parse_list_var::<T::Item, T::Punct>(
                T::parse_list_real,
                |input| input.parse_id::<T::ParseItem>(),
            )?))
        } else {
            let list = T::parse_list_real(self)?;
            Ok(NodeList::Real(RealNodeList::new(list)))
        }
    }

    pub(crate) fn parse_list_or_item<T: ParseListOrItem>(
        &self,
    ) -> Result<ListOrItem<T::Target, T::Punct>> {
        if self.peek_list_var(<T as ParseListOrItem>::Target::kind()) {
            Ok(ListOrItem::List(NodeList::Pat(
                self.parse_list_var::<<T as ParseListOrItem>::Target, <T as ParseListOrItem>::Punct>(
                    parse_list::<T>, parse_single::<T>
                )?,
            )))
        } else {
            T::parse_list_or_item(self)
        }
    }

    pub(crate) fn mode(&self) -> ParsingMode {
        self.mode
    }
}

#[derive(Clone, Copy)]
pub struct PosMarker {
    start: usize,
}

impl<T: Parse> Parse for Box<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse().map(Box::new)
    }
}

impl<T: Parse + Token> Parse for Option<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        if T::peek(input.cursor()) {
            Ok(Some(input.parse()?))
        } else {
            Ok(None)
        }
    }
}

impl<T> Parse for Option<NodeId<T>>
where
    NodeId<T>: Parse,
    T: Token,
{
    fn parse(input: ParseStream) -> Result<Self> {
        // Also match variables here.
        if T::peek(input.cursor()) || input.peek(Token![$]) {
            Ok(Some(input.parse()?))
        } else {
            Ok(None)
        }
    }
}

impl Parse for TokenStream {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| Ok((cursor.token_stream(), Cursor::empty())))
    }
}

impl Parse for TokenTree {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.token_tree() {
            Some((tt, rest)) => Ok((tt, rest)),
            None => Err(cursor.error("expected token tree")),
        })
    }
}

impl Parse for Group {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| {
            if let Some((group, rest)) = cursor.any_group_token() {
                if group.delimiter() != Delimiter::None {
                    return Ok((group, rest));
                }
            }
            Err(cursor.error("expected group token"))
        })
    }
}

impl Parse for Punct {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.punct() {
            Some((punct, rest)) => Ok((punct, rest)),
            None => Err(cursor.error("expected punctuation token")),
        })
    }
}

impl Parse for Literal {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.literal() {
            Some((literal, rest)) => Ok((literal, rest)),
            None => Err(cursor.error("expected literal token")),
        })
    }
}

fn tokens_to_parse_buffer(ctx: ParseCtx, tokens: &TokenBuffer, mode: ParsingMode) -> ParseBuffer {
    let scope = Span::call_site();
    let cursor = tokens.begin();
    let unexpected = Rc::new(Cell::new(Unexpected::None));
    new_parse_buffer(scope, cursor, unexpected, ctx, mode)
}

fn parse2_impl<T>(
    ctx: ParseCtx,
    f: impl FnOnce(ParseStream) -> Result<T>,
    tokens: TokenStream,
    mode: ParsingMode,
) -> Result<T> {
    let buf = TokenBuffer::new2(tokens);
    let state = tokens_to_parse_buffer(ctx.clone(), &buf, mode);
    let node = f(&state)?;
    state.check_unexpected()?;
    if let Some((unexpected_span, delimiter)) = span_of_unexpected_ignoring_nones(state.cursor()) {
        Err(err_unexpected_token(unexpected_span, delimiter))
    } else {
        Ok(node)
    }
}

pub fn parse_str<T: Parse>(s: &str, mode: ParsingMode) -> Result<T> {
    let ctx = ParseCtx::default();
    parse2_impl(ctx, T::parse, proc_macro2::TokenStream::from_str(s)?, mode)
}

pub(crate) fn parse_str_ctx<T: Parse>(s: &str, mode: ParsingMode) -> Result<(T, Ctx<Node>)> {
    let ctx = ParseCtx::default();
    parse2_impl(
        ctx.clone(),
        T::parse,
        proc_macro2::TokenStream::from_str(s)?,
        mode,
    )
    .map(|t| (t, ctx.take()))
}

pub fn parse_ctx<T>(
    f: impl FnOnce(ParseStream) -> Result<T>,
    s: &str,
    mode: ParsingMode,
) -> Result<(T, Ctx<Node>)> {
    let ctx = ParseCtx::default();
    parse2_impl(ctx.clone(), f, proc_macro2::TokenStream::from_str(s)?, mode)
        .map(|t| (t, ctx.take()))
}

pub fn parse_with_ctx<T>(
    ctx: ParseCtx,
    f: impl FnOnce(ParseStream) -> Result<T>,
    tokens: TokenStream,
    mode: ParsingMode,
) -> Result<T> {
    parse2_impl(ctx.clone(), f, tokens, mode)
}

fn err_unexpected_token(span: Span, delimiter: Delimiter) -> Error {
    let msg = match delimiter {
        Delimiter::Parenthesis => "unexpected token, expected `)`",
        Delimiter::Brace => "unexpected token, expected `}`",
        Delimiter::Bracket => "unexpected token, expected `]`",
        Delimiter::None => "unexpected token",
    };
    Error::new(span, msg)
}
