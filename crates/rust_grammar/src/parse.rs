//! Parsing interface for parsing a token stream into a syntax tree node.
//!
//! Parsing in Syn is built on parser functions that take in a [`ParseStream`]
//! and produce a [`Result<T>`] where `T` is some syntax tree node. Underlying
//! these parser functions is a lower level mechanism built around the
//! [`Cursor`] type. `Cursor` is a cheaply copyable cursor over a range of
//! tokens in a token stream.
//!
//! [`Result<T>`]: Result
//! [`Cursor`]: crate::buffer::Cursor
//!
//! # Example
//!
//! Here is a snippet of parsing code to get a feel for the style of the
//! library. We define data structures for a subset of Rust syntax including
//! enums (not shown) and structs, then provide implementations of the [`Parse`]
//! trait to parse these syntax tree data structures from a token stream.
//!
//! Once `Parse` impls have been defined, they can be called conveniently from a
//! procedural macro through [`parse_macro_input!`] as shown at the bottom of
//! the snippet. If the caller provides syntactically invalid input to the
//! procedural macro, they will receive a helpful compiler error message
//! pointing out the exact token that triggered the failure to parse.
//!
//! [`parse_macro_input!`]: crate::parse_macro_input!
//!
//! ```
//! # extern crate proc_macro;
//! #
//! use proc_macro::TokenStream;
//! use syn::{braced, parse_macro_input, token, Field, Ident, Result, Token};
//! use syn::parse::{Parse, ParseStream};
//! use syn::punctuated::Punctuated;
//!
//! enum Item {
//!     Struct(ItemStruct),
//!     Enum(ItemEnum),
//! }
//!
//! struct ItemStruct {
//!     struct_token: Token![struct],
//!     ident: Ident,
//!     brace_token: token::Brace,
//!     fields: Punctuated<Field, Token![,]>,
//! }
//! #
//! # enum ItemEnum {}
//!
//! impl Parse for Item {
//!     fn parse(input: ParseStream) -> Result<Self> {
//!         let lookahead = input.lookahead1();
//!         if lookahead.peek(Token![struct]) {
//!             input.parse().map(Item::Struct)
//!         } else if lookahead.peek(Token![enum]) {
//!             input.parse().map(Item::Enum)
//!         } else {
//!             Err(lookahead.error())
//!         }
//!     }
//! }
//!
//! impl Parse for ItemStruct {
//!     fn parse(input: ParseStream) -> Result<Self> {
//!         let content;
//!         Ok(ItemStruct {
//!             struct_token: input.parse()?,
//!             ident: input.parse()?,
//!             brace_token: braced!(content in input),
//!             fields: content.parse_terminated(Field::parse_named, Token![,])?,
//!         })
//!     }
//! }
//! #
//! # impl Parse for ItemEnum {
//! #     fn parse(input: ParseStream) -> Result<Self> {
//! #         unimplemented!()
//! #     }
//! # }
//!
//! # const IGNORE: &str = stringify! {
//! #[proc_macro]
//! # };
//! pub fn my_macro(tokens: TokenStream) -> TokenStream {
//!     let input = parse_macro_input!(tokens as Item);
//!
//!     /* ... */
//! #   TokenStream::new()
//! }
//! ```
//!
//! # The `syn::parse*` functions
//!
//! The [`syn::parse`], [`syn::parse2`], and [`syn::parse_str`] functions serve
//! as an entry point for parsing syntax tree nodes that can be parsed in an
//! obvious default way. These functions can return any syntax tree node that
//! implements the [`Parse`] trait, which includes most types in Syn.
//!
//! [`syn::parse`]: crate::parse()
//! [`syn::parse2`]: crate::parse2()
//! [`syn::parse_str`]: crate::parse_str()
//!
//! ```
//! use syn::Type;
//!
//! # fn run_parser() -> syn::Result<()> {
//! let t: Type = syn::parse_str("std::collections::HashMap<String, Value>")?;
//! #     Ok(())
//! # }
//! #
//! # run_parser().unwrap();
//! ```
//!
//! The [`parse_quote!`] macro also uses this approach.
//!
//! [`parse_quote!`]: crate::parse_quote!
//!
//! # The `Parser` trait
//!
//! Some types can be parsed in several ways depending on context. For example
//! an [`Attribute`] can be either "outer" like `#[...]` or "inner" like
//! `#![...]` and parsing the wrong one would be a bug. Similarly [`Punctuated`]
//! may or may not allow trailing punctuation, and parsing it the wrong way
//! would either reject valid input or accept invalid input.
//!
//! [`Attribute`]: crate::Attribute
//! [`Punctuated`]: crate::punctuated
//!
//! The `Parse` trait is not implemented in these cases because there is no good
//! behavior to consider the default.
//!
//! ```compile_fail
//! # extern crate proc_macro;
//! #
//! # use syn::punctuated::Punctuated;
//! # use syn::{PathSegment, Result, Token};
//! #
//! # fn f(tokens: proc_macro::TokenStream) -> Result<()> {
//! #
//! // Can't parse `Punctuated` without knowing whether trailing punctuation
//! // should be allowed in this context.
//! let path: Punctuated<PathSegment, Token![::]> = syn::parse(tokens)?;
//! #
//! #     Ok(())
//! # }
//! ```
//!
//! In these cases the types provide a choice of parser functions rather than a
//! single `Parse` implementation, and those parser functions can be invoked
//! through the [`Parser`] trait.
//!
//!
//! ```
//! # extern crate proc_macro;
//! #
//! use proc_macro::TokenStream;
//! use syn::parse::Parser;
//! use syn::punctuated::Punctuated;
//! use syn::{Attribute, Expr, PathSegment, Result, Token};
//!
//! fn call_some_parser_methods(input: TokenStream) -> Result<()> {
//!     // Parse a nonempty sequence of path segments separated by `::` punctuation
//!     // with no trailing punctuation.
//!     let tokens = input.clone();
//!     let parser = Punctuated::<PathSegment, Token![::]>::parse_separated_nonempty;
//!     let _path = parser.parse(tokens)?;
//!
//!     // Parse a possibly empty sequence of expressions terminated by commas with
//!     // an optional trailing punctuation.
//!     let tokens = input.clone();
//!     let parser = Punctuated::<Expr, Token![,]>::parse_terminated;
//!     let _args = parser.parse(tokens)?;
//!
//!     // Parse zero or more outer attributes but not inner attributes.
//!     let tokens = input.clone();
//!     let parser = Attribute::parse_outer;
//!     let _attrs = parser.parse(tokens)?;
//!
//!     Ok(())
//! }
//! ```

#[path = "discouraged.rs"]
pub mod discouraged;

use discouraged::Speculative;
use molt_lib::{
    Ctx, GetKind, Id, NodeId, NodeList, ParsingMode, PatNodeList, Pattern, RealNodeList, Spanned,
    SpannedPat, ToNode, Var, WithSpan,
};

use crate::buffer::{Cursor, TokenBuffer};
use crate::node::Node;
use crate::punctuated::Punctuated;
use crate::token::Token;
use crate::{error, Ident};
use crate::{lookahead, Kind};
use proc_macro2::{Delimiter, Group, Literal, Punct, Span, TokenStream, TokenTree};
#[cfg(feature = "printing")]
use quote::ToTokens;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::fmt::{self, Debug, Display};
#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::rc::Rc;
use std::str::FromStr;

pub type ParseCtx = Rc<RefCell<Ctx<Node>>>;

pub use crate::error::{Error, Result};
pub use crate::lookahead::{End, Lookahead1, Peek};

/// Parsing interface implemented by all types that can be parsed in a default
/// way from a token stream.
///
/// Refer to the [module documentation] for details about implementing and using
/// the `Parse` trait.
///
/// [module documentation]: self
pub trait Parse: Sized {
    fn parse(input: ParseStream) -> Result<Self>;
}

pub trait ParsePat {
    type Target: ToNode<Node>;

    fn parse_pat(input: ParseStream) -> Result<SpannedPat<Self::Target>>;

    fn parse_id(input: ParseStream) -> Result<NodeId<Self::Target>> {
        let p = Self::parse_pat(input)?;
        Ok(input.add_pat(p))
    }
}

pub trait PeekPat {
    type Target: ToNode<Node>;
    const KIND: Kind;

    fn peek(cursor: Cursor) -> bool;

    fn peek_pat(cursor: Cursor, ctx: &Ctx<Node>) -> bool {
        Self::peek(cursor) || peek_var(cursor, ctx, <Self as PeekPat>::KIND)
    }
}

pub trait ParseList {
    type Target: ToNode<Node>;
    type Punct: Parse;

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Self::Target>>>;
}

pub fn parse_list_real_normal<T: ParsePat, P: Parse>(
    input: ParseStream,
) -> Result<Vec<NodeId<<T as ParsePat>::Target>>> {
    Ok(
        Punctuated::<NodeId<T::Target>, P>::parse_terminated_with(input, T::parse_id)?
            .into_iter()
            .collect(),
    )
}

pub enum ListOrItem<T, P> {
    Item(SpannedPat<T>),
    List(NodeList<T, P>),
}

pub trait ParseListOrItem {
    type Target: ToNode<Node>;
    type Punct;

    fn parse_list_or_item(input: ParseStream) -> Result<ListOrItem<Self::Target, Self::Punct>>;
}

pub fn peek_var(cursor: Cursor, ctx: &Ctx<Node>, kind: Kind) -> bool {
    if let Some((punct, _)) = cursor.punct() {
        if punct.as_char() == '$' {
            if let Some((ident, _)) = cursor.skip().and_then(|cursor| cursor.ident()) {
                return kind_matches(&ctx, &ident, kind);
            }
        }
    }
    false
}

impl<T: ToNode<Node> + ParsePat<Target = T>> Parse for NodeId<T> {
    fn parse(input: ParseStream) -> Result<NodeId<T>> {
        T::parse_id(input)
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
///
/// [`ParseStream::step`]: ParseBuffer::step
///
/// # Example
///
/// ```
/// use proc_macro2::TokenTree;
/// use syn::Result;
/// use syn::parse::ParseStream;
///
/// // This function advances the stream past the next occurrence of `@`. If
/// // no `@` is present in the stream, the stream position is unchanged and
/// // an error is returned.
/// fn skip_past_next_at(input: ParseStream) -> Result<()> {
///     input.step(|cursor| {
///         let mut rest = *cursor;
///         while let Some((tt, next)) = rest.token_tree() {
///             match &tt {
///                 TokenTree::Punct(punct) if punct.as_char() == '@' => {
///                     return Ok(((), next));
///                 }
///                 _ => rest = next,
///             }
///         }
///         Err(cursor.error("no `@` was found after this point"))
///     })
/// }
/// #
/// # fn remainder_after_skipping_past_next_at(
/// #     input: ParseStream,
/// # ) -> Result<proc_macro2::TokenStream> {
/// #     skip_past_next_at(input)?;
/// #     input.parse()
/// # }
/// #
/// # use syn::parse::Parser;
/// # let remainder = remainder_after_skipping_past_next_at
/// #     .parse_str("a @ b c")
/// #     .unwrap();
/// # assert_eq!(remainder.to_string(), "b c");
/// ```
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
            scope: self.scope.clone(),
            cursor: self.cursor.clone(),
            marker: self.marker.clone(),
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

pub(crate) enum Unexpected {
    None,
    Some(Span, Delimiter),
    Chain(Rc<Cell<Unexpected>>),
}

impl Default for Unexpected {
    fn default() -> Self {
        Unexpected::None
    }
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

pub fn kind_matches(ctx: &Ctx<Node>, ident: &Ident, kind: crate::Kind) -> bool {
    ctx.get_kind_by_name(&ident.to_string()) == kind
}

impl<'a> ParseBuffer<'a> {
    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    pub fn call<T>(&'a self, function: fn(ParseStream<'a>) -> Result<T>) -> Result<T> {
        function(self)
    }

    pub fn peek<T: Peek>(&self, token: T) -> bool {
        let _ = token;
        T::Token::peek(self.cursor())
    }

    pub fn peek2<T: Peek>(&self, token: T) -> bool {
        fn peek2(buffer: &ParseBuffer, peek: fn(Cursor) -> bool) -> bool {
            buffer.cursor().skip().map_or(false, peek)
        }

        let _ = token;
        peek2(self, T::Token::peek)
    }

    pub fn peek3<T: Peek>(&self, token: T) -> bool {
        fn peek3(buffer: &ParseBuffer, peek: fn(Cursor) -> bool) -> bool {
            buffer
                .cursor()
                .skip()
                .and_then(Cursor::skip)
                .map_or(false, peek)
        }

        let _ = token;
        peek3(self, T::Token::peek)
    }

    pub fn parse_terminated<T, P>(
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

    pub fn span(&self) -> Span {
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

    pub fn span_from_marker(&self, marker: PosMarker) -> molt_lib::Span {
        let end = self.cursor().prev_span().byte_range().end;
        molt_lib::Span::new(marker.start, end)
    }

    pub fn marker(&self) -> PosMarker {
        let start = self.cursor().span().byte_range().start;
        PosMarker { start }
    }

    #[allow(unused)]
    pub(crate) fn dbg<T: Debug + ToNode<Node>>(&self, t: NodeId<T>) {
        println!("{:?}", self.ctx.borrow().get::<T>(t));
    }

    pub fn ctx(&self) -> Ref<'_, Ctx<Node>> {
        self.ctx.borrow()
    }

    pub fn ctx_mut(&self) -> RefMut<'_, Ctx<Node>> {
        self.ctx.borrow_mut()
    }

    pub fn parse_pat<T: ParsePat>(&self) -> Result<SpannedPat<T::Target>> {
        T::parse_pat(self)
    }

    pub fn parse_id<T: ParsePat>(&self) -> Result<NodeId<T::Target>> {
        T::parse_id(self)
    }

    pub fn parse_spanned<T: Parse>(&self) -> Result<Spanned<T>> {
        let marker = self.marker();
        let item = T::parse(self)?;
        let span = self.span_from_marker(marker);
        Ok(item.with_span(span))
    }

    pub fn parse_span_with<'b, T: Parse, S: ToNode<Node>>(
        &self,
        f: impl Fn(T) -> S,
    ) -> Result<Spanned<Pattern<S, Id>>> {
        let item: Spanned<T> = self.parse_spanned()?;
        Ok(item.map(f).as_pattern())
    }

    pub fn call_spanned<T>(
        &self,
        f: impl for<'b> Fn(&'b ParseBuffer<'b>) -> Result<T>,
    ) -> Result<Spanned<Pattern<T, Id>>> {
        let marker = self.marker();
        let t = f(self)?;
        Ok(t.pattern_with_span(self.span_from_marker(marker)))
    }

    pub fn call_add<T: ToNode<Node>>(
        &self,
        f: impl for<'b> Fn(&'b ParseBuffer<'b>) -> Result<T>,
    ) -> Result<NodeId<T>> {
        let marker = self.marker();
        let t = f(self)?;
        Ok(self.add_pat(t.pattern_with_span(self.span_from_marker(marker))))
    }

    pub fn add_var<T: ToNode<Node>>(&self, var: Var<Node>) -> NodeId<T> {
        self.ctx.borrow_mut().add_var(var)
    }

    pub fn add_existing_var(&self, var: &str) -> Option<Id> {
        self.ctx.borrow_mut().add_existing_var(var)
    }

    pub fn from_marker<T>(&self, marker: PosMarker, t: T) -> Spanned<T> {
        t.with_span(self.span_from_marker(marker))
    }

    pub fn add<T: ToNode<Node>>(&self, t: Spanned<T>) -> NodeId<T> {
        self.ctx.borrow_mut().add(t)
    }

    pub fn peek_var<T: ToNode<Node>>(&self) -> bool {
        peek_var(self.cursor(), &self.ctx.borrow(), T::kind())
    }

    pub fn peek_pat<T: PeekPat>(&self) -> bool {
        T::peek_pat(self.cursor(), &self.ctx.borrow())
    }

    pub fn parse_var<T: ToNode<Node>>(&self) -> Option<Result<SpannedPat<T>>> {
        let transposed = || -> Result<Option<SpannedPat<T>>> {
            let ahead = self.fork();
            if ahead.peek(Token![$]) {
                let marker = ahead.marker();
                let _: Token![$] = ahead.parse()?;
                let ident: Ident = ahead.parse()?;
                if !kind_matches(&ahead.ctx.borrow(), &ident, T::kind()) {
                    return Ok(None);
                }
                self.advance_to(&ahead);
                let span = self.span_from_marker(marker);
                let id = self
                    .add_var::<T>(Var::new(ident.to_string(), T::kind()))
                    .into();
                let item: SpannedPat<T> = Pattern::Pat(id).with_span(span);
                Ok(Some(item))
            } else {
                Ok(None)
            }
        };
        transposed().transpose()
    }

    fn parse_list_var<T, P>(
        &self,
        f: impl Fn(ParseStream) -> Result<Vec<NodeId<T>>>,
    ) -> Result<PatNodeList<T, P>> {
        let _: Token![$] = self.parse()?;
        todo!()
    }

    fn peek_list_var(&self, kind: Kind) -> bool {
        if self.peek(Token![$]) {
            if self.peek2(Ident) {
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

    pub(crate) fn parse_list<T: ParseList>(&self) -> Result<NodeList<T::Target, T::Punct>> {
        if self.peek_list_var(<T as ParseList>::Target::kind()) {
            Ok(NodeList::Pat(
                self.parse_list_var::<<T as ParseList>::Target, <T as ParseList>::Punct>(
                    T::parse_list_real,
                )?,
            ))
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
                    |input| {
                        let list_or_item = T::parse_list_or_item(input)?;
                        Ok(match list_or_item {
                            ListOrItem::Item(item) => vec![input.add_pat(item)],
                            ListOrItem::List(list) => list.unwrap_real().into(),
                        })
                    }
                )?,
            )))
        } else {
            T::parse_list_or_item(self)
        }
    }

    pub(crate) fn add_pat<T: ToNode<Node>>(&self, item: SpannedPat<T>) -> NodeId<T> {
        self.ctx.borrow_mut().add_pat(item)
    }

    pub(crate) fn add_with_marker<T: ToNode<Node>>(&self, marker: PosMarker, t: T) -> NodeId<T> {
        self.add(t.with_span(self.span_from_marker(marker)))
    }

    pub(crate) fn mode(&self) -> ParsingMode {
        self.mode
    }
}

#[derive(Clone, Copy)]
pub struct PosMarker {
    start: usize,
}

#[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
impl<T: Parse> Parse for Box<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse().map(Box::new)
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
impl<T: Parse + Token> Parse for Option<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        if T::peek(input.cursor()) {
            Ok(Some(input.parse()?))
        } else {
            Ok(None)
        }
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
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

#[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
impl Parse for TokenStream {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| Ok((cursor.token_stream(), Cursor::empty())))
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
impl Parse for TokenTree {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.token_tree() {
            Some((tt, rest)) => Ok((tt, rest)),
            None => Err(cursor.error("expected token tree")),
        })
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
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

#[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
impl Parse for Punct {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.punct() {
            Some((punct, rest)) => Ok((punct, rest)),
            None => Err(cursor.error("expected punctuation token")),
        })
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
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
    let t = parse2_impl(ctx.clone(), f, tokens, mode);
    t
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

/// An empty syntax tree node that consumes no tokens when parsed.
///
/// This is useful for attribute macros that want to ensure they are not
/// provided any attribute args.
///
/// ```
/// # extern crate proc_macro;
/// #
/// use proc_macro::TokenStream;
/// use syn::parse_macro_input;
/// use syn::parse::Nothing;
///
/// # const IGNORE: &str = stringify! {
/// #[proc_macro_attribute]
/// # };
/// pub fn my_attr(args: TokenStream, input: TokenStream) -> TokenStream {
///     parse_macro_input!(args as Nothing);
///
///     /* ... */
/// #   TokenStream::new()
/// }
/// ```
///
/// ```text
/// error: unexpected token
///  --> src/main.rs:3:19
///   |
/// 3 | #[my_attr(asdf)]
///   |           ^^^^
/// ```
pub struct Nothing;

impl Parse for Nothing {
    fn parse(_input: ParseStream) -> Result<Self> {
        Ok(Nothing)
    }
}

#[cfg(feature = "printing")]
#[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
impl ToTokens for Nothing {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let _ = tokens;
    }
}

#[cfg(feature = "clone-impls")]
#[cfg_attr(docsrs, doc(cfg(feature = "clone-impls")))]
impl Clone for Nothing {
    fn clone(&self) -> Self {
        *self
    }
}

#[cfg(feature = "clone-impls")]
#[cfg_attr(docsrs, doc(cfg(feature = "clone-impls")))]
impl Copy for Nothing {}

#[cfg(feature = "extra-traits")]
#[cfg_attr(docsrs, doc(cfg(feature = "extra-traits")))]
impl Debug for Nothing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Nothing")
    }
}

#[cfg(feature = "extra-traits")]
#[cfg_attr(docsrs, doc(cfg(feature = "extra-traits")))]
impl Eq for Nothing {}

#[cfg(feature = "extra-traits")]
#[cfg_attr(docsrs, doc(cfg(feature = "extra-traits")))]
impl PartialEq for Nothing {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

#[cfg(feature = "extra-traits")]
#[cfg_attr(docsrs, doc(cfg(feature = "extra-traits")))]
impl Hash for Nothing {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}
