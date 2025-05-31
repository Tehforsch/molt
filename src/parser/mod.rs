pub(crate) mod molt_grammar;
mod node;
pub(crate) mod rust_grammar;
mod span;
#[cfg(test)]
mod tests;

use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;
use std::{cell::Cell, marker::PhantomData};

pub(crate) use molt_grammar::{Command, Decl, MoltFile, Todo, UntypedVar, Var, VarDecl, VarId};
pub(crate) use node::{Kind, Node, Pattern, ToNode, UserKind};
pub(crate) use rust_grammar::RustFile;
use rust_grammar::{Attribute, Ident};
pub(crate) use span::Span;
use syn::token::Token;

use crate::ctx::{Ctx, Id, NodeId, NodeList};

pub type Result<T, E = syn::Error> = std::result::Result<T, E>;

pub type ParseStream<'a> = &'a Parser<'a>;

fn parse_file<T: Parse>(code: &str, mode: Mode) -> Result<(T, Ctx), crate::Error> {
    let tokens = proc_macro2::TokenStream::from_str(code)?;
    Ok(Parser::parse_tokens::<T>(tokens, mode)?)
}

pub fn parse_rust_file(code: &str) -> Result<(RustFile, Ctx), crate::Error> {
    parse_file::<RustFile>(code, Mode::Rust)
}

pub fn parse_molt_file(code: &str) -> Result<(MoltFile, Ctx), crate::Error> {
    parse_file::<MoltFile>(code, Mode::Molt)
}

#[derive(Copy, Clone)]
pub enum Mode {
    Molt,
    Rust,
}

pub(crate) struct Spanned<T> {
    pub span: Span,
    pub item: T,
}

impl<T> Spanned<T> {
    pub fn map<S>(self, f: impl Fn(T) -> S) -> Spanned<S> {
        Spanned {
            span: self.span,
            item: f(self.item),
        }
    }
}

struct SpanMarker {
    start: usize,
}

pub(crate) trait Parse: Sized {
    fn parse(input: ParseStream) -> Result<Self>;
}

pub(crate) struct Parser<'a> {
    ctx: Rc<RefCell<Ctx>>,
    // TODO make this properly private, this should never be touched
    // from outside.  Currently, since the parsing is done in
    // submodules of parser, this can still be accessed.
    stream: syn::parse::ParseStream<'a>,
    mode: Mode,
    // This is a really ugly field to accomodate the fact that
    // `syn::parse::ParseStream::prev_span` is a private method
    // but required to properly create the spans we need for
    // parse_spanned and other methods that create a `Node` in the
    // `ctx`.
    prev_span: RefCell<proc_macro2::Span>,
}

impl<'a> Parser<'a> {
    fn parse_tokens<T: Parse>(tokens: proc_macro2::TokenStream, mode: Mode) -> Result<(T, Ctx)> {
        match mode {
            Mode::Molt => {
                let wrapper = syn::parse2::<MoltWrapper<T>>(tokens)?;
                Ok((wrapper.0, wrapper.1))
            }
            Mode::Rust => {
                let wrapper = syn::parse2::<RustWrapper<T>>(tokens)?;
                Ok((wrapper.0, wrapper.1))
            }
        }
    }

    fn parse<T: Parse>(&self) -> Result<T> {
        *self.prev_span.borrow_mut() = self.stream.span();
        T::parse(self)
    }

    fn parse_spanned<T: Parse>(&self) -> Result<Spanned<T>> {
        let marker = self.span_marker();
        let t = T::parse(self)?;
        Ok(self.make_spanned(marker, t))
    }

    fn is_empty(&self) -> bool {
        self.stream.is_empty()
    }

    fn peek<T: syn::parse::Peek>(&self, token: T) -> bool {
        self.stream.peek(token)
    }

    fn clone_with<'b>(&self, stream: &'b syn::parse::ParseBuffer<'b>) -> Parser<'b> {
        Parser {
            ctx: self.ctx.clone(),
            stream: &stream,
            mode: self.mode,
            prev_span: stream.span().into(),
        }
    }

    fn add_var(&self, var: Var) -> VarId {
        self.ctx.borrow_mut().add_var(var)
    }

    fn add_var_typed<T: ToNode>(&self, var: Var) -> NodeId<T> {
        self.ctx.borrow_mut().add_var_typed(var)
    }

    fn add_node(&self, node: Spanned<Node>) -> Id {
        self.ctx.borrow_mut().add_node(node, self.mode)
    }

    fn add_item<T: ToNode>(&self, t: Spanned<T>) -> NodeId<T> {
        self.ctx.borrow_mut().add(t, self.mode)
    }

    fn lookahead1(&self) -> Lookahead1 {
        Lookahead1(self.stream.lookahead1())
    }

    fn call<T>(&'a self, function: fn(ParseStream<'a>) -> Result<T>) -> Result<T> {
        function(self)
    }

    fn call_internal<T>(
        &'a self,
        function: fn(syn::parse::ParseStream<'a>) -> Result<T>,
    ) -> Result<T> {
        *self.prev_span.borrow_mut() = self.stream.span();
        function(self.stream)
    }

    fn span_marker(&self) -> SpanMarker {
        let start = self.stream.span().byte_range().start;
        SpanMarker { start }
    }

    fn make_spanned<T>(&self, marker: SpanMarker, item: T) -> Spanned<T> {
        let end = self.prev_span.borrow().byte_range().end;
        let span = Span::new(marker.start, end);
        Spanned { span, item }
    }

    fn error(&self, s: &str) -> syn::Error {
        syn::Error::new(self.stream.span(), s)
    }
}

struct Lookahead1<'a>(syn::parse::Lookahead1<'a>);

impl<'a> Lookahead1<'a> {
    // TODO own the trait here and
    // make all node kinds return true
    // on peek if the encountered item is
    // a var
    fn peek<T: syn::parse::Peek>(&self, token: T) -> bool {
        self.0.peek(token)
    }

    fn peek_ident(&self) -> bool {
        self.0.peek(syn::Ident) || self.0.peek(syn::Token![$])
    }

    fn error(self) -> syn::Error {
        self.0.error()
    }
}

// This function along with `MoltWrapper` and `RustWrapper` exist only
// to construct the `syn::parse::ParseStream` that we need for
// everything to work. The only public API that makes sense for us to
// get this stream from is `syn::parse2`, but this needs a type that
// implements `syn::Parse`.  Therefore, we construct a type that
// implements `syn::Parse` but then internally constructs our own
// `Parser` along with a `Ctx` and then uses the `crate::Parse` impl
// to do the rest of the work.
// We can't pass any other arguments along with the `syn::parse2`
// call, so we "pass" the parsing mode via the type, which is why both
// `MoltWrapper` and `RustWrapper` exist.
fn parse_shared<T: Parse>(stream: syn::parse::ParseStream, mode: Mode) -> Result<(T, Ctx)> {
    let mut p = Parser {
        ctx: Rc::new(RefCell::new(Ctx::default())),
        stream,
        mode,
        prev_span: stream.span().into(),
    };
    let t = p.parse::<T>()?;
    Ok((t, p.ctx.take()))
}

struct MoltWrapper<T>(T, Ctx);

impl<T: Parse> syn::parse::Parse for MoltWrapper<T> {
    fn parse(stream: syn::parse::ParseStream) -> Result<Self> {
        let (t, ctx) = parse_shared(stream, Mode::Molt)?;
        Ok(MoltWrapper(t, ctx))
    }
}

struct RustWrapper<T>(T, Ctx);

impl<T: Parse> syn::parse::Parse for RustWrapper<T> {
    fn parse(stream: syn::parse::ParseStream) -> Result<Self> {
        let (t, ctx) = parse_shared(stream, Mode::Rust)?;
        Ok(RustWrapper(t, ctx))
    }
}

impl<T: syn::parse::Parse> Parse for T {
    fn parse(input: ParseStream) -> Result<Self> {
        input.stream.parse::<T>()
    }
}

fn convert_attrs(input: ParseStream, attrs: Vec<syn::Attribute>) -> Result<NodeList<Attribute>> {
    Ok(attrs
        .into_iter()
        .map(|attr| {
            let span = <syn::Attribute as syn::spanned::Spanned>::span(&attr);
            let spanned = Spanned {
                span: span.into(),
                item: Attribute::new(attr),
            };
            input.add_item(spanned)
        })
        .collect())
}

impl Attribute {
    fn parse_inner(input: ParseStream) -> Result<NodeList<Self>> {
        convert_attrs(input, syn::Attribute::parse_inner(input.stream)?)
    }

    fn parse_outer(input: ParseStream) -> Result<NodeList<Self>> {
        convert_attrs(input, syn::Attribute::parse_outer(input.stream)?)
    }
}

macro_rules! parenthesized {
    ($content:ident in $cursor:expr) => {
        let syn_content;
        let syn_cursor = $cursor.stream;
        syn::parenthesized!(syn_content in syn_cursor);
        $content = $cursor.clone_with(&syn_content);
    };
}

macro_rules! braced {
    ($content:ident in $cursor:expr) => {
        let syn_content;
        let syn_cursor = $cursor.stream;
        syn::braced!(syn_content in syn_cursor);
        $content = $cursor.clone_with(&syn_content);
    };
}

macro_rules! bracketed {
    ($content:ident in $cursor:expr) => {
        let syn_content;
        let syn_cursor = $cursor.stream;
        syn::bracketed!(syn_content in syn_cursor);
        $content = $cursor.clone_with(&syn_content);
    };
}

pub(crate) use braced;
pub(crate) use parenthesized;
