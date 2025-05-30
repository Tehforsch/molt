mod molt_grammar;
mod node;
mod rust_grammar;
mod span;
#[cfg(test)]
mod tests;

use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;
use std::{cell::Cell, marker::PhantomData};

pub(crate) use molt_grammar::{Command, Decl, MoltFile, Todo, Var, VarDecl, VarId};
pub(crate) use node::{CustomDebug, Kind, Node, Pattern, ToNode};
pub(crate) use rust_grammar::RustFile;
pub(crate) use span::Span;

use crate::ctx::{Ctx, Id, NodeId};

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

pub(crate) trait Parse: Sized {
    fn parse<'a>(parser: &'a Parser<'a>) -> Result<Self>;
}

pub(crate) struct Parser<'a> {
    ctx: Rc<RefCell<Ctx>>,
    stream: syn::parse::ParseStream<'a>,
    mode: Mode,
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
        T::parse(self)
    }

    // TODO
    fn parse_spanned<T: Parse>(&self) -> Result<T> {
        T::parse(self)
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
        }
    }

    fn add_var(&self, var: Var) -> VarId {
        self.ctx.borrow_mut().add_var(var)
    }

    fn add_node(&self, node: Node) -> Id {
        self.ctx.borrow_mut().add_node(node, self.mode)
    }

    fn add_item<T: Parse + ToNode>(&self, t: T) -> NodeId<T> {
        self.ctx.borrow_mut().add(t, self.mode)
    }
}

// This function along with `MoltWrapper` and `RustWrapper` exist only to
// construct the `syn::parse::ParseStream` that we need for everything to
// work. The only public API that makes sense for us to get this stream from
// is `syn::parse2`, but this needs a type that implements `syn::Parse`.
// Therefore, we construct a type that implements `syn::Parse` but then internally
// constructs our own `Parser` along with a `Ctx` and then uses the `crate::Parse`
// impl to do the rest of the work.
fn parse_shared<T: Parse>(stream: syn::parse::ParseStream, mode: Mode) -> Result<(T, Ctx)> {
    let mut p = Parser {
        ctx: Rc::new(RefCell::new(Ctx::default())),
        stream,
        mode,
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
