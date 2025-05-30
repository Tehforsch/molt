mod molt_grammar;
mod node;
mod parse;
mod rust_grammar;
mod span;
#[cfg(test)]
mod tests;

use std::marker::PhantomData;
use std::str::FromStr;

pub(crate) use molt_grammar::{Command, Decl, MoltFile, Todo, Var, VarDecl, VarId};
pub(crate) use node::{CustomDebug, Kind, Node, Pattern, ToNode};
pub(crate) use rust_grammar::RustFile;
pub(crate) use span::Span;
use syn::parse::{ParseBuffer, ParseStream};

use crate::ctx::Ctx;

pub type Result<T, E = syn::Error> = std::result::Result<T, E>;

fn parse_file<T: Parse>(code: &str, mode: Mode) -> Result<(T, Ctx), crate::Error> {
    let tokens = proc_macro2::TokenStream::from_str(code)?;
    Ok(Parser::parse_top_level::<T>(tokens, mode)?)
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
    fn parse<'a>(parser: &'a mut Parser<'a>) -> Result<Self>;
}

pub(crate) struct Parser<'a> {
    ctx: Ctx,
    stream: ParseStream<'a>,
}

impl<'a> Parser<'a> {
    fn parse_top_level<T: Parse>(tokens: proc_macro2::TokenStream, mode: Mode) -> Result<(T, Ctx)> {
        let wrapper = syn::parse2::<Wrapper<T>>(tokens)?;
        Ok((wrapper.0, wrapper.1))
    }

    fn parse<T: Parse>(&self) -> Result<T> {
        todo!()
    }
}

struct Wrapper<T>(T, Ctx);

impl<T: Parse> syn::parse::Parse for Wrapper<T> {
    fn parse(stream: syn::parse::ParseStream) -> Result<Self> {
        let mut p = Parser {
            ctx: Ctx::default(),
            stream,
        };
        let t = p.parse::<T>()?;
        Ok(Wrapper(t, p.ctx))
    }
}
