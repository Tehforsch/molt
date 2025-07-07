#![allow(clippy::large_enum_variant)]

#[macro_use]
mod group;

#[macro_use]
pub mod token;

mod attr;
pub use proc_macro2::Span;

mod bigint;

pub mod buffer;

mod classify;

mod custom_keyword;

mod custom_punctuation;

mod data;
pub use crate::data::{Field, FieldNamed, FieldUnnamed};

mod derive;

mod drops;

mod error;
pub use crate::error::{Error, Result};

mod expr;
pub use crate::expr::Expr;

pub mod ext;

mod file;

pub use crate::file::{parse_file, File};

mod generics;

mod ident;
pub use crate::ident::Ident;

mod item;
pub use crate::item::Item;

mod lifetime;

mod lit;
pub use crate::lit::Lit;

mod lookahead;

mod mac;

mod op;

pub mod parse;

mod pat;
pub use pat::Pat;

mod path;

mod precedence;

pub mod punctuated;

mod restriction;

mod sealed;

mod span;

mod stmt;
pub use crate::stmt::Stmt;

mod thread;

mod ty;
pub use crate::ty::Type;

mod verbatim;

mod whitespace;

#[path = "export.rs"]
pub mod __private;

pub(crate) mod node;
pub use node::{Kind, Node};

pub use parse::parse_ctx;
pub use parse::parse_str;
pub use parse::parse_with_ctx;

pub use parse::ParseCtx;
pub use proc_macro2::TokenStream;
pub use proc_macro2::TokenTree;
