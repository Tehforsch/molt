use derive_macro::CmpSyn;
use molt_lib::NodeId;
use proc_macro2::TokenStream;

use crate::error::Result;
use crate::expr::{Expr, ExprLit};
use crate::ident::TokenIdent;
use crate::lit::Lit;
use crate::mac::MacroDelimiter;
use crate::parse::discouraged::Speculative as _;
use crate::parse::{Parse, ParseNode, ParseStream};
use crate::path::Path;
use crate::{mac, token};

#[derive(Debug, CmpSyn)]
/// An attribute, like `#[repr(transparent)]`.
///
/// <br>
///
/// # Syntax
///
/// Rust has six types of attributes.
///
/// - Outer attributes like `#[repr(transparent)]`. These appear outside or
///   in front of the item they describe.
///
/// - Inner attributes like `#![feature(proc_macro)]`. These appear inside
///   of the item they describe, usually a module.
///
/// - Outer one-line doc comments like `/// Example`.
///
/// - Inner one-line doc comments like `//! Please file an issue`.
///
/// - Outer documentation blocks `/** Example */`.
///
/// - Inner documentation blocks `/*! Please file an issue */`.
///
/// The `style` field of type `AttrStyle` distinguishes whether an attribute
/// is outer or inner.
///
/// Every attribute has a `path` that indicates the intended interpretation
/// of the rest of the attribute's contents. The path and the optional
/// additional contents are represented together in the `meta` field of the
/// attribute in three possible varieties:
///
/// - Meta::Path &mdash; attributes whose information content conveys just a
///   path, for example the `#[test]` attribute.
///
/// - Meta::List &mdash; attributes that carry arbitrary tokens after the
///   path, surrounded by a delimiter (parenthesis, bracket, or brace). For
///   example `#[derive(Copy)]` or `#[precondition(x < 5)]`.
///
/// - Meta::NameValue &mdash; attributes with an `=` sign after the path,
///   followed by a Rust expression. For example `#[path =
///   "sys/windows.rs"]`.
///
/// All doc comments are represented in the NameValue style with a path of
/// "doc", as this is how they are processed by the compiler and by
/// `macro_rules!` macros.
///
/// ```text
/// #[derive(Copy, Clone)]
///   ~~~~~~Path
///   ^^^^^^^^^^^^^^^^^^^Meta::List
///
/// #[path = "sys/windows.rs"]
///   ~~~~Path
///   ^^^^^^^^^^^^^^^^^^^^^^^Meta::NameValue
///
/// #[test]
///   ^^^^Meta::Path
/// ```
pub struct Attribute {
    pub pound_token: Token![#],
    pub style: AttrStyle,
    pub bracket_token: token::Bracket,
    pub meta: Meta,
}

impl Attribute {
    /// Parses zero or more outer attributes from the stream.
    ///
    /// # Example
    ///
    /// See
    /// [*Parsing from tokens to Attribute*](#parsing-from-tokens-to-attribute).
    pub fn parse_outer(input: ParseStream) -> Result<Vec<Self>> {
        let mut attrs = Vec::new();
        while input.peek(Token![#]) {
            attrs.push(input.call(single_parse_outer)?);
        }
        Ok(attrs)
    }

    /// Parses zero or more inner attributes from the stream.
    ///
    /// # Example
    ///
    /// See
    /// [*Parsing from tokens to Attribute*](#parsing-from-tokens-to-attribute).
    pub fn parse_inner(input: ParseStream) -> Result<Vec<Self>> {
        let mut attrs = Vec::new();
        parse_inner(input, &mut attrs)?;
        Ok(attrs)
    }
}

#[derive(Debug, CmpSyn)]
/// Distinguishes between attributes that decorate an item and attributes
/// that are contained within an item.
///
/// # Outer attributes
///
/// - `#[repr(transparent)]`
/// - `/// # Example`
/// - `/** Please file an issue */`
///
/// # Inner attributes
///
/// - `#![feature(proc_macro)]`
/// - `//! # Example`
/// - `/*! Please file an issue */`
pub enum AttrStyle {
    Outer,
    Inner(Token![!]),
}

#[derive(Debug, CmpSyn)]
/// Content of a compile-time structured attribute.
///
/// ## Path
///
/// A meta path is like the `test` in `#[test]`.
///
/// ## List
///
/// A meta list is like the `derive(Copy)` in `#[derive(Copy)]`.
///
/// ## NameValue
///
/// A name-value meta is like the `path = "..."` in `#[path =
/// "sys/windows.rs"]`.
pub enum Meta {
    Path(Path),

    /// A structured list within an attribute, like `derive(Copy, Clone)`.
    List(MetaList),

    /// A name-value pair within an attribute, like `feature = "nightly"`.
    NameValue(MetaNameValue),
}

#[derive(Debug, CmpSyn)]
/// A structured list within an attribute, like `derive(Copy, Clone)`.
pub struct MetaList {
    pub path: Path,
    pub delimiter: MacroDelimiter,
    pub tokens: TokenStream,
}

#[derive(Debug, CmpSyn)]
/// A name-value pair within an attribute, like `feature = "nightly"`.
pub struct MetaNameValue {
    pub path: Path,
    pub eq_token: Token![=],
    // This is intentionally not a `NodeId` since we
    // don't want to match on the exprs inside of attrs.
    // If we ever add a feature like matching on
    // #[cfg(...)] arms, we might have to change this
    // and add functionality to ignore specific NodeId
    // during matching.
    pub value: Expr,
}

impl From<Path> for Meta {
    fn from(meta: Path) -> Meta {
        Meta::Path(meta)
    }
}

impl From<MetaList> for Meta {
    fn from(meta: MetaList) -> Meta {
        Meta::List(meta)
    }
}

impl From<MetaNameValue> for Meta {
    fn from(meta: MetaNameValue) -> Meta {
        Meta::NameValue(meta)
    }
}

pub(crate) fn parse_inner(input: ParseStream, attrs: &mut Vec<Attribute>) -> Result<()> {
    while input.peek(Token![#]) && input.peek2(Token![!]) {
        attrs.push(input.call(single_parse_inner)?);
    }
    Ok(())
}

pub(crate) fn single_parse_inner(input: ParseStream) -> Result<Attribute> {
    let content;
    Ok(Attribute {
        pound_token: input.parse()?,
        style: AttrStyle::Inner(input.parse()?),
        bracket_token: bracketed!(content in input),
        meta: content.parse()?,
    })
}

pub(crate) fn single_parse_outer(input: ParseStream) -> Result<Attribute> {
    let content;
    Ok(Attribute {
        pound_token: input.parse()?,
        style: AttrStyle::Outer,
        bracket_token: bracketed!(content in input),
        meta: content.parse()?,
    })
}

impl Parse for Meta {
    fn parse(input: ParseStream) -> Result<Self> {
        let path = parse_outermost_meta_path(input)?;
        parse_meta_after_path(path, input)
    }
}

impl Parse for MetaList {
    fn parse(input: ParseStream) -> Result<Self> {
        let path = parse_outermost_meta_path(input)?;
        parse_meta_list_after_path(path, input)
    }
}

impl Parse for MetaNameValue {
    fn parse(input: ParseStream) -> Result<Self> {
        let path = parse_outermost_meta_path(input)?;
        parse_meta_name_value_after_path(path, input)
    }
}

// Unlike meta::parse_meta_path which accepts arbitrary keywords in the path,
// only the `unsafe` keyword is accepted as an attribute's outermost path.
fn parse_outermost_meta_path(input: ParseStream) -> Result<Path> {
    if input.peek(Token![unsafe]) {
        let unsafe_ident = input.parse_id::<TokenIdent<Token![unsafe]>>()?;
        Ok(Path::from(unsafe_ident))
    } else {
        Path::parse_mod_style(input)
    }
}

fn parse_meta_after_path(path: Path, input: ParseStream) -> Result<Meta> {
    if input.peek(token::Paren) || input.peek(token::Bracket) || input.peek(token::Brace) {
        parse_meta_list_after_path(path, input).map(Meta::List)
    } else if input.peek(Token![=]) {
        parse_meta_name_value_after_path(path, input).map(Meta::NameValue)
    } else {
        Ok(Meta::Path(path))
    }
}

fn parse_meta_list_after_path(path: Path, input: ParseStream) -> Result<MetaList> {
    let (delimiter, tokens) = mac::parse_delimiter(input)?;
    Ok(MetaList {
        path,
        delimiter,
        tokens,
    })
}

fn parse_meta_name_value_after_path(path: Path, input: ParseStream) -> Result<MetaNameValue> {
    let eq_token: Token![=] = input.parse()?;
    let ahead = input.fork();
    let lit: Option<Lit> = ahead.parse()?;
    let value: Expr = if let (Some(_lit), true) = (lit, ahead.is_empty()) {
        input.advance_to(&ahead);
        Expr::Lit(ExprLit {
            attrs: Vec::new(),
            // We use a placeholder here, since we don't want to add
            // the Lit above to the ctx (we would match it otherwise).
            // This is safe, since we also dont add the expr to the
            // ctx, so there is no way to refer to the id later.
            //
            // If we ever allow matching on exprs in attributes (to check
            // for #[cfg(...)] for example, this needs to be changed.
            lit: NodeId::placeholder(),
        })
    } else if input.peek(Token![#]) && input.peek2(token::Bracket) {
        return Err(input.error("unexpected attribute inside of attribute"));
    } else {
        Expr::parse_pat(input)?.unwrap_real()
    };
    Ok(MetaNameValue {
        path,
        eq_token,
        value,
    })
}
