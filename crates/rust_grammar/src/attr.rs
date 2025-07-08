use derive_macro::CmpSyn;

use crate::error::Error;
use crate::error::Result;
use crate::expr::Expr;
use crate::expr::ExprLit;
use crate::ident::TokenIdent;
use crate::lit::Lit;
use crate::mac;
use crate::mac::MacroDelimiter;
use crate::parse::Parse;
use crate::parse::ParseStream;
use crate::parse::discouraged::Speculative as _;
use crate::path::Path;
use crate::token;
use molt_lib::NodeId;
use molt_lib::WithSpan;
use proc_macro2::TokenStream;

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
///
/// <br>
///
/// # Parsing from tokens to Attribute
///
/// This type does not implement the [`Parse`] trait and thus cannot be
/// parsed directly by [`ParseStream::parse`]. Instead use
/// [`ParseStream::call`] with one of the two parser functions
/// [`Attribute::parse_outer`] or [`Attribute::parse_inner`] depending on
/// which you intend to parse.
///
/// [`Parse`]: crate::parse::Parse
/// [`ParseStream::parse`]: crate::parse::ParseBuffer::parse
/// [`ParseStream::call`]: crate::parse::ParseBuffer::call
///
/// ```
/// use syn::{Attribute, Ident, Result, Token};
/// use syn::parse::{Parse, ParseStream};
///
/// // Parses a unit struct with attributes.
/// //
/// //     #[path = "s.tmpl"]
/// //     struct S;
/// struct UnitStruct {
///     attrs: Vec<Attribute>,
///     struct_token: Token![struct],
///     name: Ident,
///     semi_token: Token![;],
/// }
///
/// impl Parse for UnitStruct {
///     fn parse(input: ParseStream) -> Result<Self> {
///         Ok(UnitStruct {
///             attrs: input.call(Attribute::parse_outer)?,
///             struct_token: input.parse()?,
///             name: input.parse()?,
///             semi_token: input.parse()?,
///         })
///     }
/// }
/// ```
///
/// <p><br></p>
///
/// # Parsing from Attribute to structured arguments
///
/// The grammar of attributes in Rust is very flexible, which makes the
/// syntax tree not that useful on its own. In particular, arguments of the
/// `Meta::List` variety of attribute are held in an arbitrary `tokens:
/// TokenStream`. Macros are expected to check the `path` of the attribute,
/// decide whether they recognize it, and then parse the remaining tokens
/// according to whatever grammar they wish to require for that kind of
/// attribute. Use [`parse_args()`] to parse those tokens into the expected
/// data structure.
///
/// [`parse_args()`]: Attribute::parse_args
///
/// <p><br></p>
///
/// # Doc comments
///
/// The compiler transforms doc comments, such as `/// comment` and `/*!
/// comment */`, into attributes before macros are expanded. Each comment is
/// expanded into an attribute of the form `#[doc = r"comment"]`.
///
/// As an example, the following `mod` items are expanded identically:
///
/// ```
/// # use syn::{ItemMod, parse_quote};
/// let doc: ItemMod = parse_quote! {
///     /// Single line doc comments
///     /// We write so many!
///     /**
///      * Multi-line comments...
///      * May span many lines
///      */
///     mod example {
///         //! Of course, they can be inner too
///         /*! And fit in a single line */
///     }
/// };
/// let attr: ItemMod = parse_quote! {
///     #[doc = r" Single line doc comments"]
///     #[doc = r" We write so many!"]
///     #[doc = r"
///      * Multi-line comments...
///      * May span many lines
///      "]
///     mod example {
///         #![doc = r" Of course, they can be inner too"]
///         #![doc = r" And fit in a single line "]
///     }
/// };
/// assert_eq!(doc, attr);
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
///
/// # Syntax tree enum
///
/// This type is a [syntax tree enum].
///
/// [syntax tree enum]: crate::expr::Expr#syntax-tree-enums
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
    pub value: NodeId<Expr>,
}

impl Meta {
    /// Returns the path that begins this structured meta item.
    ///
    /// For example this would return the `test` in `#[test]`, the `derive` in
    /// `#[derive(Copy)]`, and the `path` in `#[path = "sys/windows.rs"]`.
    pub fn path(&self) -> &Path {
        match self {
            Meta::Path(path) => path,
            Meta::List(meta) => &meta.path,
            Meta::NameValue(meta) => &meta.path,
        }
    }

    /// Error if this is a `Meta::List` or `Meta::NameValue`.
    pub fn require_path_only(&self) -> Result<&Path> {
        let error_span = match self {
            Meta::Path(path) => return Ok(path),
            Meta::List(meta) => meta.delimiter.span().open(),
            Meta::NameValue(meta) => meta.eq_token.span,
        };
        Err(Error::new(error_span, "unexpected token in attribute"))
    }
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

pub(crate) fn parse_meta_after_path(path: Path, input: ParseStream) -> Result<Meta> {
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
    let lit: Option<NodeId<Lit>> = ahead.parse()?;
    let value = if let (Some(lit), true) = (lit, ahead.is_empty()) {
        input.advance_to(&ahead);
        let span = input.ctx().get_span(lit);
        input.add(
            Expr::Lit(ExprLit {
                attrs: Vec::new(),
                lit,
            })
            .with_span(span),
        )
    } else if input.peek(Token![#]) && input.peek2(token::Bracket) {
        return Err(input.error("unexpected attribute inside of attribute"));
    } else {
        input.parse()?
    };
    Ok(MetaNameValue {
        path,
        eq_token,
        value,
    })
}
