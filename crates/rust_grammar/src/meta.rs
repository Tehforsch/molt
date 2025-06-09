//! Facility for interpreting structured content inside of an `Attribute`.

use crate::error::Result;
use crate::ident::AnyIdent;
use crate::lit::Lit;
use crate::parse::ParseStream;
use crate::path::{Path, PathSegment};
use crate::punctuated::Punctuated;

/// Context for parsing a single property in the conventional syntax for
/// structured attributes.
///
/// # Examples
///
/// Refer to usage examples on the following two entry-points:
///
/// - [`Attribute::parse_nested_meta`] if you have an entire `Attribute` to
///   parse. Always use this if possible. Generally this is able to produce
///   better error messages because `Attribute` holds span information for all
///   of the delimiters therein.
///
/// - [`syn::meta::parser`] if you are implementing a `proc_macro_attribute`
///   macro and parsing the arguments to the attribute macro, i.e. the ones
///   written in the same attribute that dispatched the macro invocation. Rustc
///   does not pass span information for the surrounding delimiters into the
///   attribute macro invocation in this situation, so error messages might be
///   less precise.
///
/// [`Attribute::parse_nested_meta`]: crate::Attribute::parse_nested_meta
/// [`syn::meta::parser`]: crate::meta::parser
#[non_exhaustive]
pub struct ParseNestedMeta<'a> {
    pub path: Path,
    pub input: ParseStream<'a>,
}

impl<'a> ParseNestedMeta<'a> {
    /// Used when parsing `key = "value"` syntax.
    ///
    /// All it does is advance `meta.input` past the `=` sign in the input. You
    /// could accomplish the same effect by writing
    /// `meta.parse::<Token![=]>()?`, so at most it is a minor convenience to
    /// use `meta.value()?`.
    ///
    /// # Example
    ///
    /// ```
    /// use syn::{parse_quote, Attribute, LitStr};
    ///
    /// let attr: Attribute = parse_quote! {
    ///     #[tea(kind = "EarlGrey")]
    /// };
    ///                                          // conceptually:
    /// if attr.path().is_ident("tea") {         // this parses the `tea`
    ///     attr.parse_nested_meta(|meta| {      // this parses the `(`
    ///         if meta.path.is_ident("kind") {  // this parses the `kind`
    ///             let value = meta.value()?;   // this parses the `=`
    ///             let s: LitStr = value.parse()?;  // this parses `"EarlGrey"`
    ///             if s.value() == "EarlGrey" {
    ///                 // ...
    ///             }
    ///             Ok(())
    ///         } else {
    ///             Err(meta.error("unsupported attribute"))
    ///         }
    ///     })?;
    /// }
    /// # anyhow::Ok(())
    /// ```
    pub fn value(&self) -> Result<ParseStream<'a>> {
        self.input.parse::<Token![=]>()?;
        Ok(self.input)
    }
}

// Like Path::parse_mod_style, but accepts keywords in the path.
fn parse_meta_path(input: ParseStream) -> Result<Path> {
    Ok(Path {
        leading_colon: input.parse()?,
        segments: {
            let mut segments = Punctuated::new();
            if input.peek_pat::<AnyIdent>() {
                let ident = input.parse_id::<AnyIdent>()?;
                segments.push_value(PathSegment::from(ident));
            } else if input.is_empty() {
                return Err(input.error("expected nested attribute"));
            } else if input.peek(Lit) {
                return Err(input.error("unexpected literal in nested attribute, expected ident"));
            } else {
                return Err(input.error("unexpected token in nested attribute, expected ident"));
            }
            while input.peek(Token![::]) {
                let punct = input.parse()?;
                segments.push_punct(punct);
                let ident = input.parse_id::<AnyIdent>()?;
                segments.push_value(PathSegment::from(ident));
            }
            segments
        },
    })
}
