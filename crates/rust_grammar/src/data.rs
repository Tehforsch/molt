use molt_lib::{NodeId, NodeList};

use crate::attr::Attribute;
use crate::expr::{Expr, Index, Member};
use crate::ident::Ident;
use crate::punctuated::{self};
use crate::restriction::{FieldMutability, Visibility};
use crate::token;
use crate::ty::Type;

ast_struct! {
    /// An enum variant.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct Variant {
        pub attrs: Vec<Attribute>,

        /// Name of the variant.
        pub ident: NodeId<Ident>,

        /// Content stored in the variant.
        pub fields: Fields,

        /// Explicit discriminant: `Variant = 1`
        pub discriminant: Option<(Token![=], NodeId<Expr>)>,
    }
}

ast_enum_of_structs! {
    /// Data stored within an enum variant or struct.
    ///
    /// # Syntax tree enum
    ///
    /// This type is a [syntax tree enum].
    ///
    /// [syntax tree enum]: crate::expr::Expr#syntax-tree-enums
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub enum Fields {
        /// Named fields of a struct or struct variant such as `Point { x: f64,
        /// y: f64 }`.
        Named(FieldsNamed),

        /// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
        Unnamed(FieldsUnnamed),

        /// Unit struct or unit variant such as `None`.
        Unit,
    }
}

pub struct FieldNamed;

pub struct FieldUnnamed;

ast_struct! {
    /// Named fields of a struct or struct variant such as `Point { x: f64,
    /// y: f64 }`.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct FieldsNamed {
        pub brace_token: token::Brace,
        pub named: NodeList<Field, Token![,]>,
    }
}

ast_struct! {
    /// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct FieldsUnnamed {
        pub paren_token: token::Paren,
        pub unnamed: NodeList<Field, Token![,]>,
    }
}

ast_struct! {
    /// A field of a struct or enum variant.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct Field {
        pub attrs: Vec<Attribute>,

        pub vis: Visibility,

        pub mutability: FieldMutability,

        /// Name of the field, if any.
        ///
        /// Fields of tuple structs have no names.
        pub ident: Option<NodeId<Ident>>,

        pub colon_token: Option<Token![:]>,

        pub ty: NodeId<Type>,
    }
}

pub struct Members<'a> {
    fields: punctuated::Iter<'a, Field>,
    index: u32,
}

impl<'a> Iterator for Members<'a> {
    type Item = Member;

    fn next(&mut self) -> Option<Self::Item> {
        let field = self.fields.next()?;
        let member = match &field.ident {
            Some(ident) => Member::Named(ident.clone()),
            None => {
                #[cfg(all(feature = "parsing", feature = "printing"))]
                let span = crate::spanned::Spanned::span(&field.ty);
                #[cfg(not(all(feature = "parsing", feature = "printing")))]
                let span = proc_macro2::Span::call_site();
                Member::Unnamed(Index {
                    index: self.index,
                    span,
                })
            }
        };
        self.index += 1;
        Some(member)
    }
}

impl<'a> Clone for Members<'a> {
    fn clone(&self) -> Self {
        Members {
            fields: self.fields.clone(),
            index: self.index,
        }
    }
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    use molt_lib::{NodeId, SpannedPat, WithSpan};

    use crate::attr::Attribute;
    use crate::data::{Field, Fields, FieldsNamed, FieldsUnnamed, Variant};
    use crate::error::Result;
    use crate::ext::IdentExt as _;
    use crate::ident::{AnyIdent, Ident};
    #[cfg(not(feature = "full"))]
    use crate::parse::discouraged::Speculative as _;
    use crate::parse::{Parse, ParsePat, ParseStream};
    use crate::restriction::{FieldMutability, Visibility};
    #[cfg(not(feature = "full"))]
    use crate::scan_expr::scan_expr;
    use crate::token;
    use crate::ty::Type;
    use crate::verbatim;

    use super::{FieldNamed, FieldUnnamed};

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Variant {
        fn parse(input: ParseStream) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let _visibility: Visibility = input.parse()?;
            let ident: NodeId<Ident> = input.parse()?;
            let fields = if input.peek(token::Brace) {
                Fields::Named(input.parse()?)
            } else if input.peek(token::Paren) {
                Fields::Unnamed(input.parse()?)
            } else {
                Fields::Unit
            };
            let discriminant = if input.peek(Token![=]) {
                let eq_token: Token![=] = input.parse()?;
                #[cfg(feature = "full")]
                let discriminant = input.parse()?;
                #[cfg(not(feature = "full"))]
                let discriminant = {
                    let begin = input.fork();
                    let ahead = input.fork();
                    let mut discriminant: Result<Expr> = ahead.parse();
                    if discriminant.is_ok() {
                        input.advance_to(&ahead);
                    } else if scan_expr(input).is_ok() {
                        discriminant = Ok(Expr::Verbatim(verbatim::between(&begin, input)));
                    }
                    discriminant?
                };
                Some((eq_token, discriminant))
            } else {
                None
            };
            Ok(Variant {
                attrs,
                ident,
                fields,
                discriminant,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for FieldsNamed {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(FieldsNamed {
                brace_token: braced!(content in input),
                named: content
                    .parse_terminated_pat::<FieldNamed, _>(Token![,])?
                    .into(),
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for FieldsUnnamed {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(FieldsUnnamed {
                paren_token: parenthesized!(content in input),
                unnamed: content.parse_terminated_pat::<FieldUnnamed, _>(Token![,])?,
            })
        }
    }

    impl ParsePat for FieldNamed {
        type Target = Field;

        fn parse_pat(input: ParseStream) -> Result<SpannedPat<Field>> {
            if let Some(var) = input.parse_var() {
                return var;
            }
            input.call_spanned(|input| {
                let attrs = input.call(Attribute::parse_outer)?;
                let vis: Visibility = input.parse()?;

                let unnamed_field = cfg!(feature = "full") && input.peek(Token![_]);
                let ident = if unnamed_field {
                    input.parse_id::<AnyIdent>()
                } else {
                    input.parse()
                }?;

                let colon_token: Token![:] = input.parse()?;

                let ty: NodeId<Type> = if unnamed_field
                    && (input.peek(Token![struct])
                        || input.peek(Token![union]) && input.peek2(token::Brace))
                {
                    let begin = input.fork();
                    input.parse_id::<AnyIdent>()?;
                    input.parse::<FieldsNamed>()?;
                    input.add(
                        Type::Verbatim(verbatim::between(&begin, input))
                            .with_span(molt_lib::Span::fake()),
                    )
                } else {
                    input.parse()?
                };

                Ok(Field {
                    attrs,
                    vis,
                    mutability: FieldMutability::None,
                    ident: Some(ident),
                    colon_token: Some(colon_token),
                    ty,
                })
            })
        }
    }

    impl ParsePat for FieldUnnamed {
        type Target = Field;

        /// Parses an unnamed (tuple struct) field.
        fn parse_pat(input: ParseStream) -> Result<SpannedPat<Field>> {
            if let Some(var) = input.parse_var() {
                return var;
            }
            input.call_spanned(|input| {
                Ok(Field {
                    attrs: input.call(Attribute::parse_outer)?,
                    vis: input.parse()?,
                    mutability: FieldMutability::None,
                    ident: None,
                    colon_token: None,
                    ty: input.parse()?,
                })
            })
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use crate::data::{Field, FieldsNamed, FieldsUnnamed, Variant};
    use crate::print::TokensOrDefault;
    use proc_macro2::TokenStream;
    use quote::{ToTokens, TokenStreamExt};

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for Variant {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.ident.to_tokens(tokens);
            self.fields.to_tokens(tokens);
            if let Some((eq_token, disc)) = &self.discriminant {
                eq_token.to_tokens(tokens);
                disc.to_tokens(tokens);
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for FieldsNamed {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.brace_token.surround(tokens, |tokens| {
                self.named.to_tokens(tokens);
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for FieldsUnnamed {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.paren_token.surround(tokens, |tokens| {
                self.unnamed.to_tokens(tokens);
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for Field {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.vis.to_tokens(tokens);
            if let Some(ident) = &self.ident {
                ident.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.ty.to_tokens(tokens);
        }
    }
}
