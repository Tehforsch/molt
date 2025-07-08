use derive_macro::CmpSyn;
use molt_lib::{NodeId, NodeList, Spanned, WithSpan};

use crate::attr::Attribute;
use crate::error::Result;
use crate::expr::{Expr, Index, Member};
use crate::ident::{AnyIdent, Ident};
use crate::parse::{Parse, ParseList, ParseNode, ParseStream, parse_punctuated_list_real};
use crate::punctuated::{self};
use crate::restriction::{FieldMutability, Visibility};
use crate::ty::Type;
use crate::{token, verbatim};

#[derive(Debug, CmpSyn)]
/// An enum variant.
pub struct Variant {
    pub attrs: Vec<Attribute>,

    /// Name of the variant.
    pub ident: NodeId<Ident>,

    /// Content stored in the variant.
    pub fields: Fields,

    /// Explicit discriminant: `Variant = 1`
    pub discriminant: Option<(Token![=], NodeId<Expr>)>,
}

#[derive(Debug, CmpSyn)]
/// Data stored within an enum variant or struct.
pub enum Fields {
    /// Named fields of a struct or struct variant such as `Point { x: f64,
    /// y: f64 }`.
    Named(FieldsNamed),

    /// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
    Unnamed(FieldsUnnamed),

    /// Unit struct or unit variant such as `None`.
    Unit,
}

pub struct FieldNamed;

pub struct FieldUnnamed;

#[derive(Debug, CmpSyn)]
/// Named fields of a struct or struct variant such as `Point { x: f64,
/// y: f64 }`.
pub struct FieldsNamed {
    pub brace_token: token::Brace,
    pub named: NodeList<Field, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
pub struct FieldsUnnamed {
    pub paren_token: token::Paren,
    pub unnamed: NodeList<Field, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A field of a struct or enum variant.
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

pub struct Members<'a> {
    fields: punctuated::Iter<'a, Field>,
    index: u32,
}

impl<'a> Iterator for Members<'a> {
    type Item = Member;

    fn next(&mut self) -> Option<Self::Item> {
        let field = self.fields.next()?;
        let member = match &field.ident {
            Some(ident) => Member::Named(*ident),
            None => {
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

            let discriminant = input.parse()?;
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

impl Parse for FieldsNamed {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(FieldsNamed {
            brace_token: braced!(content in input),
            named: content.parse_list::<FieldsNamed>()?,
        })
    }
}

impl Parse for FieldsUnnamed {
    fn parse(input: ParseStream) -> Result<Self> {
        dbg!(&input.cursor().token_stream().to_string());
        let content;
        Ok(FieldsUnnamed {
            paren_token: parenthesized!(content in input),
            unnamed: content.parse_list::<FieldsUnnamed>()?,
        })
    }
}

impl ParseList for FieldsNamed {
    type Item = Field;
    type ParseItem = FieldNamed;
    type Punct = Token![,];

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Self::Item>>> {
        parse_punctuated_list_real::<FieldNamed, Self::Punct>(input)
    }
}

impl ParseList for FieldsUnnamed {
    type Item = Field;
    type ParseItem = FieldUnnamed;
    type Punct = Token![,];

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Self::Item>>> {
        parse_punctuated_list_real::<FieldUnnamed, Self::Punct>(input)
    }
}

impl ParseNode for FieldNamed {
    type Target = Field;

    fn parse_spanned(input: ParseStream) -> Result<Spanned<Field>> {
        input.call_spanned(|input| {
            let attrs = input.call(Attribute::parse_outer)?;
            let vis: Visibility = input.parse()?;

            let unnamed_field = input.peek(Token![_]);
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

impl ParseNode for FieldUnnamed {
    type Target = Field;

    /// Parses an unnamed (tuple struct) field.
    fn parse_spanned(input: ParseStream) -> Result<Spanned<Field>> {
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
