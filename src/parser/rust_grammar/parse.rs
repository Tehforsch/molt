use syn::Token;

use crate::parser::{Parse, ParseStream, Parser};

use super::super::Result;
use super::{Attribute, Expr, Generics, Ident, Item, ItemConst, Lit, RustFile, Type, Visibility};

impl Parse for RustFile {
    fn parse(input: ParseStream) -> Result<Self> {
        let _attrs = input.call(Attribute::parse_inner)?;
        Ok(RustFile {
            items: {
                let mut items = Vec::new();
                while !input.is_empty() {
                    items.push(input.parse()?);
                }
                items
            },
        })
    }
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Item::Const(input.parse()?))
    }
}

impl Parse for ItemConst {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let const_token: Token![const] = input.parse()?;

        let lookahead = input.lookahead1();
        let ident = if lookahead.peek_ident() || lookahead.peek(Token![_]) {
            input.call(Ident::parse_any)?
        } else {
            return Err(lookahead.error());
        };

        let colon_token: Token![:] = input.parse()?;
        let ty: Type = input.parse()?;
        let eq_token: Token![=] = input.parse()?;
        let expr: Expr = input.parse()?;
        let semi_token: Token![;] = input.parse()?;

        Ok(ItemConst {
            attrs,
            vis,
            const_token,
            ident,
            colon_token,
            ty: Box::new(ty),
            eq_token,
            expr: Box::new(expr),
            semi_token,
        })
    }
}

impl Parse for Ident {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse()?))
    }
}

impl Parse for Lit {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse()?))
    }
}

// This just exists because it is required for Attr
// to be a Node kind, but it should not really be
// used unless we explicitly try to match on Attr.
// To allow doing that, we probably need to introduce
// `OuterAttr` and `InnerAttr` or sth. like that.
impl Parse for Attribute {
    fn parse(input: ParseStream) -> Result<Self> {
        todo!()
    }
}
