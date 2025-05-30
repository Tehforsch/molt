use crate::parser::{Parse, ParseStream, Parser};

use super::super::Result;
use super::{Attribute, Expr, Generics, ItemConst, RustFile, Type, Visibility};

impl Parse for RustFile {
    fn parse(parser: &Parser) -> Result<Self> {
        todo!()
        // Ok(Self)
    }
}

impl Parse for ItemConst {
    fn parse(input: ParseStream) -> Result<Self> {
        todo!()
        // // let attrs = input.call(Attribute::parse_outer)?;
        // let vis: Visibility = input.parse()?;
        // let const_token: Token![const] = input.parse()?;

        // let lookahead = input.lookahead1();
        // let ident = if lookahead.peek::<Ident>() || lookahead.peek::<Token![_]>() {
        //     todo!()
        //     // input.call(Ident::parse)?
        // } else {
        //     todo!()
        //     // return Err(lookahead.error());
        // };

        // let colon_token: Token![:] = input.parse()?;
        // let ty: Type = input.parse()?;
        // let eq_token: Token![=] = input.parse()?;
        // let expr: Expr = input.parse()?;
        // let semi_token: Token![;] = input.parse()?;

        // Ok(ItemConst {
        //     attrs: todo!(),
        //     vis,
        //     const_token,
        //     ident,
        //     colon_token,
        //     ty: Box::new(ty),
        //     eq_token,
        //     expr: Box::new(expr),
        //     semi_token,
        // })
    }
}
