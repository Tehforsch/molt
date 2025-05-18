use std::collections::HashSet;

use syn::{
    braced,
    parse::{Parse, ParseStream},
    token::Brace,
    Ident, Result, Token,
};

use crate::{
    convert::Convert,
    grammar::{ItemConst, Kind},
    transformation::{Dependencies, ParseSynVarDecl, ParseTransform, SynVar},
};

impl Parse for ParseTransform {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut vars = vec![];
        while !input.is_empty() {
            vars.push(ParseSynVarDecl::parse(input)?);
        }
        Ok(ParseTransform { vars })
    }
}

impl Parse for ParseSynVarDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        let _ = input.parse::<Token![let]>();
        let name = input.parse()?;
        let _ = input.parse::<Token![:]>();
        let kind = input.parse::<Kind>()?;
        let node = if input.peek(Token![=]) {
            let _ = input.parse::<Token![=]>()?;
            let node;
            let _: Brace = braced!(node in input);
            Some(node.parse()?)
        } else {
            None
        };
        let _ = input.parse::<Token![;]>()?;
        Ok(ParseSynVarDecl { name, node, kind })
    }
}

impl Parse for SynVar {
    fn parse(input: ParseStream) -> Result<Self> {
        let _ = input.parse::<Token![$]>();
        let name: Ident = input.parse()?;
        Ok(SynVar {
            name: name.to_string(),
        })
    }
}

impl Parse for Dependencies {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut vars = HashSet::default();
        while !input.is_empty() {
            if input.peek(Token![$]) {
                let _ = input.parse::<Token![$]>();
                let name = input.parse()?;
                vars.insert(name);
            } else {
                // advance by one?
                input.step(|cursor| {
                    if let Some((_tt, next)) = cursor.token_tree() {
                        Ok(((), next)) // discard the token tree, advance cursor
                    } else {
                        Err(cursor.error("expected a token"))
                    }
                })?;
            }
        }
        Ok(Dependencies { vars })
    }
}

impl Parse for ItemConst {
    fn parse(input: ParseStream) -> Result<Self> {
        let const_ = syn::ItemConst::parse(input)?;
        Ok(const_.convert())
    }
}
