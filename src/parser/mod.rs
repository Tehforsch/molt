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
    transformation::{Command, Dependencies, ParseSynVarDecl, ParseTransform, SynVar},
};

pub(crate) mod commands {
    syn::custom_keyword!(transform);
}

enum Declaration {
    SynVarDecl(ParseSynVarDecl),
    Command(Command),
}

impl Parse for ParseTransform {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut decls = vec![];
        while !input.is_empty() {
            decls.push(Declaration::parse(input)?);
        }
        let mut commands = vec![];
        let mut vars = vec![];
        for decl in decls.into_iter() {
            match decl {
                Declaration::SynVarDecl(var) => vars.push(var),
                Declaration::Command(command) => commands.push(command),
            }
        }
        let command = if commands.len() == 0 {
            // Not the greatest error span
            return Err(syn::Error::new(input.cursor().span(), "No command given."));
        } else if commands.len() > 1 {
            // Not the greatest error span
            return Err(syn::Error::new(
                input.cursor().span(),
                "Multiple commands given.",
            ));
        } else {
            commands.remove(0)
        };
        Ok(ParseTransform { vars, command })
    }
}

impl Parse for Declaration {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(commands::transform) || input.peek(Token![match]) {
            Ok(Self::Command(input.parse()?))
        } else {
            Ok(Self::SynVarDecl(input.parse()?))
        }
    }
}

impl Parse for Command {
    fn parse(input: ParseStream) -> Result<Self> {
        let command = if input.peek(commands::transform) {
            let _: commands::transform = input.parse()?;
            let input_var: SynVar = input.parse()?;
            let _: Token![-] = input.parse()?;
            let _: Token![>] = input.parse()?;
            let output_var: SynVar = input.parse()?;
            Command::Transform(input_var, output_var)
        } else {
            let _: Token![match] = input.parse()?;
            let match_var: SynVar = input.parse()?;
            Command::Match(match_var)
        };
        let _: Token![;] = input.parse()?;
        Ok(command)
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
