use std::str::FromStr;

use proc_macro2::{Group, TokenStream, TokenTree};
use quote::TokenStreamExt;
use syn::{File, Ident};

use crate::grammar;
use crate::grammar::convert::Convert;
use crate::transform::{Tf, IDENT_IDENTIFIER};
use crate::{grammar::Item, transform::Transform};

/// Represents a real Rust AST.
pub struct Ast {
    pub file: File,
}

impl Ast {
    pub fn new(file: File) -> Self {
        Self { file }
    }

    pub fn apply_transform(self, mut t: Transform) -> Self {
        let input = t.input.items.pop().unwrap();
        let output = t.output.items.pop().unwrap();
        assert!(t.input.items.is_empty());
        assert!(t.output.items.is_empty());
        Ast {
            file: self.file.tf(&input, &output),
        }
    }

    pub fn dump(self) -> String {
        prettyplease::unparse(&self.file)
    }
}

/// Represents a Rust AST with optional
/// pattern identifiers
pub struct PatternAst {
    pub items: Vec<Item>,
}

impl PatternAst {
    pub(crate) fn from_str(s: &str) -> PatternAst {
        // TODO: dont unwrap
        let tokens = TokenStream::from_str(s).unwrap();
        let tokens = annotate_identifiers(tokens);
        let file: File = syn::parse2::<File>(tokens).unwrap();
        Ast { file }.convert()
    }
}

fn annotate_identifiers(tokens: TokenStream) -> TokenStream {
    let mut new = TokenStream::new();
    let mut token_iter = tokens.into_iter();
    while let Some(token) = token_iter.next() {
        match token {
            TokenTree::Group(group) => new.append(Group::new(
                group.delimiter(),
                annotate_identifiers(group.stream()),
            )),
            TokenTree::Punct(punct) => {
                if punct.as_char() == IDENT_IDENTIFIER {
                    if let Some(TokenTree::Ident(ident)) = token_iter.next() {
                        new.append(mangle(ident));
                    } else {
                        panic!("{} not followed by identifier.", IDENT_IDENTIFIER)
                    }
                }
            }
            rest => new.append(rest),
        }
    }
    new
}

const MANGLE_STR: &str = "__mangle_";

fn mangle(ident: Ident) -> Ident {
    let name = format!("{}{}", MANGLE_STR, ident.to_string());
    Ident::new(&name, ident.span())
}

pub fn unmangle(ident: Ident) -> grammar::Ident {
    let s = ident.to_string();
    if s.starts_with(MANGLE_STR) {
        grammar::Ident::Pattern(s.replacen(MANGLE_STR, "", 1).to_string())
    } else {
        grammar::Ident::Exact(ident)
    }
}
