use derive_macro::CmpSyn;

use crate::parser::error::Result;
use crate::parser::parse::discouraged::Speculative as _;
use crate::parser::parse::{ParseNode, ParseStream};
use crate::parser::token;
use crate::rust_grammar::ident::AnyIdent;
use crate::rust_grammar::path::Path;

#[derive(Debug, CmpSyn)]
/// The visibility level of an item: inherited or `pub` or
/// `pub(restricted)`.
#[requires_rule]
pub enum Vis {
    /// A public visibility level: `pub`.
    Public(Token![pub]),

    /// A visibility level restricted to some path: `pub(self)` or
    /// `pub(super)` or `pub(crate)` or `pub(in some::module)`.
    Restricted(VisRestricted),

    /// An inherited visibility, which usually means private.
    Inherited,
}

#[derive(Debug, CmpSyn)]
/// A visibility level restricted to some path: `pub(self)` or
/// `pub(super)` or `pub(crate)` or `pub(in some::module)`.
pub struct VisRestricted {
    pub pub_token: Token![pub],
    pub paren_token: token::Paren,
    pub in_token: Option<Token![in]>,
    pub path: Box<Path>,
}

#[derive(Debug, CmpSyn)]
/// Unused, but reserved for RFC 3323 restrictions.
pub enum FieldMutability {
    None,
    // TODO: https://rust-lang.github.io/rfcs/3323-restrictions.html
    //
    // FieldMutability::Restricted(MutRestricted)
    //
    // pub struct MutRestricted {
    //     pub mut_token: Token![mut],
    //     pub paren_token: token::Paren,
    //     pub in_token: Option<Token![in]>,
    //     pub path: Box<Path>,
    // }
}

impl ParseNode for Vis {
    type Target = Self;

    fn parse_node(input: ParseStream) -> Result<Self::Target> {
        // Recognize an empty None-delimited group, as produced by a $:vis
        // matcher that matched no tokens.
        if input.peek(token::Group) {
            let ahead = input.fork();
            let group = crate::parser::group::parse_group(&ahead)?;
            if group.content.is_empty() {
                input.advance_to(&ahead);
                return Ok(Vis::Inherited);
            }
        }

        if input.peek(Token![pub]) {
            Self::parse_pub(input)
        } else {
            Ok(Vis::Inherited)
        }
    }
}

impl Vis {
    fn parse_pub(input: ParseStream) -> Result<Self> {
        let pub_token = input.parse::<Token![pub]>()?;

        if input.peek(token::Paren) {
            let ahead = input.fork();

            let content;
            let paren_token = parenthesized!(content in ahead);
            if content.peek(Token![crate])
                || content.peek(Token![self])
                || content.peek(Token![super])
            {
                let path = content.parse_id::<AnyIdent>()?;

                // Ensure there are no additional tokens within `content`.
                // Without explicitly checking, we may misinterpret a tuple
                // field as a restricted visibility, causing a parse error.
                // e.g. `pub (crate::rust_grammar::A, crate::rust_grammar::B)` (Issue #720).
                if content.is_empty() {
                    input.advance_to(&ahead);
                    return Ok(Vis::Restricted(VisRestricted {
                        pub_token,
                        paren_token,
                        in_token: None,
                        path: Box::new(Path::from(path)),
                    }));
                }
            } else if content.peek(Token![in]) {
                let in_token: Token![in] = content.parse()?;
                let path = content.call(Path::parse_mod_style)?;

                input.advance_to(&ahead);
                return Ok(Vis::Restricted(VisRestricted {
                    pub_token,
                    paren_token,
                    in_token: Some(in_token),
                    path: Box::new(path),
                }));
            }
        }

        Ok(Vis::Public(pub_token))
    }

    pub(crate) fn is_some(&self) -> bool {
        !matches!(self, Vis::Inherited)
    }
}
