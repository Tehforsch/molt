use crate::{Ctx, Mode, NodeList};

use crate::parser::error::Result;
use crate::parser::parse::{Parse, ParseStream, parse_str_ctx};
use crate::rust_grammar::attr::Attribute;
use crate::rust_grammar::item::{Item, Items};
use crate::rust_grammar::{Node, whitespace};

#[derive(Debug)]
/// A complete file of Rust source code.
pub struct File {
    pub shebang: Option<String>,
    pub _attrs: Vec<Attribute>,
    pub _items: NodeList<Item, Token![;]>,
}

impl Parse for File {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(File {
            shebang: None,
            _attrs: input.call(Attribute::parse_inner)?,
            _items: input.parse_list::<Items>()?,
        })
    }
}

pub fn parse_file(mut content: &str, mode: Mode) -> Result<(File, Ctx<Node>)> {
    // Strip the BOM if it is present
    const BOM: &str = "\u{feff}";
    if content.starts_with(BOM) {
        content = &content[BOM.len()..];
    }

    let mut shebang = None;
    if content.starts_with("#!") {
        let rest = whitespace::skip(&content[2..]);
        if !rest.starts_with('[') {
            if let Some(idx) = content.find('\n') {
                shebang = Some(content[..idx].to_string());
                content = &content[idx..];
            } else {
                shebang = Some(content.to_string());
                content = "";
            }
        }
    }

    let (mut file, ctx): (File, _) = parse_str_ctx(content, mode)?;
    file.shebang = shebang;
    Ok((file, ctx))
}
