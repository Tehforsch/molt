use crate::parser::parse_str;
use crate::{Mode, NodeList};

use crate::parser::error::Result;
use crate::parser::parse::{Parse, ParseResult, ParseStream};
use crate::rust_grammar::attr::Attribute;
use crate::rust_grammar::item::{Item, Items};
use crate::rust_grammar::whitespace;

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

pub fn parse_file(mut content: &str, mode: Mode) -> Result<ParseResult<File>> {
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

    let mut result = parse_str::<File>(content, mode)?;
    result.item_mut().shebang = shebang;
    Ok(result)
}
