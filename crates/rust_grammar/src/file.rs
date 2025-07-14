use molt_lib::{Ctx, NodeList, ParsingMode};

use crate::attr::Attribute;
use crate::error::Result;
use crate::item::{Item, Items};
use crate::parse::{Parse, ParseStream, parse_str_ctx};
use crate::{Node, whitespace};

#[derive(Debug)]
/// A complete file of Rust source code.
pub struct File {
    pub shebang: Option<String>,
    pub attrs: Vec<Attribute>,
    pub items: NodeList<Item, Token![;]>,
}

impl Parse for File {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(File {
            shebang: None,
            attrs: input.call(Attribute::parse_inner)?,
            items: input.parse_list::<Items>()?,
        })
    }
}

pub fn parse_file(mut content: &str, mode: ParsingMode) -> Result<(File, Ctx<Node>)> {
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
