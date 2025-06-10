use derive_macro::CmpSyn;
use molt_lib::{NoPunct, NodeId, NodeList, ParsingMode};

use crate::attr::Attribute;
use crate::error::Result;
use crate::item::Item;
use crate::{parse_str, whitespace};

#[derive(Debug, CmpSyn)]
/// A complete file of Rust source code.
///
/// Typically `File` objects are created with [`parse_file`].
///
/// [`parse_file`]: crate::parse_file
///
/// # Example
///
/// Parse a Rust source file into a `syn::File` and print out a debug
/// representation of the syntax tree.
///
/// ```
/// use std::env;
/// use std::fs;
/// use std::process;
///
/// fn main() {
/// # }
/// #
/// # fn fake_main() {
///     let mut args = env::args();
///     let _ = args.next(); // executable name
///
///     let filename = match (args.next(), args.next()) {
///         (Some(filename), None) => filename,
///         _ => {
///             eprintln!("Usage: dump-syntax path/to/filename.rs");
///             process::exit(1);
///         }
///     };
///
///     let src = fs::read_to_string(&filename).expect("unable to read file");
///     let syntax = syn::parse_file(&src).expect("unable to parse file");
///
///     // Debug impl is available if Syn is built with "extra-traits" feature.
///     println!("{:#?}", syntax);
/// }
/// ```
///
/// Running with its own source code as input, this program prints output
/// that begins with:
///
/// ```text
/// File {
///     shebang: None,
///     attrs: [],
///     items: [
///         Use(
///             ItemUse {
///                 attrs: [],
///                 vis: Inherited,
///                 use_token: Use,
///                 leading_colon: None,
///                 tree: Path(
///                     UsePath {
///                         ident: Ident(
///                             std,
///                         ),
///                         colon2_token: Colon2,
///                         tree: Name(
///                             UseName {
///                                 ident: Ident(
///                                     env,
///                                 ),
///                             },
///                         ),
///                     },
///                 ),
///                 semi_token: Semi,
///             },
///         ),
/// ...
/// ```
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct File {
    pub shebang: Option<String>,
    pub attrs: Vec<Attribute>,
    pub items: Vec<NodeId<Item>>,
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    use molt_lib::NodeId;

    use crate::attr::Attribute;
    use crate::error::Result;
    use crate::file::File;
    use crate::parse::{Parse, ParseStream};
    use crate::Item;

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for File {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(File {
                shebang: None,
                attrs: input.call(Attribute::parse_inner)?,
                items: {
                    let mut items: Vec<NodeId<Item>> = Vec::new();
                    while !input.is_empty() {
                        items.push(input.parse()?);
                    }
                    items
                },
            })
        }
    }
}

pub fn parse_file(mut content: &str, mode: ParsingMode) -> Result<File> {
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

    let mut file: File = parse_str(content, mode)?;
    file.shebang = shebang;
    Ok(file)
}

#[cfg(feature = "printing")]
mod printing {
    use crate::attr::FilterAttrs;
    use crate::file::File;
    use proc_macro2::TokenStream;
    use quote::{ToTokens, TokenStreamExt};

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for File {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.inner());
            tokens.append_all(&self.items);
        }
    }
}
