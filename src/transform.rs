use std::path::Path;

use syn::{Data, File};

use crate::grammar::{Item, ItemFn, PatternAst, Signature};

pub const SEPARATOR: &'static str = "==========";
pub const IDENT_IDENTIFIER: char = '$';

pub struct Transform {
    pub input: PatternAst,
    pub output: PatternAst,
}

impl Transform {
    pub(crate) fn from_path(path: &Path) -> Self {
        // This is ugly, but might be a compromise at first.
        let contents = std::fs::read_to_string(path).unwrap();
        let split: Vec<_> = contents.split(SEPARATOR).collect();
        if split.len() != 2 {
            panic!(
                "More than one separator ({}) found in transform file",
                SEPARATOR
            );
        }
        let input = PatternAst::from_str(split[0]);
        let output = PatternAst::from_str(split[1]);
        Transform { input, output }
    }
}

pub trait Tf<Data> {
    fn tf(self, input: &Data, output: &Data) -> Self;
}

impl Tf<Item> for syn::File {
    fn tf(self, input: &Item, output: &Item) -> Self {
        let items = self
            .items
            .into_iter()
            .map(|item| item.tf(&input, &output))
            .collect();
        File {
            items,
            attrs: self.attrs,
            shebang: self.shebang,
        }
    }
}

impl Tf<Item> for syn::Item {
    fn tf(self, input: &Item, output: &Item) -> Self {
        match self {
            syn::Item::Fn(item_fn) => {
                if let Item::Fn(input) = input {
                    if let Item::Fn(output) = output {
                        return syn::Item::Fn(item_fn.tf(input, output));
                    }
                }
                syn::Item::Fn(item_fn)
            }
            _ => todo!(),
        }
    }
}

impl Tf<ItemFn> for syn::ItemFn {
    fn tf(self, input: &ItemFn, output: &ItemFn) -> Self {
        syn::ItemFn {
            attrs: self.attrs,
            vis: self.vis,
            sig: self.sig.tf(&input.sig, &output.sig),
            block: self.block,
        }
    }
}

fn change_option<T>(opt: Option<T>, input_opt: Option<T>, output_opt: Option<T>) -> Option<T> {
    if opt.is_some() == input_opt.is_some() {
        output_opt
    } else {
        opt
    }
}

impl Tf<Signature> for syn::Signature {
    fn tf(self, input: &Signature, output: &Signature) -> Self {
        let constness = change_option(self.constness, input.constness, output.constness);
        let asyncness = change_option(self.asyncness, input.asyncness, output.asyncness);
        let unsafety = change_option(self.unsafety, input.unsafety, output.unsafety);
        syn::Signature {
            constness,
            asyncness,
            unsafety,
            abi: self.abi,
            fn_token: self.fn_token,
            ident: self.ident,
            generics: self.generics,
            paren_token: self.paren_token,
            inputs: self.inputs,
            variadic: self.variadic,
            output: self.output,
        }
    }
}
