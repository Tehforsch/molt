use super::{ast::unmangle, Ast, Ident, Item, ItemFn, PatternAst, Signature};

pub(crate) trait Convert {
    type Output;

    fn convert(self) -> Self::Output;
}

impl Convert for Ast {
    type Output = PatternAst;

    fn convert(self) -> Self::Output {
        PatternAst {
            items: self
                .file
                .items
                .into_iter()
                .map(|item| item.convert())
                .collect(),
        }
    }
}

impl Convert for syn::Item {
    type Output = Item;

    fn convert(self) -> Self::Output {
        match self {
            syn::Item::Fn(item_fn) => Item::Fn(item_fn.convert()),
            _ => todo!(),
        }
    }
}

impl Convert for syn::ItemFn {
    type Output = ItemFn;

    fn convert(self) -> Self::Output {
        ItemFn {
            attrs: self.attrs,
            vis: self.vis,
            sig: self.sig.convert(),
            block: self.block,
        }
    }
}

impl Convert for syn::Signature {
    type Output = Signature;

    fn convert(self) -> Self::Output {
        Signature {
            constness: self.constness,
            asyncness: self.asyncness,
            unsafety: self.unsafety,
            abi: self.abi,
            fn_token: self.fn_token,
            ident: self.ident.convert(),
            generics: self.generics,
            paren_token: self.paren_token,
            inputs: self.inputs,
            variadic: self.variadic,
            output: self.output,
        }
    }
}

impl Convert for syn::Ident {
    type Output = Ident;

    fn convert(self) -> Self::Output {
        unmangle(self)
    }
}
