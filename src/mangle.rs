use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{Lit, LitStr};

use crate::{
    grammar::{self, Kind},
    spec::SynVar,
};

pub(crate) const MANGLE_STR: &str = "__mangle";

#[derive(Clone)]
pub(crate) enum Pattern<E> {
    Exact(Box<E>),
    Pattern(SynVar),
}

pub(crate) trait ToPlaceholderTokens: ToTokens {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens;
}

pub(crate) fn mangle_str(ident: &str) -> String {
    format!("{}_{}", MANGLE_STR, ident.to_string())
}

pub(crate) fn mangle(name: &str, kind: Kind) -> TokenStream {
    let mangled_str = mangle_str(name);
    kind.to_placeholder_tokens(mangled_str)
}

fn fake_span() -> Span {
    // This is very very ugly and inefficient, but it seems like
    // a simple solution for now.
    let ts = quote! { "hello" };
    ts.into_iter().next().unwrap().span()
}

impl ToPlaceholderTokens for grammar::Ident {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        grammar::Ident::new(name, fake_span())
    }
}

impl ToPlaceholderTokens for syn::Item {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let ident = syn::Ident::new(name, fake_span());
        quote! { const #ident: usize = 0; }
    }
}

impl ToPlaceholderTokens for syn::Expr {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let lit = Lit::Str(LitStr::new(name, fake_span()));
        quote! { #lit }
    }
}

impl ToPlaceholderTokens for syn::ItemConst {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let ident = syn::Ident::new(name, fake_span());
        quote! { const #ident: usize = 0; }
    }
}

impl ToPlaceholderTokens for syn::Lit {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let lit = Lit::Str(LitStr::new(name, fake_span()));
        quote! { #lit }
    }
}

impl ToPlaceholderTokens for syn::ExprUnary {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let expr = syn::Expr::to_placeholder_tokens(name);
        quote! {
            !#expr
        }
    }
}

impl ToPlaceholderTokens for syn::ExprBinary {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let expr = syn::Expr::to_placeholder_tokens(name);
        quote! {
            #expr + #expr
        }
    }
}

impl ToPlaceholderTokens for syn::ExprLit {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let lit = syn::Lit::to_placeholder_tokens(name);
        quote! {
            #lit
        }
    }
}

pub(crate) trait FromPlaceholder: Sized {
    fn get_mangled_str(&self) -> Option<String>;

    fn from_placeholder(self) -> Pattern<Self> {
        let name = self.get_mangled_str().and_then(|s| {
            if s.starts_with(MANGLE_STR) {
                Some(s.replacen(MANGLE_STR, "", 1).to_string())
            } else {
                None
            }
        });
        if let Some(name) = name {
            Pattern::Pattern(SynVar { name })
        } else {
            Pattern::Exact(Box::new(self))
        }
    }
}

impl FromPlaceholder for syn::Ident {
    fn get_mangled_str(&self) -> Option<String> {
        Some(self.to_string())
    }
}

impl FromPlaceholder for syn::Lit {
    fn get_mangled_str(&self) -> Option<String> {
        if let syn::Lit::Str(lit_str) = &self {
            return Some(lit_str.value());
        } else {
            None
        }
    }
}

impl FromPlaceholder for syn::Item {
    fn get_mangled_str(&self) -> Option<String> {
        if let syn::Item::Const(const_item) = self {
            syn::ItemConst::get_mangled_str(const_item)
        } else {
            None
        }
    }
}

impl FromPlaceholder for syn::ItemConst {
    fn get_mangled_str(&self) -> Option<String> {
        Some(self.ident.to_string())
    }
}

impl FromPlaceholder for syn::Expr {
    fn get_mangled_str(&self) -> Option<String> {
        if let syn::Expr::Lit(lit) = self {
            return lit.lit.get_mangled_str();
        }
        None
    }
}

impl FromPlaceholder for syn::ExprUnary {
    fn get_mangled_str(&self) -> Option<String> {
        self.expr.get_mangled_str()
    }
}

impl FromPlaceholder for syn::ExprBinary {
    fn get_mangled_str(&self) -> Option<String> {
        self.left.get_mangled_str()
    }
}

impl FromPlaceholder for syn::ExprLit {
    fn get_mangled_str(&self) -> Option<String> {
        self.lit.get_mangled_str()
    }
}

#[cfg(test)]
mod tests {
    use crate::grammar::{unmangles_as_pattern, Kind};

    use super::mangle;

    #[test]
    fn bijective() {
        for kind in Kind::all_kinds() {
            let mangled = mangle("foo", kind);
            assert!(unmangles_as_pattern(mangled, kind));
        }
    }
}
