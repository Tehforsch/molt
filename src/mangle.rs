use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{Lit, LitStr};

use crate::{
    grammar::{self, GetKind, Kind},
    spec::SynVar,
};

pub(crate) const MANGLE_STR: &str = "__mangle";

#[derive(Clone)]
pub(crate) enum Pattern<E> {
    Exact(E),
    Pattern(SynVar),
}

pub(crate) trait ToPlaceholderTokens: ToTokens {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens;
}

pub(crate) fn mangle_str(ident: &str, kind: Kind) -> String {
    format!(
        "{}_{}_{}",
        MANGLE_STR,
        ident.to_string(),
        format!("{:?}", kind)
    )
}

fn unmangle(name: &str, desired_kind: Kind) -> Option<String> {
    let name = name.replacen(&format!("{}_", MANGLE_STR), "", 1);
    let split: Vec<_> = name.split("_").collect();
    let name = split[0];
    let kind = Kind::from_str(split[1]);
    if desired_kind == kind {
        Some(name.to_string())
    } else {
        None
    }
}

pub(crate) fn mangle(name: &str, kind: Kind) -> TokenStream {
    let mangled_str = mangle_str(name, kind);
    let t = kind.to_placeholder_tokens(mangled_str);
    t
}

fn fake_span() -> Span {
    Span::call_site()
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

impl ToPlaceholderTokens for syn::Lit {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let lit = Lit::Str(LitStr::new(name, fake_span()));
        quote! { #lit }
    }
}

impl ToPlaceholderTokens for syn::ItemFn {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let ident = syn::Ident::new(name, fake_span());
        quote! { fn #ident() {} }
    }
}

impl ToPlaceholderTokens for syn::Signature {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let ident = syn::Ident::new(name, fake_span());
        quote! { fn #ident() }
    }
}

impl ToPlaceholderTokens for syn::FnArg {
    fn to_placeholder_tokens(name: &str) -> impl ToTokens {
        let ident = syn::Ident::new(name, fake_span());
        quote! { #ident: Foo }
    }
}

pub(crate) trait FromPlaceholder: Sized + GetKind {
    fn get_mangled_str(&self) -> Option<String>;

    fn from_placeholder(self) -> Pattern<Self> {
        let name = self.get_mangled_str().and_then(|s| {
            if s.starts_with(MANGLE_STR) {
                unmangle(&s, Self::get_kind())
            } else {
                None
            }
        });
        if let Some(name) = name {
            Pattern::Pattern(SynVar { name })
        } else {
            Pattern::Exact(self)
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
            Some(const_item.ident.to_string())
        } else {
            None
        }
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

impl FromPlaceholder for syn::Signature {
    fn get_mangled_str(&self) -> Option<String> {
        Some(self.ident.to_string())
    }
}

impl FromPlaceholder for syn::FnArg {
    fn get_mangled_str(&self) -> Option<String> {
        match self {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => match &*pat_type.pat {
                syn::Pat::Ident(pat_ident) => Some(pat_ident.ident.to_string()),
                _ => None,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::grammar::{unmangle_pattern_var_name, Kind};

    use super::mangle;

    #[test]
    fn bijective() {
        for kind in Kind::all_kinds() {
            let mangled = mangle("foo", kind);
            assert_eq!(
                unmangle_pattern_var_name(mangled, kind),
                Some("foo".to_string()),
                "{:?}",
                kind
            );
        }
    }
}
