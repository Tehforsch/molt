use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{Ident, Lit, LitStr, Token, Visibility};

use crate::{
    grammar::{self, Kind, Pattern},
    transformation::SynVar,
};

pub(crate) const MANGLE_STR: &str = "__mangle";

trait Mangle: ToTokens {
    fn from_mangled_str(name: &str) -> impl ToTokens;
}

pub(crate) fn mangle_str(ident: &str) -> String {
    format!("{}_{}", MANGLE_STR, ident.to_string())
}

pub(crate) fn mangle(name: &str, kind: Kind) -> TokenStream {
    let mangled_str = mangle_str(name);
    let mut stream = TokenStream::new();
    match kind {
        Kind::Const => syn::ItemConst::from_mangled_str(&mangled_str).to_tokens(&mut stream),
        Kind::Ident => syn::Ident::from_mangled_str(&mangled_str).to_tokens(&mut stream),
        Kind::Expr => syn::Expr::from_mangled_str(&mangled_str).to_tokens(&mut stream),
    }
    stream
}

fn fake_span() -> Span {
    // This is very very ugly and inefficient, but it seems like
    // a simple solution for now.
    let ts = quote! { "hello" };
    ts.into_iter().next().unwrap().span()
}

impl Mangle for grammar::Ident {
    fn from_mangled_str(name: &str) -> impl ToTokens {
        grammar::Ident::new(name, fake_span())
    }
}

impl Mangle for syn::Expr {
    fn from_mangled_str(name: &str) -> impl ToTokens {
        let lit = Lit::Str(LitStr::new(name, fake_span()));
        quote! { #lit }
    }
}

impl Mangle for syn::ItemConst {
    fn from_mangled_str(name: &str) -> impl ToTokens {
        let ident = syn::Ident::new(name, fake_span());
        quote! { const #ident: usize = 0; }
    }
}

pub(crate) trait Unmangle: Sized {
    fn mangled_str(&self) -> Option<String>;

    fn unmangle(self) -> Pattern<Self> {
        let name = self.mangled_str().and_then(|s| {
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

impl Unmangle for syn::Ident {
    fn mangled_str(&self) -> Option<String> {
        Some(self.to_string())
    }
}

impl Unmangle for syn::ItemConst {
    fn mangled_str(&self) -> Option<String> {
        Some(self.ident.to_string())
    }
}

impl Unmangle for syn::Expr {
    fn mangled_str(&self) -> Option<String> {
        if let syn::Expr::Lit(lit) = self {
            if let syn::Lit::Str(lit_str) = &lit.lit {
                return Some(lit_str.value());
            }
        }
        None
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
            dbg!(kind);
            assert!(unmangles_as_pattern(mangled, kind));
        }
    }
}
