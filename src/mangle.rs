use syn::Ident;

use crate::{
    grammar::{Kind, Pattern},
    transformation::SynVar,
};

pub(crate) const MANGLE_STR: &str = "__mangle_";

pub(crate) fn mangle_ident(ident: Ident, kind: Kind) -> Ident {
    let name = format!("{}{}{}", MANGLE_STR, kind.to_str(), ident.to_string());
    syn::Ident::new(&name, ident.span())
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
