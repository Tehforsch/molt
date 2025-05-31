mod derive_cmp_syn;
mod derive_get_dependencies;
mod utils;

#[proc_macro_derive(GetDependencies)]
pub fn derive_get_dependencies(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_get_dependencies::impl_get_dependencies(input)
}

#[proc_macro_derive(CmpSyn)]
pub fn derive_cmp_syn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_cmp_syn::impl_cmp_syn(input)
}
