mod derive_cmp_syn;
mod utils;

#[proc_macro_derive(CmpSyn)]
pub fn derive_cmp_syn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_cmp_syn::impl_cmp_syn(input)
}
