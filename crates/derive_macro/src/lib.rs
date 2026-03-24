mod derive_cmp_syn;
mod derive_molt_fields;
mod utils;

#[proc_macro_derive(CmpSyn, attributes(rule, requires_rule, node))]
pub fn derive_cmp_syn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_cmp_syn::impl_cmp_syn(input)
}

#[proc_macro_derive(MoltFields)]
pub fn derive_molt_fields(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_molt_fields::impl_molt_fields(input)
}
