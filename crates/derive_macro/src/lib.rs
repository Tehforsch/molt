mod derive_get_dependencies;

#[proc_macro_derive(GetDependencies)]
pub fn derive_get_dependencies(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_get_dependencies::impl_get_dependencies(input)
}
