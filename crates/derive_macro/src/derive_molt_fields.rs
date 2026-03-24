use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, PathArguments, Type};

pub fn impl_molt_fields(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let field_adds = match &input.data {
        Data::Struct(data) => generate_field_adds(&data.fields),
        Data::Enum(_) => Vec::new(),
        Data::Union(_) => panic!("MoltFields cannot be derived for unions"),
    };

    let expanded = quote! {
        impl #impl_generics crate::molt_lang::MoltFields for #name #ty_generics #where_clause {
            type Target=Self;
            fn add_fields(builder: &mut crate::molt_lang::FieldDefBuilder) {
                #(#field_adds)*
            }
        }
    };

    expanded.into()
}

fn generate_field_adds(fields: &Fields) -> Vec<proc_macro2::TokenStream> {
    let Fields::Named(fields) = fields else {
        return Vec::new();
    };

    fields
        .named
        .iter()
        .filter_map(|field| {
            if is_node_id_type(&field.ty) {
                let field_name = field.ident.as_ref()?;
                let field_name_str = field_name.to_string();
                Some(quote! {
                    builder.add(#field_name_str, |f: &Self| f.#field_name);
                })
            } else {
                None
            }
        })
        .collect()
}

fn is_node_id_type(ty: &Type) -> bool {
    let Type::Path(type_path) = ty else {
        return false;
    };
    let Some(last_segment) = type_path.path.segments.last() else {
        return false;
    };
    last_segment.ident == "NodeId"
        && matches!(last_segment.arguments, PathArguments::AngleBracketed(_))
}
