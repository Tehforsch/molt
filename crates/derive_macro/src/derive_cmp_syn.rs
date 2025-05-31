use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident, Type, parse_macro_input};

use crate::utils::{is_box, is_node_id, is_node_list, is_token, is_vec_attribute};

pub fn impl_cmp_syn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let impl_ = match input.data {
        Data::Struct(data_struct) => impl_struct(data_struct),
        Data::Enum(data_enum) => impl_enum(data_enum),
        _ => panic!(),
    };

    let expanded = quote! {
        impl crate::cmp_syn::CmpSyn for #name {
            fn cmp_syn(&self, ctx: &mut crate::match_pattern::Match, pat: &Self) {
                #impl_
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn impl_struct(data_struct: syn::DataStruct) -> TokenStream {
    let calls = data_struct
        .fields
        .iter()
        .filter_map(|field| cmp_ty(field.ident.as_ref().unwrap(), &field.ty));
    quote! {
        #(#calls)*
    }
}

fn cmp_ty(field_name: &Ident, ty: &Type) -> Option<TokenStream> {
    if is_node_id(&ty) {
        Some(quote! { ctx.cmp_nodes(self. #field_name, pat. #field_name ); })
    } else if is_node_list(&ty) {
        Some(quote! { ctx.cmp_lists(self. #field_name, pat. #field_name ); })
    } else if is_vec_attribute(ty) {
        None
    } else if is_token(&ty) {
        None
    } else if is_box(&ty) {
        Some(quote! { ctx.cmp_syn(&*self. #field_name, &*pat. #field_name ); })
    } else {
        Some(quote! { ctx.cmp_syn(&self. #field_name, &pat. #field_name ); })
    }
}

fn impl_enum(data_enum: syn::DataEnum) -> TokenStream {
    let matches = data_enum
        .variants
        .into_iter()
        .map(|variant| {
            let Fields::Unnamed(fields) = variant.fields else {
                panic!()
            };
            assert_eq!(fields.unnamed.len(), 1);
            let ident = &variant.ident;
            quote! {
                (Self::#ident(item1), Self::#ident(item2)) => { ctx.cmp_syn(item1, item2) },
            }
        })
        .collect::<Vec<_>>();
    quote! {
        match (self, pat) {
            #(#matches)*
            _ => ctx.no_match(),
        }
    }
}
