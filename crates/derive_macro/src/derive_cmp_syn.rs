use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Type, parse_macro_input};

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
    todo!()
    // let calls = data_struct.fields.iter().filter_map(|field| {
    //     let field_name = field.ident.as_ref().unwrap();
    //     match &field.ty {
    //         Type::Path(type_path) => {
    //             let seg = &type_path.path.segments.last().unwrap().ident;
    //             if seg == "NodeId" {
    //                 Some(quote! {
    //                     self. #field_name .get_dependencies(ctx, deps);
    //                 })
    //             } else {
    //                 None
    //             }
    //         }
    //         _ => None,
    //     }
    // });
    // quote! {
    //     #(#calls)*
    // }
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
