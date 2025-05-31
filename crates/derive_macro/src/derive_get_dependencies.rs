use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Type, parse_macro_input};

pub fn impl_get_dependencies(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let impl_ = match input.data {
        Data::Struct(data_struct) => impl_struct(data_struct),
        Data::Enum(data_enum) => impl_enum(data_enum),
        _ => panic!(),
    };

    let expanded = quote! {
        impl crate::resolve::GetDependencies for #name {
            fn get_dependencies(&self, ctx: &crate::ctx::PatCtx, deps: &mut crate::resolve::Dependencies) {
                #impl_
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn impl_struct(data_struct: syn::DataStruct) -> TokenStream {
    let calls = data_struct.fields.iter().filter_map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        match &field.ty {
            Type::Path(type_path) => {
                let seg = &type_path.path.segments.last().unwrap().ident;
                if seg == "NodeId" {
                    Some(quote! {
                        self. #field_name .get_dependencies(ctx, deps);
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    });
    quote! {
        #(#calls)*
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
                Self::#ident(item) => { item.get_dependencies(ctx, deps); },
            }
        })
        .collect::<Vec<_>>();
    quote! {
        match self {
            #(#matches)*
        }
    }
}
