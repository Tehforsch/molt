use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Type, parse_macro_input};

use crate::utils::{is_node_id, type_path_matches};

pub fn impl_get_dependencies(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let impl_ = match input.data {
        Data::Struct(data_struct) => impl_struct(data_struct),
        Data::Enum(data_enum) => impl_enum(data_enum),
        _ => panic!(),
    };

    let expanded = quote! {
        impl molt_lib::GetDependencies<crate::Node> for #name {
            fn get_dependencies(&self, ctx: &molt_lib::Ctx<crate::Node>, deps: &mut molt_lib::Dependencies) {
                #impl_
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn impl_struct(data_struct: syn::DataStruct) -> TokenStream {
    let calls = data_struct.fields.iter().filter_map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        if is_node_id(&field.ty) {
            Some(quote! {
                self. #field_name .get_dependencies(ctx, deps);
            })
        } else {
            None
        }
    });
    quote! {
        #(#calls)*
    }
}

fn impl_enum(data_enum: syn::DataEnum) -> TokenStream {
    if data_enum.variants.len() == 0 {
        return quote! {};
    }
    let matches = data_enum
        .variants
        .into_iter()
        .map(|variant| {
            let ident = &variant.ident;
            match &variant.fields {
                Fields::Named(fields) => {
                    assert_eq!(fields.named.len(), 1);
                    let field_name = &fields.named[0].ident;
                    quote! {
                        Self::#ident { #field_name: s } => { s.get_dependencies(ctx) },
                    }
                }
                Fields::Unnamed(fields) => {
                    // Too lazy to write this for generically many arguments
                    if fields.unnamed.len() == 1 {
                        if is_foreign_type(&fields.unnamed[0].ty) {
                            quote! {
                                Self::#ident(item) => {}
                            }
                        } else {
                            quote! {
                                Self::#ident(item) => { item.get_dependencies(ctx, deps); },
                            }
                        }
                    } else if fields.unnamed.len() == 2 {
                        assert!(!is_foreign_type(&fields.unnamed[0].ty));
                        assert!(!is_foreign_type(&fields.unnamed[1].ty));
                        quote! {
                            Self::#ident(s1, s2) => {
                                s1.get_dependencies(ctx, deps);
                                s2.get_dependencies(ctx, deps);
                            },
                        }
                    } else {
                        panic!("Too many items")
                    }
                }
                Fields::Unit => {
                    quote! {
                        Self::#ident => {},
                    }
                }
            }
        })
        .collect::<Vec<_>>();
    quote! {
        match self {
            #(#matches)*
        }
    }
}

fn is_foreign_type(ty: &Type) -> bool {
    type_path_matches(ty, "TokenStream") || type_path_matches(ty, "Literal")
}
