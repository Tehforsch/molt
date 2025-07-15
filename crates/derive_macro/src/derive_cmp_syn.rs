use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident, Type, parse_macro_input};

use crate::utils::{is_box, is_node_list, is_token, is_vec_attribute};

pub fn impl_cmp_syn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let impl_ = match input.data {
        Data::Struct(data_struct) => impl_struct(data_struct),
        Data::Enum(data_enum) => impl_enum(data_enum),
        _ => panic!("Union found"),
    };

    let expanded = quote! {
        impl molt_lib::CmpSyn for #name {
            fn cmp_syn(&self, ctx: &mut molt_lib::Matcher, pat: &Self) {
                #impl_
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn impl_struct(data_struct: syn::DataStruct) -> TokenStream {
    let calls = data_struct.fields.iter().filter_map(|field| {
        let rule = field
            .attrs
            .iter()
            .find(|attr| attr.path().is_ident("rule"))
            .map(|attr| attr.parse_args().unwrap());
        cmp_ty(field.ident.as_ref().unwrap(), &field.ty, rule)
    });
    quote! {
        #(#calls)*
    }
}

fn cmp_ty(field_name: &Ident, ty: &Type, rule: Option<Ident>) -> Option<TokenStream> {
    let cmp = if is_node_list(ty) {
        Some(quote! { ctx.cmp_lists(&self. #field_name, &pat. #field_name ); })
    } else if is_vec_attribute(ty) || is_token(ty) {
        None
    } else if is_box(ty) {
        Some(quote! { ctx.cmp_syn(&*self. #field_name, &*pat. #field_name ); })
    } else {
        Some(quote! { ctx.cmp_syn(&self. #field_name, &pat. #field_name ); })
    };
    cmp.map(|cmp| {
        if let Some(rule) = rule {
            quote! {
                if ctx.should_compare(molt_lib::RuleKey::#rule) {
                    #cmp
                }
            }
        } else {
            cmp
        }
    })
}

fn impl_enum(data_enum: syn::DataEnum) -> TokenStream {
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
                        (Self::#ident { #field_name: s1 }, Self::#ident { #field_name: s2 }) => { ctx.cmp_syn(s1, s2) },
                    }
                }
                Fields::Unnamed(fields) => {
                    // Too lazy to write this for generically many arguments
                    if fields.unnamed.len() == 1 {
                        quote! {
                            (Self::#ident(s1), Self::#ident(s2)) => { ctx.cmp_syn(s1, s2) },
                        }
                    }
                    else if fields.unnamed.len() == 2 {
                        quote! {
                            (Self::#ident(s11, s12), Self::#ident(s21, s22)) => {
                                ctx.cmp_syn(s11, s21);
                                ctx.cmp_syn(s12, s22);
                            },
                        }
                    }
                    else {
                        panic!("Too many items")
                    }
                }
                Fields::Unit => {
                    quote! {
                        (Self::#ident, Self::#ident) => {},
                    }
                }
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
