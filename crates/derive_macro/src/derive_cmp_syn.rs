use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident, Token, Type, parse::Parse, parse_macro_input};

use crate::utils::{is_box, is_node_list, is_token, is_vec_attribute};

pub fn impl_cmp_syn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let node = get_node_from_attr(&input);
    let requires_rule = input
        .attrs
        .iter()
        .any(|attr| attr.path().is_ident("requires_rule"));
    let name = &input.ident;

    let impl_ = match input.data {
        Data::Struct(data_struct) => impl_struct(data_struct),
        Data::Enum(data_enum) => impl_enum(data_enum),
        _ => panic!("Union found"),
    };

    let rule_ty = if requires_rule {
        quote! { crate::rule::RequiresRule }
    } else {
        quote! { crate::rule::DoesNotRequireRule }
    };

    let expanded = quote! {
        impl crate::CmpSyn<#node, #name, #rule_ty> for #name {
            fn cmp_syn(&self, ctx: &mut crate::Matcher<#node>, pat: &Self) -> crate::match_pattern::IsMatch {
                #impl_
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn get_node_from_attr(input: &DeriveInput) -> Type {
    input
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("node"))
        .map(|attr| attr.parse_args().unwrap())
        .expect("No node type given. Add #[node(...)] attribute.")
}

pub struct Rule {
    rule_name: Ident,
    _comma: Token![,],
    surrounding_ty: Ident,
}

impl Parse for Rule {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Rule {
            rule_name: input.parse()?,
            _comma: input.parse()?,
            surrounding_ty: input.parse()?,
        })
    }
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
        crate::match_pattern::IsMatch::Ok(())
    }
}

fn cmp_ty(field_name: &Ident, ty: &Type, rule: Option<Rule>) -> Option<TokenStream> {
    let cmp_fn = match rule {
        Some(_) => quote! { cmp_syn_with_rule },
        None => quote! { cmp_syn },
    };
    let rule_arg = match rule {
        Some(rule) => {
            let rule_name = rule.rule_name;
            let surrounding_ty = rule.surrounding_ty;
            quote! { , crate::rule::RuleKey::#rule_name(crate::rule::#rule_name::#surrounding_ty) }
        }
        None => quote! {},
    };
    if is_node_list(ty) {
        Some(quote! { ctx.cmp_lists(&self. #field_name, &pat. #field_name )?; })
    } else if is_vec_attribute(ty) || is_token(ty) {
        None
    } else if is_box(ty) {
        Some(quote! { ctx.#cmp_fn(&*self. #field_name, &*pat. #field_name #rule_arg )?; })
    } else {
        Some(quote! { ctx.#cmp_fn(&self. #field_name, &pat. #field_name #rule_arg )?; })
    }
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
                                ctx.cmp_syn(s11, s21)?;
                                ctx.cmp_syn(s12, s22)
                            },
                        }
                    }
                    else {
                        panic!("Too many items")
                    }
                }
                Fields::Unit => {
                    quote! {
                        (Self::#ident, Self::#ident) => { Ok(()) },
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
