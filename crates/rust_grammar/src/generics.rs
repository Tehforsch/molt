use derive_macro::CmpSyn;

use crate::attr::Attribute;
use crate::expr::Expr;
use crate::ident::Ident;
use crate::lifetime::Lifetime;
use crate::path::Path;
use crate::punctuated::Punctuated;
use crate::token;
use crate::ty::Type;
use molt_lib::NodeId;
use proc_macro2::TokenStream;

#[derive(Debug, CmpSyn, Default)]
/// Lifetimes and type parameters attached to a declaration of a function,
/// enum, trait, etc.
///
/// This struct represents two distinct optional syntactic elements,
/// [generic parameters] and [where clause]. In some locations of the
/// grammar, there may be other tokens in between these two things.
///
/// [generic parameters]: https://doc.rust-lang.org/stable/reference/items/generics.html#generic-parameters
/// [where clause]: https://doc.rust-lang.org/stable/reference/items/generics.html#where-clauses
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct Generics {
    pub lt_token: Option<Token![<]>,
    pub params: Punctuated<GenericParam, Token![,]>,
    pub gt_token: Option<Token![>]>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, CmpSyn)]
/// A generic type parameter, lifetime, or const generic: `T: Into<String>`,
/// `'a: 'b`, `const LEN: usize`.
///
/// # Syntax tree enum
///
/// This type is a [syntax tree enum].
///
/// [syntax tree enum]: crate::expr::Expr#syntax-tree-enums
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub enum GenericParam {
    /// A lifetime parameter: `'a: 'b + 'c + 'd`.
    Lifetime(LifetimeParam),

    /// A generic type parameter: `T: Into<String>`.
    Type(TypeParam),

    /// A const generic parameter: `const LENGTH: usize`.
    Const(ConstParam),
}

#[derive(Debug, CmpSyn)]
/// A lifetime definition: `'a: 'b + 'c + 'd`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct LifetimeParam {
    pub attrs: Vec<Attribute>,
    pub lifetime: Lifetime,
    pub colon_token: Option<Token![:]>,
    pub bounds: Punctuated<Lifetime, Token![+]>,
}

#[derive(Debug, CmpSyn)]
/// A generic type parameter: `T: Into<String>`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct TypeParam {
    pub attrs: Vec<Attribute>,
    pub ident: NodeId<Ident>,
    pub colon_token: Option<Token![:]>,
    pub bounds: Punctuated<TypeParamBound, Token![+]>,
    pub eq_token: Option<Token![=]>,
    pub default: Option<NodeId<Type>>,
}

#[derive(Debug, CmpSyn)]
/// A const generic parameter: `const LENGTH: usize`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ConstParam {
    pub attrs: Vec<Attribute>,
    pub const_token: Token![const],
    pub ident: NodeId<Ident>,
    pub colon_token: Token![:],
    pub ty: NodeId<Type>,
    pub eq_token: Option<Token![=]>,
    pub default: Option<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A set of bound lifetimes: `for<'a, 'b, 'c>`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct BoundLifetimes {
    pub for_token: Token![for],
    pub lt_token: Token![<],
    pub lifetimes: Punctuated<GenericParam, Token![,]>,
    pub gt_token: Token![>],
}

#[derive(Debug, CmpSyn)]
/// A trait used as a bound on a type parameter.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct TraitBound {
    pub paren_token: Option<token::Paren>,
    pub modifier: TraitBoundModifier,
    /// The `for<'a>` in `for<'a> Foo<&'a T>`
    pub lifetimes: Option<BoundLifetimes>,
    /// The `Foo<&'a T>` in `for<'a> Foo<&'a T>`
    pub path: Path,
}

#[derive(Debug, CmpSyn)]
/// A trait or lifetime used as a bound on a type parameter.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
#[non_exhaustive]
pub enum TypeParamBound {
    Trait(TraitBound),
    Lifetime(Lifetime),
    PreciseCapture(PreciseCapture),
    Verbatim(TokenStream),
}

#[derive(Debug, CmpSyn)]
/// A modifier on a trait bound, currently only used for the `?` in
/// `?Sized`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub enum TraitBoundModifier {
    None,
    Maybe(Token![?]),
}

#[derive(Debug, CmpSyn)]
/// Precise capturing bound: the 'use&lt;&hellip;&gt;' in `impl Trait +
/// use<'a, T>`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct PreciseCapture {
    pub use_token: Token![use],
    pub lt_token: Token![<],
    pub params: Punctuated<CapturedParam, Token![,]>,
    pub gt_token: Token![>],
}

#[derive(Debug, CmpSyn)]
/// Single parameter in a precise capturing bound.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
#[non_exhaustive]
pub enum CapturedParam {
    /// A lifetime parameter in precise capturing bound: `fn f<'a>() -> impl
    /// Trait + use<'a>`.
    Lifetime(Lifetime),
    /// A type parameter or const generic parameter in precise capturing
    /// bound: `fn f<T>() -> impl Trait + use<T>` or `fn f<const K: T>() ->
    /// impl Trait + use<K>`.
    Ident(NodeId<Ident>),
}

#[derive(Debug, CmpSyn)]
/// A `where` clause in a definition: `where T: Deserialize<'de>, D:
/// 'static`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct WhereClause {
    pub where_token: Token![where],
    pub predicates: Punctuated<WherePredicate, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A single predicate in a `where` clause: `T: Deserialize<'de>`.
///
/// # Syntax tree enum
///
/// This type is a [syntax tree enum].
///
/// [syntax tree enum]: crate::expr::Expr#syntax-tree-enums
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
#[non_exhaustive]
pub enum WherePredicate {
    /// A lifetime predicate in a `where` clause: `'a: 'b + 'c`.
    Lifetime(PredicateLifetime),

    /// A type predicate in a `where` clause: `for<'c> Foo<'c>: Trait<'c>`.
    Type(PredicateType),
}

#[derive(Debug, CmpSyn)]
/// A lifetime predicate in a `where` clause: `'a: 'b + 'c`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct PredicateLifetime {
    pub lifetime: Lifetime,
    pub colon_token: Token![:],
    pub bounds: Punctuated<Lifetime, Token![+]>,
}

#[derive(Debug, CmpSyn)]
/// A type predicate in a `where` clause: `for<'c> Foo<'c>: Trait<'c>`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct PredicateType {
    /// Any lifetimes from a `for` binding
    pub lifetimes: Option<BoundLifetimes>,
    /// The type being bounded
    pub bounded_ty: NodeId<Type>,
    pub colon_token: Token![:],
    /// Trait and lifetime bounds (`Clone+Send+'static`)
    pub bounds: Punctuated<TypeParamBound, Token![+]>,
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    use molt_lib::NodeId;

    use crate::attr::Attribute;
    use crate::error::{self, Result};
    use crate::generics::{
        BoundLifetimes, ConstParam, GenericParam, Generics, LifetimeParam, PredicateLifetime,
        PredicateType, TraitBound, TraitBoundModifier, TypeParam, TypeParamBound, WhereClause,
        WherePredicate,
    };
    #[cfg(feature = "full")]
    use crate::generics::{CapturedParam, PreciseCapture};
    use crate::ident::{AnyIdent, Ident};
    use crate::lifetime::Lifetime;
    use crate::parse::{Parse, ParseStream};
    use crate::path::{self, ParenthesizedGenericArguments, Path, PathArguments};
    use crate::punctuated::Punctuated;
    use crate::token;
    use crate::verbatim;

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Generics {
        fn parse(input: ParseStream) -> Result<Self> {
            if !input.peek(Token![<]) {
                return Ok(Generics::default());
            }

            let lt_token: Token![<] = input.parse()?;

            let mut params = Punctuated::new();
            loop {
                if input.peek(Token![>]) {
                    break;
                }

                let attrs = input.call(Attribute::parse_outer)?;
                let lookahead = input.lookahead1();
                if lookahead.peek(Lifetime) {
                    params.push_value(GenericParam::Lifetime(LifetimeParam {
                        attrs,
                        ..input.parse()?
                    }));
                } else if lookahead.peek_pat::<Ident>() {
                    params.push_value(GenericParam::Type(TypeParam {
                        attrs,
                        ..input.parse()?
                    }));
                } else if lookahead.peek(Token![const]) {
                    params.push_value(GenericParam::Const(ConstParam {
                        attrs,
                        ..input.parse()?
                    }));
                } else if input.peek(Token![_]) {
                    params.push_value(GenericParam::Type(TypeParam {
                        attrs,
                        ident: input.parse_id::<AnyIdent>()?,
                        colon_token: None,
                        bounds: Punctuated::new(),
                        eq_token: None,
                        default: None,
                    }));
                } else {
                    return Err(lookahead.error());
                }

                if input.peek(Token![>]) {
                    break;
                }
                let punct = input.parse()?;
                params.push_punct(punct);
            }

            let gt_token: Token![>] = input.parse()?;

            Ok(Generics {
                lt_token: Some(lt_token),
                params,
                gt_token: Some(gt_token),
                where_clause: None,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for GenericParam {
        fn parse(input: ParseStream) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;

            let lookahead = input.lookahead1();
            if lookahead.peek_pat::<Ident>() {
                Ok(GenericParam::Type(TypeParam {
                    attrs,
                    ..input.parse()?
                }))
            } else if lookahead.peek(Lifetime) {
                Ok(GenericParam::Lifetime(LifetimeParam {
                    attrs,
                    ..input.parse()?
                }))
            } else if lookahead.peek(Token![const]) {
                Ok(GenericParam::Const(ConstParam {
                    attrs,
                    ..input.parse()?
                }))
            } else {
                Err(lookahead.error())
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for LifetimeParam {
        fn parse(input: ParseStream) -> Result<Self> {
            let has_colon;
            Ok(LifetimeParam {
                attrs: input.call(Attribute::parse_outer)?,
                lifetime: input.parse()?,
                colon_token: {
                    if input.peek(Token![:]) {
                        has_colon = true;
                        Some(input.parse()?)
                    } else {
                        has_colon = false;
                        None
                    }
                },
                bounds: {
                    let mut bounds = Punctuated::new();
                    if has_colon {
                        loop {
                            if input.peek(Token![,]) || input.peek(Token![>]) {
                                break;
                            }
                            let value = input.parse()?;
                            bounds.push_value(value);
                            if !input.peek(Token![+]) {
                                break;
                            }
                            let punct = input.parse()?;
                            bounds.push_punct(punct);
                        }
                    }
                    bounds
                },
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for BoundLifetimes {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(BoundLifetimes {
                for_token: input.parse()?,
                lt_token: input.parse()?,
                lifetimes: {
                    let mut lifetimes = Punctuated::new();
                    while !input.peek(Token![>]) {
                        let attrs = input.call(Attribute::parse_outer)?;
                        let lifetime: Lifetime = input.parse()?;
                        lifetimes.push_value(GenericParam::Lifetime(LifetimeParam {
                            attrs,
                            lifetime,
                            colon_token: None,
                            bounds: Punctuated::new(),
                        }));
                        if input.peek(Token![>]) {
                            break;
                        }
                        lifetimes.push_punct(input.parse()?);
                    }
                    lifetimes
                },
                gt_token: input.parse()?,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Option<BoundLifetimes> {
        fn parse(input: ParseStream) -> Result<Self> {
            if input.peek(Token![for]) {
                input.parse().map(Some)
            } else {
                Ok(None)
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for TypeParam {
        fn parse(input: ParseStream) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let ident: NodeId<Ident> = input.parse()?;
            let colon_token: Option<Token![:]> = input.parse()?;

            let mut bounds = Punctuated::new();
            if colon_token.is_some() {
                loop {
                    if input.peek(Token![,]) || input.peek(Token![>]) || input.peek(Token![=]) {
                        break;
                    }
                    bounds.push_value({
                        let allow_precise_capture = false;
                        let allow_tilde_const = true;
                        TypeParamBound::parse_single(
                            input,
                            allow_precise_capture,
                            allow_tilde_const,
                        )?
                    });
                    if !input.peek(Token![+]) {
                        break;
                    }
                    let punct: Token![+] = input.parse()?;
                    bounds.push_punct(punct);
                }
            }

            let eq_token: Option<Token![=]> = input.parse()?;
            let default = if eq_token.is_some() {
                Some(input.parse()?)
            } else {
                None
            };

            Ok(TypeParam {
                attrs,
                ident,
                colon_token,
                bounds,
                eq_token,
                default,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for TypeParamBound {
        fn parse(input: ParseStream) -> Result<Self> {
            let allow_precise_capture = true;
            let allow_tilde_const = true;
            Self::parse_single(input, allow_precise_capture, allow_tilde_const)
        }
    }

    impl TypeParamBound {
        pub(crate) fn parse_single(
            input: ParseStream,
            #[cfg_attr(not(feature = "full"), allow(unused_variables))] allow_precise_capture: bool,
            allow_tilde_const: bool,
        ) -> Result<Self> {
            if input.peek(Lifetime) {
                return input.parse().map(TypeParamBound::Lifetime);
            }

            let begin = input.fork();

            #[cfg(feature = "full")]
            {
                if input.peek(Token![use]) {
                    let precise_capture: PreciseCapture = input.parse()?;
                    return if allow_precise_capture {
                        Ok(TypeParamBound::PreciseCapture(precise_capture))
                    } else {
                        let msg = "`use<...>` precise capturing syntax is not allowed here";
                        Err(error::new2(
                            precise_capture.use_token.span,
                            precise_capture.gt_token.span,
                            msg,
                        ))
                    };
                }
            }

            let content;
            let (paren_token, content) = if input.peek(token::Paren) {
                (Some(parenthesized!(content in input)), &content)
            } else {
                (None, input)
            };

            let is_tilde_const =
                cfg!(feature = "full") && content.peek(Token![~]) && content.peek2(Token![const]);
            if is_tilde_const {
                let tilde_token: Token![~] = content.parse()?;
                let const_token: Token![const] = content.parse()?;
                if !allow_tilde_const {
                    let msg = "`~const` is not allowed here";
                    return Err(error::new2(tilde_token.span, const_token.span, msg));
                }
            }

            let mut bound: TraitBound = content.parse()?;
            bound.paren_token = paren_token;

            if is_tilde_const {
                Ok(TypeParamBound::Verbatim(verbatim::between(&begin, input)))
            } else {
                Ok(TypeParamBound::Trait(bound))
            }
        }

        pub(crate) fn parse_multiple(
            input: ParseStream,
            allow_plus: bool,
            allow_precise_capture: bool,
            allow_tilde_const: bool,
        ) -> Result<Punctuated<Self, Token![+]>> {
            let mut bounds = Punctuated::new();
            loop {
                let bound = Self::parse_single(input, allow_precise_capture, allow_tilde_const)?;
                bounds.push_value(bound);
                if !(allow_plus && input.peek(Token![+])) {
                    break;
                }
                bounds.push_punct(input.parse()?);
                if !(input.peek_pat::<AnyIdent>()
                    || input.peek(Token![::])
                    || input.peek(Token![?])
                    || input.peek(Lifetime)
                    || input.peek(token::Paren)
                    || input.peek(Token![~]))
                {
                    break;
                }
            }
            Ok(bounds)
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for TraitBound {
        fn parse(input: ParseStream) -> Result<Self> {
            let modifier: TraitBoundModifier = input.parse()?;
            let lifetimes: Option<BoundLifetimes> = input.parse()?;

            let mut path: Path = input.parse()?;
            if path.segments.last().unwrap().arguments.is_empty()
                && (input.peek(token::Paren) || input.peek(Token![::]) && input.peek3(token::Paren))
            {
                input.parse::<Option<Token![::]>>()?;
                let args: ParenthesizedGenericArguments = input.parse()?;
                let parenthesized = PathArguments::Parenthesized(args);
                path.segments.last_mut().unwrap().arguments = parenthesized;
            }

            Ok(TraitBound {
                paren_token: None,
                modifier,
                lifetimes,
                path,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for TraitBoundModifier {
        fn parse(input: ParseStream) -> Result<Self> {
            if input.peek(Token![?]) {
                input.parse().map(TraitBoundModifier::Maybe)
            } else {
                Ok(TraitBoundModifier::None)
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ConstParam {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut default = None;
            Ok(ConstParam {
                attrs: input.call(Attribute::parse_outer)?,
                const_token: input.parse()?,
                ident: input.parse()?,
                colon_token: input.parse()?,
                ty: input.parse()?,
                eq_token: {
                    if input.peek(Token![=]) {
                        let eq_token = input.parse()?;
                        default = Some(path::parsing::const_argument(input)?);
                        Some(eq_token)
                    } else {
                        None
                    }
                },
                default,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for WhereClause {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(WhereClause {
                where_token: input.parse()?,
                predicates: {
                    let mut predicates = Punctuated::new();
                    loop {
                        if input.is_empty()
                            || input.peek(token::Brace)
                            || input.peek(Token![,])
                            || input.peek(Token![;])
                            || input.peek(Token![:]) && !input.peek(Token![::])
                            || input.peek(Token![=])
                        {
                            break;
                        }
                        let value = input.parse()?;
                        predicates.push_value(value);
                        if !input.peek(Token![,]) {
                            break;
                        }
                        let punct = input.parse()?;
                        predicates.push_punct(punct);
                    }
                    predicates
                },
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Option<WhereClause> {
        fn parse(input: ParseStream) -> Result<Self> {
            if input.peek(Token![where]) {
                input.parse().map(Some)
            } else {
                Ok(None)
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for WherePredicate {
        fn parse(input: ParseStream) -> Result<Self> {
            if input.peek(Lifetime) && input.peek2(Token![:]) {
                Ok(WherePredicate::Lifetime(PredicateLifetime {
                    lifetime: input.parse()?,
                    colon_token: input.parse()?,
                    bounds: {
                        let mut bounds = Punctuated::new();
                        loop {
                            if input.is_empty()
                                || input.peek(token::Brace)
                                || input.peek(Token![,])
                                || input.peek(Token![;])
                                || input.peek(Token![:])
                                || input.peek(Token![=])
                            {
                                break;
                            }
                            let value = input.parse()?;
                            bounds.push_value(value);
                            if !input.peek(Token![+]) {
                                break;
                            }
                            let punct = input.parse()?;
                            bounds.push_punct(punct);
                        }
                        bounds
                    },
                }))
            } else {
                Ok(WherePredicate::Type(PredicateType {
                    lifetimes: input.parse()?,
                    bounded_ty: input.parse()?,
                    colon_token: input.parse()?,
                    bounds: {
                        let mut bounds = Punctuated::new();
                        loop {
                            if input.is_empty()
                                || input.peek(token::Brace)
                                || input.peek(Token![,])
                                || input.peek(Token![;])
                                || input.peek(Token![:]) && !input.peek(Token![::])
                                || input.peek(Token![=])
                            {
                                break;
                            }
                            bounds.push_value({
                                let allow_precise_capture = false;
                                let allow_tilde_const = true;
                                TypeParamBound::parse_single(
                                    input,
                                    allow_precise_capture,
                                    allow_tilde_const,
                                )?
                            });
                            if !input.peek(Token![+]) {
                                break;
                            }
                            let punct = input.parse()?;
                            bounds.push_punct(punct);
                        }
                        bounds
                    },
                }))
            }
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for PreciseCapture {
        fn parse(input: ParseStream) -> Result<Self> {
            let use_token: Token![use] = input.parse()?;
            let lt_token: Token![<] = input.parse()?;
            let mut params = Punctuated::new();
            loop {
                let lookahead = input.lookahead1();
                params.push_value(
                    if lookahead.peek(Lifetime)
                        || lookahead.peek_pat::<Ident>()
                        || input.peek(Token![Self])
                    {
                        input.parse::<CapturedParam>()?
                    } else if lookahead.peek(Token![>]) {
                        break;
                    } else {
                        return Err(lookahead.error());
                    },
                );
                let lookahead = input.lookahead1();
                params.push_punct(if lookahead.peek(Token![,]) {
                    input.parse::<Token![,]>()?
                } else if lookahead.peek(Token![>]) {
                    break;
                } else {
                    return Err(lookahead.error());
                });
            }
            let gt_token: Token![>] = input.parse()?;
            Ok(PreciseCapture {
                use_token,
                lt_token,
                params,
                gt_token,
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for CapturedParam {
        fn parse(input: ParseStream) -> Result<Self> {
            let lookahead = input.lookahead1();
            if lookahead.peek(Lifetime) {
                input.parse().map(CapturedParam::Lifetime)
            } else if lookahead.peek_pat::<Ident>() || input.peek(Token![Self]) {
                input.parse_id::<AnyIdent>().map(CapturedParam::Ident)
            } else {
                Err(lookahead.error())
            }
        }
    }
}
