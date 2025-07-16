#[macro_export]
macro_rules! custom_keyword {
    ($ident:ident) => {
        #[allow(non_camel_case_types)]
        pub struct $ident {
            #[allow(dead_code)]
            pub span: $crate::rust_grammar::__private::Span,
        }

        #[doc(hidden)]
        #[allow(dead_code, non_snake_case)]
        pub fn $ident<
            __S: $crate::rust_grammar::__private::IntoSpans<$crate::rust_grammar::__private::Span>,
        >(
            span: __S,
        ) -> $ident {
            $ident {
                span: $crate::rust_grammar::__private::IntoSpans::into_spans(span),
            }
        }

        const _: () = {
            impl Default for $ident {
                fn default() -> Self {
                    $ident {
                        span: $crate::rust_grammar::__private::Span::call_site(),
                    }
                }
            }

            $crate::impl_parse_for_custom_keyword!($ident);
        };
    };
}

// Not public API.
#[doc(hidden)]
#[macro_export]
macro_rules! impl_parse_for_custom_keyword {
    ($ident:ident) => {
        // For peek.
        impl $crate::rust_grammar::__private::CustomToken for $ident {
            fn peek(cursor: $crate::rust_grammar::buffer::Cursor) -> bool {
                if let Some((ident, _rest)) = cursor.ident() {
                    ident == stringify!($ident)
                } else {
                    false
                }
            }

            fn display() -> &'static str {
                $crate::rust_grammar::__private::concat!("`", stringify!($ident), "`")
            }
        }

        impl $crate::rust_grammar::parse::Parse for $ident {
            fn parse(
                input: $crate::rust_grammar::parse::ParseStream,
            ) -> $crate::rust_grammar::parse::Result<$ident> {
                input.step(|cursor| {
                    if let Some((ident, rest)) = cursor.ident() {
                        if ident == stringify!($ident) {
                            return Ok(($ident { span: ident.span() }, rest));
                        }
                    }
                    Err(cursor.error($crate::rust_grammar::__private::concat!(
                        "expected `",
                        stringify!($ident),
                        "`",
                    )))
                })
            }
        }
    };
}
