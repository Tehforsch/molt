#[macro_export]
macro_rules! custom_keyword {
    ($ident:ident) => {
        #[allow(non_camel_case_types)]
        pub struct $ident {
            #[allow(dead_code)]
            pub span: $crate::parser::__private::Span,
        }

        #[doc(hidden)]
        #[allow(dead_code, non_snake_case)]
        pub fn $ident<
            __S: $crate::parser::__private::IntoSpans<$crate::parser::__private::Span>,
        >(
            span: __S,
        ) -> $ident {
            $ident {
                span: $crate::parser::__private::IntoSpans::into_spans(span),
            }
        }

        const _: () = {
            impl Default for $ident {
                fn default() -> Self {
                    $ident {
                        span: $crate::parser::__private::Span::call_site(),
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
        impl $crate::parser::__private::CustomToken for $ident {
            fn peek(cursor: $crate::parser::buffer::Cursor) -> bool {
                if let Some((ident, _rest)) = cursor.ident() {
                    ident == stringify!($ident)
                } else {
                    false
                }
            }

            fn display() -> &'static str {
                std::concat!("`", stringify!($ident), "`")
            }
        }

        impl $crate::parser::parse::Parse for $ident {
            fn parse(
                input: $crate::parser::parse::ParseStream,
            ) -> $crate::parser::parse::Result<$ident> {
                input.step(|cursor| {
                    if let Some((ident, rest)) = cursor.ident() {
                        if ident == stringify!($ident) {
                            return Ok(($ident { span: ident.span() }, rest));
                        }
                    }
                    Err(cursor.error(std::concat!("expected `", stringify!($ident), "`",)))
                })
            }
        }
    };
}
