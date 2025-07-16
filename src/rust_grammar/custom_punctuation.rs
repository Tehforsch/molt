#[macro_export]
macro_rules! custom_punctuation {
    ($ident:ident, $($tt:tt)+) => {
        pub struct $ident {
            #[allow(dead_code)]
            pub spans: $crate::rust_grammar::custom_punctuation_repr!($($tt)+),
        }

        #[doc(hidden)]
        #[allow(dead_code, non_snake_case)]
        pub fn $ident<__S: $crate::rust_grammar::__private::IntoSpans<$crate::rust_grammar::custom_punctuation_repr!($($tt)+)>>(
            spans: __S,
        ) -> $ident {
            let _validate_len = 0 $(+ $crate::rust_grammar::custom_punctuation_len!(strict, $tt))*;
            $ident {
                spans: $crate::rust_grammar::__private::IntoSpans::into_spans(spans)
            }
        }

        const _: () = {
            impl Default for $ident {
                fn default() -> Self {
                    $ident($crate::rust_grammar::__private::Span::call_site())
                }
            }

            $crate::impl_parse_for_custom_punctuation!($ident, $($tt)+);
            $crate::impl_to_tokens_for_custom_punctuation!($ident, $($tt)+);
            $crate::impl_clone_for_custom_punctuation!($ident, $($tt)+);
            $crate::impl_extra_traits_for_custom_punctuation!($ident, $($tt)+);
        };
    };
}

// Not public API.
#[doc(hidden)]
#[macro_export]
macro_rules! impl_parse_for_custom_punctuation {
    ($ident:ident, $($tt:tt)+) => {
        impl $crate::rust_grammar::__private::CustomToken for $ident {
            fn peek(cursor: $crate::rust_grammar::buffer::Cursor) -> $crate::rust_grammar::__private::bool {
                $crate::rust_grammar::__private::peek_punct(cursor, $crate::rust_grammar::stringify_punct!($($tt)+))
            }

            fn display() -> &'static $crate::rust_grammar::__private::str {
                $crate::rust_grammar::__private::concat!("`", $crate::rust_grammar::stringify_punct!($($tt)+), "`")
            }
        }

        impl $crate::rust_grammar::parse::Parse for $ident {
            fn parse(input: $crate::rust_grammar::parse::ParseStream) -> $crate::rust_grammar::parse::Result<$ident> {
                let spans: $crate::rust_grammar::custom_punctuation_repr!($($tt)+) =
                    $crate::rust_grammar::__private::parse_punct(input, $crate::rust_grammar::stringify_punct!($($tt)+))?;
                Ok($ident(spans))
            }
        }
    };
}

// Not public API.
#[doc(hidden)]
#[macro_export]
macro_rules! impl_to_tokens_for_custom_punctuation {
    ($ident:ident, $($tt:tt)+) => {};
}

// Not public API.
#[doc(hidden)]
#[macro_export]
macro_rules! custom_punctuation_repr {
    ($($tt:tt)+) => {
        [$crate::rust_grammar::__private::Span; 0 $(+ $crate::rust_grammar::custom_punctuation_len!(lenient, $tt))+]
    };
}

// Not public API.
#[doc(hidden)]
#[macro_export]
#[rustfmt::skip]
macro_rules! custom_punctuation_len {
    ($mode:ident, &)     => { 1 };
    ($mode:ident, &&)    => { 2 };
    ($mode:ident, &=)    => { 2 };
    ($mode:ident, @)     => { 1 };
    ($mode:ident, ^)     => { 1 };
    ($mode:ident, ^=)    => { 2 };
    ($mode:ident, :)     => { 1 };
    ($mode:ident, ,)     => { 1 };
    ($mode:ident, $)     => { 1 };
    ($mode:ident, .)     => { 1 };
    ($mode:ident, ..)    => { 2 };
    ($mode:ident, ...)   => { 3 };
    ($mode:ident, ..=)   => { 3 };
    ($mode:ident, =)     => { 1 };
    ($mode:ident, ==)    => { 2 };
    ($mode:ident, =>)    => { 2 };
    ($mode:ident, >=)    => { 2 };
    ($mode:ident, >)     => { 1 };
    ($mode:ident, <-)    => { 2 };
    ($mode:ident, <=)    => { 2 };
    ($mode:ident, <)     => { 1 };
    ($mode:ident, -)     => { 1 };
    ($mode:ident, -=)    => { 2 };
    ($mode:ident, !=)    => { 2 };
    ($mode:ident, !)     => { 1 };
    ($mode:ident, |)     => { 1 };
    ($mode:ident, |=)    => { 2 };
    ($mode:ident, ||)    => { 2 };
    ($mode:ident, ::)    => { 2 };
    ($mode:ident, %)     => { 1 };
    ($mode:ident, %=)    => { 2 };
    ($mode:ident, +)     => { 1 };
    ($mode:ident, +=)    => { 2 };
    ($mode:ident, #)     => { 1 };
    ($mode:ident, ?)     => { 1 };
    ($mode:ident, ->)    => { 2 };
    ($mode:ident, ;)     => { 1 };
    ($mode:ident, <<)    => { 2 };
    ($mode:ident, <<=)   => { 3 };
    ($mode:ident, >>)    => { 2 };
    ($mode:ident, >>=)   => { 3 };
    ($mode:ident, /)     => { 1 };
    ($mode:ident, /=)    => { 2 };
    ($mode:ident, *)     => { 1 };
    ($mode:ident, *=)    => { 2 };
    ($mode:ident, ~)     => { 1 };
    (lenient, $tt:tt)    => { 0 };
    (strict, $tt:tt)     => {{ $crate::rust_grammar::custom_punctuation_unexpected!($tt); 0 }};
}

// Not public API.
#[doc(hidden)]
#[macro_export]
macro_rules! custom_punctuation_unexpected {
    () => {};
}

// Not public API.
#[doc(hidden)]
#[macro_export]
macro_rules! stringify_punct {
    ($($tt:tt)+) => {
        $crate::rust_grammar::__private::concat!($($crate::rust_grammar::__private::stringify!($tt)),+)
    };
}
