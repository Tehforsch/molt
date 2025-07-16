use std::ops::{Deref, DerefMut};

use proc_macro2::extra::DelimSpan;
use proc_macro2::{Delimiter, Ident, Literal, Punct, Spacing, Span, TokenTree};

pub use self::private::CustomToken;
use self::private::WithSpan;
use crate::rust_grammar::buffer::Cursor;
use crate::rust_grammar::error::{Error, Result};
use crate::rust_grammar::lifetime::Lifetime;
use crate::rust_grammar::parse::{Parse, ParseStream};
use crate::rust_grammar::span::IntoSpans;

/// Marker trait for types that represent single tokens.
///
/// This trait is sealed and cannot be implemented for types outside of Syn.
pub trait Token: private::Sealed {
    // Not public API.
    #[doc(hidden)]
    fn peek(cursor: Cursor) -> bool;

    // Not public API.
    #[doc(hidden)]
    fn display() -> &'static str;
}

pub(crate) mod private {

    use proc_macro2::Span;

    use crate::rust_grammar::buffer::Cursor;

    pub trait Sealed {}

    /// Support writing `token.span` rather than `token.spans[0]` on tokens that
    /// hold a single span.
    #[repr(transparent)]
    #[allow(unknown_lints, repr_transparent_external_private_fields)] // False positive: https://github.com/rust-lang/rust/issues/78586#issuecomment-1722680482
    pub struct WithSpan {
        pub span: Span,
    }

    // Not public API.
    #[doc(hidden)]
    pub trait CustomToken {
        fn peek(cursor: Cursor) -> bool;
        fn display() -> &'static str;
    }
}

impl private::Sealed for Ident {}

macro_rules! impl_low_level_token {
    ($display:literal $($path:ident)::+ $get:ident) => {

        impl Token for $($path)::+ {
            fn peek(cursor: Cursor) -> bool {
                cursor.$get().is_some()
            }

            fn display() -> &'static str {
                $display
            }
        }


        impl private::Sealed for $($path)::+ {}
    };
}

impl_low_level_token!("punctuation token" Punct punct);
impl_low_level_token!("literal" Literal literal);
impl_low_level_token!("token" TokenTree token_tree);
impl_low_level_token!("group token" proc_macro2::Group any_group);
impl_low_level_token!("lifetime" Lifetime lifetime);

impl<T: CustomToken> private::Sealed for T {}

impl<T: CustomToken> Token for T {
    fn peek(cursor: Cursor) -> bool {
        <Self as CustomToken>::peek(cursor)
    }

    fn display() -> &'static str {
        <Self as CustomToken>::display()
    }
}

macro_rules! define_keywords {
    ($($token:literal pub struct $name:ident)*) => {
        $(
            #[doc = concat!('`', $token, '`')]
            ///
            /// Don't try to remember the name of this type &mdash; use the
            /// [`Token!`] macro instead.
            ///
            /// [`Token!`]: crate::rust_grammar::token
            #[derive(Debug)]
            pub struct $name {
                #[allow(unused)]
                pub span: Span,
            }

            impl_zst_cmp_syn!($name);

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $name<S: IntoSpans<Span>>(span: S) -> $name {
                $name {
                    span: span.into_spans(),
                }
            }

            impl std::default::Default for $name {
                fn default() -> Self {
                    $name {
                        span: Span::call_site(),
                    }
                }
            }


            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self> {
                    Ok($name {
                        span: keyword(input, $token)?,
                    })
                }
            }


            impl Token for $name {
                fn peek(cursor: Cursor) -> bool {
                    peek_keyword(cursor, $token)
                }

                fn display() -> &'static str {
                    concat!("`", $token, "`")
                }
            }


            impl private::Sealed for $name {}
        )*
    };
}

macro_rules! impl_deref_if_len_is_1 {
    ($name:ident/1) => {
        impl Deref for $name {
            type Target = WithSpan;

            fn deref(&self) -> &Self::Target {
                unsafe { &*(self as *const Self).cast::<WithSpan>() }
            }
        }

        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                unsafe { &mut *(self as *mut Self).cast::<WithSpan>() }
            }
        }
    };

    ($name:ident/$len:literal) => {};
}

macro_rules! define_punctuation_structs {
    ($($token:literal pub struct $name:ident/$len:tt #[doc = $usage:literal])*) => {
        $(
            #[allow(unknown_lints, repr_transparent_external_private_fields)] // False positive: https://github.com/rust-lang/rust/issues/78586#issuecomment-1722680482
            #[doc = concat!('`', $token, '`')]
            ///
            /// Usage:
            #[doc = concat!($usage, '.')]
            ///
            /// Don't try to remember the name of this type &mdash; use the
            /// [`Token!`] macro instead.
            ///
            /// [`Token!`]: crate::rust_grammar::token
            #[derive(Debug)]
            pub struct $name {
                #[allow(unused)]
                pub spans: [Span; $len],
            }

            impl_zst_cmp_syn!($name);

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $name<S: IntoSpans<[Span; $len]>>(spans: S) -> $name {
                $name {
                    spans: spans.into_spans(),
                }
            }

            impl std::default::Default for $name {
                fn default() -> Self {
                    $name {
                        spans: [Span::call_site(); $len],
                    }
                }
            }

            impl_deref_if_len_is_1!($name/$len);
        )*
    };
}

macro_rules! define_punctuation {
    ($($token:literal pub struct $name:ident/$len:tt #[doc = $usage:literal])*) => {
        $(
            define_punctuation_structs! {
                $token pub struct $name/$len #[doc = $usage]
            }


            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self> {
                    Ok($name {
                        spans: punct(input, $token)?,
                    })
                }
            }


            impl Token for $name {
                fn peek(cursor: Cursor) -> bool {
                    peek_punct(cursor, $token)
                }

                fn display() -> &'static str {
                    concat!("`", $token, "`")
                }
            }


            impl private::Sealed for $name {}
        )*
    };
}

macro_rules! define_delimiters {
    ($($delim:ident pub struct $name:ident #[$doc:meta])*) => {
        $(
            #[$doc]
            #[derive(Debug)]
            pub struct $name {
                pub span: DelimSpan,
            }

            impl_zst_cmp_syn!($name);

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $name<S: IntoSpans<DelimSpan>>(span: S) -> $name {
                $name {
                    span: span.into_spans(),
                }
            }

            impl std::default::Default for $name {
                fn default() -> Self {
                    $name(Span::call_site())
                }
            }


            impl private::Sealed for $name {}
        )*
    };
}

define_punctuation_structs! {
    "_" pub struct Underscore/1 /// wildcard patterns, inferred types, unnamed items in constants, extern crates, use declarations, and destructuring assignment
}

impl Parse for Underscore {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| {
            if let Some((ident, rest)) = cursor.ident() {
                if ident == "_" {
                    return Ok((Underscore(ident.span()), rest));
                }
            }
            if let Some((punct, rest)) = cursor.punct() {
                if punct.as_char() == '_' {
                    return Ok((Underscore(punct.span()), rest));
                }
            }
            Err(cursor.error("expected `_`"))
        })
    }
}

impl Token for Underscore {
    fn peek(cursor: Cursor) -> bool {
        if let Some((ident, _rest)) = cursor.ident() {
            return ident == "_";
        }
        if let Some((punct, _rest)) = cursor.punct() {
            return punct.as_char() == '_';
        }
        false
    }

    fn display() -> &'static str {
        "`_`"
    }
}

impl private::Sealed for Underscore {}

/// None-delimited group
#[derive(Debug)]
pub struct Group {
    pub span: Span,
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Group<S: IntoSpans<Span>>(span: S) -> Group {
    Group {
        span: span.into_spans(),
    }
}

impl std::default::Default for Group {
    fn default() -> Self {
        Group {
            span: Span::call_site(),
        }
    }
}

impl private::Sealed for Group {}

impl Token for Paren {
    fn peek(cursor: Cursor) -> bool {
        cursor.group(Delimiter::Parenthesis).is_some()
    }

    fn display() -> &'static str {
        "parentheses"
    }
}

impl Token for Brace {
    fn peek(cursor: Cursor) -> bool {
        cursor.group(Delimiter::Brace).is_some()
    }

    fn display() -> &'static str {
        "curly braces"
    }
}

impl Token for Bracket {
    fn peek(cursor: Cursor) -> bool {
        cursor.group(Delimiter::Bracket).is_some()
    }

    fn display() -> &'static str {
        "square brackets"
    }
}

impl Token for Group {
    fn peek(cursor: Cursor) -> bool {
        cursor.group(Delimiter::None).is_some()
    }

    fn display() -> &'static str {
        "invisible group"
    }
}

define_keywords! {
    "as"          pub struct As
    "async"       pub struct Async
    "auto"        pub struct Auto
    "await"       pub struct Await
    "become"      pub struct Become
    "box"         pub struct Box
    "break"       pub struct Break
    "const"       pub struct Const
    "continue"    pub struct Continue
    "crate"       pub struct Crate
    "default"     pub struct Default
    "dyn"         pub struct Dyn
    "else"        pub struct Else
    "enum"        pub struct Enum
    "extern"      pub struct Extern
    "fn"          pub struct Fn
    "for"         pub struct For
    "if"          pub struct If
    "impl"        pub struct Impl
    "in"          pub struct In
    "let"         pub struct Let
    "loop"        pub struct Loop
    "macro"       pub struct Macro
    "match"       pub struct Match
    "mod"         pub struct Mod
    "move"        pub struct Move
    "mut"         pub struct Mut
    "pub"         pub struct Pub
    "raw"         pub struct Raw
    "ref"         pub struct Ref
    "return"      pub struct Return
    "Self"        pub struct SelfType
    "self"        pub struct SelfValue
    "static"      pub struct Static
    "struct"      pub struct Struct
    "super"       pub struct Super
    "trait"       pub struct Trait
    "try"         pub struct Try
    "type"        pub struct Type
    "union"       pub struct Union
    "unsafe"      pub struct Unsafe
    "use"         pub struct Use
    "where"       pub struct Where
    "while"       pub struct While
    "yield"       pub struct Yield
}

define_punctuation! {
    "&"           pub struct And/1        /// bitwise and logical AND, borrow, references, reference patterns
    "&&"          pub struct AndAnd/2     /// lazy AND, borrow, references, reference patterns
    "&="          pub struct AndEq/2      /// bitwise AND assignment
    "@"           pub struct At/1         /// subpattern binding
    "^"           pub struct Caret/1      /// bitwise and logical XOR
    "^="          pub struct CaretEq/2    /// bitwise XOR assignment
    ":"           pub struct Colon/1      /// various separators
    ","           pub struct Comma/1      /// various separators
    "$"           pub struct Dollar/1     /// macros
    "."           pub struct Dot/1        /// field access, tuple index
    ".."          pub struct DotDot/2     /// range, struct expressions, patterns, range patterns
    "..."         pub struct DotDotDot/3  /// variadic functions, range patterns
    "..="         pub struct DotDotEq/3   /// inclusive range, range patterns
    "="           pub struct Eq/1         /// assignment, attributes, various type definitions
    "=="          pub struct EqEq/2       /// equal
    "=>"          pub struct FatArrow/2   /// match arms, macros
    ">="          pub struct Ge/2         /// greater than or equal to, generics
    ">"           pub struct Gt/1         /// greater than, generics, paths
    "<="          pub struct Le/2         /// less than or equal to
    "<"           pub struct Lt/1         /// less than, generics, paths
    "-"           pub struct Minus/1      /// subtraction, negation
    "-="          pub struct MinusEq/2    /// subtraction assignment
    "!="          pub struct Ne/2         /// not equal
    "!"           pub struct Not/1        /// bitwise and logical NOT, macro calls, inner attributes, never type, negative impls
    "|"           pub struct Or/1         /// bitwise and logical OR, closures, patterns in match, if let, and while let
    "|="          pub struct OrEq/2       /// bitwise OR assignment
    "||"          pub struct OrOr/2       /// lazy OR, closures
    "::"          pub struct PathSep/2    /// path separator
    "%"           pub struct Percent/1    /// remainder
    "%="          pub struct PercentEq/2  /// remainder assignment
    "+"           pub struct Plus/1       /// addition, trait bounds, macro Kleene matcher
    "+="          pub struct PlusEq/2     /// addition assignment
    "#"           pub struct Pound/1      /// attributes
    "?"           pub struct Question/1   /// question mark operator, questionably sized, macro Kleene matcher
    "->"          pub struct RArrow/2     /// function return type, closure return type, function pointer type
    ";"           pub struct Semi/1       /// terminator for various items and statements, array types
    "<<"          pub struct Shl/2        /// shift left, nested generics
    "<<="         pub struct ShlEq/3      /// shift left assignment
    ">>"          pub struct Shr/2        /// shift right, nested generics
    ">>="         pub struct ShrEq/3      /// shift right assignment, nested generics
    "/"           pub struct Slash/1      /// division
    "/="          pub struct SlashEq/2    /// division assignment
    "*"           pub struct Star/1       /// multiplication, dereference, raw pointers, macro Kleene matcher, use wildcards
    "*="          pub struct StarEq/2     /// multiplication assignment
    "~"           pub struct Tilde/1      /// unused since before Rust 1.0
}

define_delimiters! {
    Brace         pub struct Brace        /// `{`&hellip;`}`
    Bracket       pub struct Bracket      /// `[`&hellip;`]`
    Parenthesis   pub struct Paren        /// `(`&hellip;`)`
}

/// A type-macro that expands to the name of the Rust type representation of a
/// given token.
///
/// As a type, `Token!` is commonly used in the type of struct fields, the type
/// of a `let` statement, or in turbofish for a `parse` function.
///
/// As an expression, `Token!` is used for peeking tokens or instantiating
/// tokens from a span.
#[macro_export]
macro_rules! Token {
    [abstract]    => { $crate::rust_grammar::token::Abstract };
    [as]          => { $crate::rust_grammar::token::As };
    [async]       => { $crate::rust_grammar::token::Async };
    [auto]        => { $crate::rust_grammar::token::Auto };
    [await]       => { $crate::rust_grammar::token::Await };
    [become]      => { $crate::rust_grammar::token::Become };
    [box]         => { $crate::rust_grammar::token::Box };
    [break]       => { $crate::rust_grammar::token::Break };
    [const]       => { $crate::rust_grammar::token::Const };
    [continue]    => { $crate::rust_grammar::token::Continue };
    [crate]       => { $crate::rust_grammar::token::Crate };
    [default]     => { $crate::rust_grammar::token::Default };
    [do]          => { $crate::rust_grammar::token::Do };
    [dyn]         => { $crate::rust_grammar::token::Dyn };
    [else]        => { $crate::rust_grammar::token::Else };
    [enum]        => { $crate::rust_grammar::token::Enum };
    [extern]      => { $crate::rust_grammar::token::Extern };
    [final]       => { $crate::rust_grammar::token::Final };
    [fn]          => { $crate::rust_grammar::token::Fn };
    [for]         => { $crate::rust_grammar::token::For };
    [if]          => { $crate::rust_grammar::token::If };
    [impl]        => { $crate::rust_grammar::token::Impl };
    [in]          => { $crate::rust_grammar::token::In };
    [let]         => { $crate::rust_grammar::token::Let };
    [loop]        => { $crate::rust_grammar::token::Loop };
    [macro]       => { $crate::rust_grammar::token::Macro };
    [match]       => { $crate::rust_grammar::token::Match };
    [mod]         => { $crate::rust_grammar::token::Mod };
    [move]        => { $crate::rust_grammar::token::Move };
    [mut]         => { $crate::rust_grammar::token::Mut };
    [override]    => { $crate::rust_grammar::token::Override };
    [priv]        => { $crate::rust_grammar::token::Priv };
    [pub]         => { $crate::rust_grammar::token::Pub };
    [raw]         => { $crate::rust_grammar::token::Raw };
    [ref]         => { $crate::rust_grammar::token::Ref };
    [return]      => { $crate::rust_grammar::token::Return };
    [Self]        => { $crate::rust_grammar::token::SelfType };
    [self]        => { $crate::rust_grammar::token::SelfValue };
    [static]      => { $crate::rust_grammar::token::Static };
    [struct]      => { $crate::rust_grammar::token::Struct };
    [super]       => { $crate::rust_grammar::token::Super };
    [trait]       => { $crate::rust_grammar::token::Trait };
    [try]         => { $crate::rust_grammar::token::Try };
    [type]        => { $crate::rust_grammar::token::Type };
    [typeof]      => { $crate::rust_grammar::token::Typeof };
    [union]       => { $crate::rust_grammar::token::Union };
    [unsafe]      => { $crate::rust_grammar::token::Unsafe };
    [unsized]     => { $crate::rust_grammar::token::Unsized };
    [use]         => { $crate::rust_grammar::token::Use };
    [virtual]     => { $crate::rust_grammar::token::Virtual };
    [where]       => { $crate::rust_grammar::token::Where };
    [while]       => { $crate::rust_grammar::token::While };
    [yield]       => { $crate::rust_grammar::token::Yield };
    [&]           => { $crate::rust_grammar::token::And };
    [&&]          => { $crate::rust_grammar::token::AndAnd };
    [&=]          => { $crate::rust_grammar::token::AndEq };
    [@]           => { $crate::rust_grammar::token::At };
    [^]           => { $crate::rust_grammar::token::Caret };
    [^=]          => { $crate::rust_grammar::token::CaretEq };
    [:]           => { $crate::rust_grammar::token::Colon };
    [,]           => { $crate::rust_grammar::token::Comma };
    [$]           => { $crate::rust_grammar::token::Dollar };
    [.]           => { $crate::rust_grammar::token::Dot };
    [..]          => { $crate::rust_grammar::token::DotDot };
    [...]         => { $crate::rust_grammar::token::DotDotDot };
    [..=]         => { $crate::rust_grammar::token::DotDotEq };
    [=]           => { $crate::rust_grammar::token::Eq };
    [==]          => { $crate::rust_grammar::token::EqEq };
    [=>]          => { $crate::rust_grammar::token::FatArrow };
    [>=]          => { $crate::rust_grammar::token::Ge };
    [>]           => { $crate::rust_grammar::token::Gt };
    [<-]          => { $crate::rust_grammar::token::LArrow };
    [<=]          => { $crate::rust_grammar::token::Le };
    [<]           => { $crate::rust_grammar::token::Lt };
    [-]           => { $crate::rust_grammar::token::Minus };
    [-=]          => { $crate::rust_grammar::token::MinusEq };
    [!=]          => { $crate::rust_grammar::token::Ne };
    [!]           => { $crate::rust_grammar::token::Not };
    [|]           => { $crate::rust_grammar::token::Or };
    [|=]          => { $crate::rust_grammar::token::OrEq };
    [||]          => { $crate::rust_grammar::token::OrOr };
    [::]          => { $crate::rust_grammar::token::PathSep };
    [%]           => { $crate::rust_grammar::token::Percent };
    [%=]          => { $crate::rust_grammar::token::PercentEq };
    [+]           => { $crate::rust_grammar::token::Plus };
    [+=]          => { $crate::rust_grammar::token::PlusEq };
    [#]           => { $crate::rust_grammar::token::Pound };
    [?]           => { $crate::rust_grammar::token::Question };
    [->]          => { $crate::rust_grammar::token::RArrow };
    [;]           => { $crate::rust_grammar::token::Semi };
    [<<]          => { $crate::rust_grammar::token::Shl };
    [<<=]         => { $crate::rust_grammar::token::ShlEq };
    [>>]          => { $crate::rust_grammar::token::Shr };
    [>>=]         => { $crate::rust_grammar::token::ShrEq };
    [/]           => { $crate::rust_grammar::token::Slash };
    [/=]          => { $crate::rust_grammar::token::SlashEq };
    [*]           => { $crate::rust_grammar::token::Star };
    [*=]          => { $crate::rust_grammar::token::StarEq };
    [~]           => { $crate::rust_grammar::token::Tilde };
    [_]           => { $crate::rust_grammar::token::Underscore };
}

pub(crate) fn keyword(input: ParseStream, token: &str) -> Result<Span> {
    input.step(|cursor| {
        if let Some((ident, rest)) = cursor.ident() {
            if ident == token {
                return Ok((ident.span(), rest));
            }
        }
        Err(cursor.error(format!("expected `{token}`")))
    })
}

pub(crate) fn peek_keyword(cursor: Cursor, token: &str) -> bool {
    if let Some((ident, _rest)) = cursor.ident() {
        ident == token
    } else {
        false
    }
}

fn punct<const N: usize>(input: ParseStream, token: &str) -> Result<[Span; N]> {
    let mut spans = [input.span(); N];
    punct_helper(input, token, &mut spans)?;
    Ok(spans)
}

fn punct_helper(input: ParseStream, token: &str, spans: &mut [Span]) -> Result<()> {
    input.step(|cursor| {
        let mut cursor = *cursor;
        assert_eq!(token.len(), spans.len());

        for (i, ch) in token.chars().enumerate() {
            match cursor.punct() {
                Some((punct, rest)) => {
                    spans[i] = punct.span();
                    if punct.as_char() != ch {
                        break;
                    } else if i == token.len() - 1 {
                        return Ok(((), rest));
                    } else if punct.spacing() != Spacing::Joint {
                        break;
                    }
                    cursor = rest;
                }
                None => break,
            }
        }

        Err(Error::new(spans[0], format!("expected `{token}`")))
    })
}

fn peek_punct(mut cursor: Cursor, token: &str) -> bool {
    for (i, ch) in token.chars().enumerate() {
        match cursor.punct() {
            Some((punct, rest)) => {
                if punct.as_char() != ch {
                    break;
                } else if i == token.len() - 1 {
                    return true;
                } else if punct.spacing() != Spacing::Joint {
                    break;
                }
                cursor = rest;
            }
            None => break,
        }
    }
    false
}

macro_rules! impl_zst_cmp_syn {
    ($ty: ty) => {
        impl molt_lib::CmpSyn for $ty {
            fn cmp_syn(&self, _: &mut molt_lib::Matcher, _: &Self) {}
        }
    };
}

use impl_zst_cmp_syn;

impl_zst_cmp_syn!(Group);
