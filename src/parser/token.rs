use std::ops::{Deref, DerefMut};

use proc_macro2::extra::DelimSpan;
use proc_macro2::{Delimiter, Ident, Literal, Punct, Spacing, Span, TokenTree};

pub use self::private::CustomToken;
use self::private::WithSpan;
use crate::parser::buffer::Cursor;
use crate::parser::error::{Error, Result};
use crate::parser::parse::{Parse, ParseStream};
use crate::parser::span::IntoSpans;
use crate::rust_grammar::lifetime::Lifetime;

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

    use crate::parser::buffer::Cursor;

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
            /// [`Token!`]: crate::parser::token
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
            /// [`Token!`]: crate::parser::token
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
    [abstract]    => { $crate::parser::token::Abstract };
    [as]          => { $crate::parser::token::As };
    [async]       => { $crate::parser::token::Async };
    [auto]        => { $crate::parser::token::Auto };
    [await]       => { $crate::parser::token::Await };
    [become]      => { $crate::parser::token::Become };
    [box]         => { $crate::parser::token::Box };
    [break]       => { $crate::parser::token::Break };
    [const]       => { $crate::parser::token::Const };
    [continue]    => { $crate::parser::token::Continue };
    [crate]       => { $crate::parser::token::Crate };
    [default]     => { $crate::parser::token::Default };
    [do]          => { $crate::parser::token::Do };
    [dyn]         => { $crate::parser::token::Dyn };
    [else]        => { $crate::parser::token::Else };
    [enum]        => { $crate::parser::token::Enum };
    [extern]      => { $crate::parser::token::Extern };
    [final]       => { $crate::parser::token::Final };
    [fn]          => { $crate::parser::token::Fn };
    [for]         => { $crate::parser::token::For };
    [if]          => { $crate::parser::token::If };
    [impl]        => { $crate::parser::token::Impl };
    [in]          => { $crate::parser::token::In };
    [let]         => { $crate::parser::token::Let };
    [loop]        => { $crate::parser::token::Loop };
    [macro]       => { $crate::parser::token::Macro };
    [match]       => { $crate::parser::token::Match };
    [mod]         => { $crate::parser::token::Mod };
    [move]        => { $crate::parser::token::Move };
    [mut]         => { $crate::parser::token::Mut };
    [override]    => { $crate::parser::token::Override };
    [priv]        => { $crate::parser::token::Priv };
    [pub]         => { $crate::parser::token::Pub };
    [raw]         => { $crate::parser::token::Raw };
    [ref]         => { $crate::parser::token::Ref };
    [return]      => { $crate::parser::token::Return };
    [Self]        => { $crate::parser::token::SelfType };
    [self]        => { $crate::parser::token::SelfValue };
    [static]      => { $crate::parser::token::Static };
    [struct]      => { $crate::parser::token::Struct };
    [super]       => { $crate::parser::token::Super };
    [trait]       => { $crate::parser::token::Trait };
    [try]         => { $crate::parser::token::Try };
    [type]        => { $crate::parser::token::Type };
    [typeof]      => { $crate::parser::token::Typeof };
    [union]       => { $crate::parser::token::Union };
    [unsafe]      => { $crate::parser::token::Unsafe };
    [unsized]     => { $crate::parser::token::Unsized };
    [use]         => { $crate::parser::token::Use };
    [virtual]     => { $crate::parser::token::Virtual };
    [where]       => { $crate::parser::token::Where };
    [while]       => { $crate::parser::token::While };
    [yield]       => { $crate::parser::token::Yield };
    [&]           => { $crate::parser::token::And };
    [&&]          => { $crate::parser::token::AndAnd };
    [&=]          => { $crate::parser::token::AndEq };
    [@]           => { $crate::parser::token::At };
    [^]           => { $crate::parser::token::Caret };
    [^=]          => { $crate::parser::token::CaretEq };
    [:]           => { $crate::parser::token::Colon };
    [,]           => { $crate::parser::token::Comma };
    [$]           => { $crate::parser::token::Dollar };
    [.]           => { $crate::parser::token::Dot };
    [..]          => { $crate::parser::token::DotDot };
    [...]         => { $crate::parser::token::DotDotDot };
    [..=]         => { $crate::parser::token::DotDotEq };
    [=]           => { $crate::parser::token::Eq };
    [==]          => { $crate::parser::token::EqEq };
    [=>]          => { $crate::parser::token::FatArrow };
    [>=]          => { $crate::parser::token::Ge };
    [>]           => { $crate::parser::token::Gt };
    [<-]          => { $crate::parser::token::LArrow };
    [<=]          => { $crate::parser::token::Le };
    [<]           => { $crate::parser::token::Lt };
    [-]           => { $crate::parser::token::Minus };
    [-=]          => { $crate::parser::token::MinusEq };
    [!=]          => { $crate::parser::token::Ne };
    [!]           => { $crate::parser::token::Not };
    [|]           => { $crate::parser::token::Or };
    [|=]          => { $crate::parser::token::OrEq };
    [||]          => { $crate::parser::token::OrOr };
    [::]          => { $crate::parser::token::PathSep };
    [%]           => { $crate::parser::token::Percent };
    [%=]          => { $crate::parser::token::PercentEq };
    [+]           => { $crate::parser::token::Plus };
    [+=]          => { $crate::parser::token::PlusEq };
    [#]           => { $crate::parser::token::Pound };
    [?]           => { $crate::parser::token::Question };
    [->]          => { $crate::parser::token::RArrow };
    [;]           => { $crate::parser::token::Semi };
    [<<]          => { $crate::parser::token::Shl };
    [<<=]         => { $crate::parser::token::ShlEq };
    [>>]          => { $crate::parser::token::Shr };
    [>>=]         => { $crate::parser::token::ShrEq };
    [/]           => { $crate::parser::token::Slash };
    [/=]          => { $crate::parser::token::SlashEq };
    [*]           => { $crate::parser::token::Star };
    [*=]          => { $crate::parser::token::StarEq };
    [~]           => { $crate::parser::token::Tilde };
    [_]           => { $crate::parser::token::Underscore };
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
        impl crate::CmpSyn for $ty {
            fn cmp_syn(&self, _: &mut crate::Matcher, _: &Self) {}
        }
    };
}

use impl_zst_cmp_syn;

impl_zst_cmp_syn!(Group);
