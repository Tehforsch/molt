pub use rustc_lexer::Base;

use crate::parser::Mode;

use super::{Ident, Parse, Parser, Result, Span, TokenizerError};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    Ident,
    RawIdent,
    Keyword(Keyword),
    Literal {
        kind: LiteralKind,
        suffix_start: usize,
    },
    Lifetime {
        starts_with_number: bool,
    },
    Semi,
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    At,
    Pound,
    Tilde,
    Question,
    Colon,
    Dollar,
    Eq,
    Not,
    Lt,
    Gt,
    Minus,
    And,
    Or,
    Plus,
    Star,
    Slash,
    Caret,
    Percent,
    Eof,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    Int { base: Base, empty_int: bool },
    Float { base: Base, empty_exponent: bool },
    Char,
    Byte,
    Str,
    ByteStr,
    RawStr { n_hashes: usize, started: bool },
    RawByteStr { n_hashes: usize, started: bool },
}

impl TokenKind {
    pub fn from_rustc_token_kind(
        code: &str,
        mode: Mode,
        value: rustc_lexer::TokenKind,
    ) -> Result<Option<Self>, TokenizerError> {
        let kind = match value {
            rustc_lexer::TokenKind::LineComment => return Ok(None),
            rustc_lexer::TokenKind::BlockComment { .. } => return Ok(None),
            rustc_lexer::TokenKind::Whitespace => return Ok(None),
            rustc_lexer::TokenKind::Unknown => return Err(TokenizerError),
            rustc_lexer::TokenKind::Literal { kind, suffix_start } => {
                return LiteralKind::from_rustc_literal_kind(kind)
                    .map(|kind| Some(TokenKind::Literal { kind, suffix_start }));
            }
            rustc_lexer::TokenKind::Ident => {
                if let Some(kw) = Keyword::new(code, mode) {
                    TokenKind::Keyword(kw)
                } else {
                    TokenKind::Ident
                }
            }
            rustc_lexer::TokenKind::RawIdent => TokenKind::RawIdent,
            rustc_lexer::TokenKind::Lifetime { starts_with_number } => {
                TokenKind::Lifetime { starts_with_number }
            }
            rustc_lexer::TokenKind::Semi => TokenKind::Semi,
            rustc_lexer::TokenKind::Comma => TokenKind::Comma,
            rustc_lexer::TokenKind::Dot => TokenKind::Dot,
            rustc_lexer::TokenKind::OpenParen => TokenKind::OpenParen,
            rustc_lexer::TokenKind::CloseParen => TokenKind::CloseParen,
            rustc_lexer::TokenKind::OpenBrace => TokenKind::OpenBrace,
            rustc_lexer::TokenKind::CloseBrace => TokenKind::CloseBrace,
            rustc_lexer::TokenKind::OpenBracket => TokenKind::OpenBracket,
            rustc_lexer::TokenKind::CloseBracket => TokenKind::CloseBracket,
            rustc_lexer::TokenKind::At => TokenKind::At,
            rustc_lexer::TokenKind::Pound => TokenKind::Pound,
            rustc_lexer::TokenKind::Tilde => TokenKind::Tilde,
            rustc_lexer::TokenKind::Question => TokenKind::Question,
            rustc_lexer::TokenKind::Colon => TokenKind::Colon,
            rustc_lexer::TokenKind::Dollar => TokenKind::Dollar,
            rustc_lexer::TokenKind::Eq => TokenKind::Eq,
            rustc_lexer::TokenKind::Not => TokenKind::Not,
            rustc_lexer::TokenKind::Lt => TokenKind::Lt,
            rustc_lexer::TokenKind::Gt => TokenKind::Gt,
            rustc_lexer::TokenKind::Minus => TokenKind::Minus,
            rustc_lexer::TokenKind::And => TokenKind::And,
            rustc_lexer::TokenKind::Or => TokenKind::Or,
            rustc_lexer::TokenKind::Plus => TokenKind::Plus,
            rustc_lexer::TokenKind::Star => TokenKind::Star,
            rustc_lexer::TokenKind::Slash => TokenKind::Slash,
            rustc_lexer::TokenKind::Caret => TokenKind::Caret,
            rustc_lexer::TokenKind::Percent => TokenKind::Percent,
        };
        Ok(Some(kind))
    }
}

fn filter_terminated(kind: LiteralKind, terminated: bool) -> Result<LiteralKind, TokenizerError> {
    if terminated {
        Ok(kind)
    } else {
        Err(TokenizerError)
    }
}

impl LiteralKind {
    fn from_rustc_literal_kind(value: rustc_lexer::LiteralKind) -> Result<Self, TokenizerError> {
        match value {
            rustc_lexer::LiteralKind::Int { base, empty_int } => {
                Ok(LiteralKind::Int { base, empty_int })
            }
            rustc_lexer::LiteralKind::Float {
                base,
                empty_exponent,
            } => Ok(LiteralKind::Float {
                base,
                empty_exponent,
            }),
            rustc_lexer::LiteralKind::Char { terminated } => {
                if terminated {
                    Ok(LiteralKind::Char)
                } else {
                    Err(TokenizerError)
                }
            }
            rustc_lexer::LiteralKind::Byte { terminated } => {
                filter_terminated(LiteralKind::Byte, terminated)
            }
            rustc_lexer::LiteralKind::Str { terminated } => {
                filter_terminated(LiteralKind::Str, terminated)
            }
            rustc_lexer::LiteralKind::ByteStr { terminated } => {
                filter_terminated(LiteralKind::ByteStr, terminated)
            }
            rustc_lexer::LiteralKind::RawStr {
                n_hashes,
                started,
                terminated,
            } => filter_terminated(LiteralKind::RawStr { n_hashes, started }, terminated),
            rustc_lexer::LiteralKind::RawByteStr {
                n_hashes,
                started,
                terminated,
            } => filter_terminated(LiteralKind::RawByteStr { n_hashes, started }, terminated),
        }
    }
}

macro_rules! define_kw {
    (
        RUST
        $(($ident1: ident, $lit1: literal)),* ,
        MOLT
        $(($ident2: ident, $lit2: literal)),* ,
    ) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
        pub enum Keyword {
            $(
                $ident1,
            )*
            $(
                $ident2,
            )*
        }

        impl Keyword {
            fn new(s: &str, mode: Mode) -> Option<Self> {
                match s {
                    $(
                        $lit1 => Some(Self::$ident1),
                    )*
                    $(
                        $lit2 if matches!(mode, Mode::Molt) => Some(Self::$ident2),
                    )*
                    _ => None,
                }
            }
        }

        #[cfg(test)]
        #[test]
        fn check_kw() {
            $(
                assert!(matches!(Keyword::new($lit1, Mode::Rust), Some(Keyword::$ident1)));
                assert!(matches!(Keyword::new($lit1, Mode::Molt), Some(Keyword::$ident1)));
            )*
            $(
                assert!(matches!(Keyword::new($lit2, Mode::Rust), None));
                assert!(matches!(Keyword::new($lit2, Mode::Molt), Some(Keyword::$ident2)));
            )*
        }
    }
}

define_kw!(
    RUST
    // Rust keywords
    (Abstract, "abstract"),
    (As, "as"),
    (Async, "async"),
    (Auto, "auto"),
    (Await, "await"),
    (Become, "become"),
    (Box, "box"),
    (Break, "break"),
    (Const, "const"),
    (Continue, "continue"),
    (Crate, "crate"),
    (Default, "default"),
    (Do, "do"),
    (Dyn, "dyn"),
    (Else, "else"),
    (Enum, "enum"),
    (Extern, "extern"),
    (Final, "final"),
    (Fn, "fn"),
    (For, "for"),
    (If, "if"),
    (Impl, "impl"),
    (In, "in"),
    (Loop, "loop"),
    (Macro, "macro"),
    (Mod, "mod"),
    (Move, "move"),
    (Mut, "mut"),
    (Override, "override"),
    (Priv, "priv"),
    (Pub, "pub"),
    (Raw, "raw"),
    (Ref, "ref"),
    (Return, "return"),
    (UpperSelf, "Self"),
    (LowerSelf, "self"),
    (Static, "static"),
    (Struct, "struct"),
    (Super, "super"),
    (Trait, "trait"),
    (Try, "try"),
    (Type, "type"),
    (Typeof, "typeof"),
    (Union, "union"),
    (Unsafe, "unsafe"),
    (Unsized, "unsized"),
    (Use, "use"),
    (Virtual, "virtual"),
    (Where, "where"),
    (While, "while"),
    (Yield, "yield"),
    (Let, "let"),
    (Match, "match"),
    MOLT
    // Molt keywords
    (Ident, "Ident"),
    (Lit, "Lit"),
    (Transform, "transform"),
);

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

pub trait IsToken {
    fn is_token(kind: TokenKind) -> bool;
}

macro_rules! define_token {
    ($(($ident: ident, $expr: expr)),* $(,)?) => {
        $(
        // #[allow(unused)]
        pub struct $ident;

        impl Parse for $ident {
            fn parse(parser: &mut Parser) -> Result<Self> {
                parser.consume($expr).map(|_| $ident)
            }
        }

        impl IsToken for $ident {
            fn is_token(kind: TokenKind) -> bool {
                kind == $expr
            }
        }
        )*
    }
}

define_token! {
    // (Abstract, TokenKind::Keyword(Keyword::Abstract)),
    // (As, TokenKind::Keyword(Keyword::As)),
    // (Async, TokenKind::Keyword(Keyword::Async)),
    // (Auto, TokenKind::Keyword(Keyword::Auto)),
    // (Await, TokenKind::Keyword(Keyword::Await)),
    // (Become, TokenKind::Keyword(Keyword::Become)),
    // (Box, TokenKind::Keyword(Keyword::Box)),
    // (Break, TokenKind::Keyword(Keyword::Break)),
    (Const, TokenKind::Keyword(Keyword::Const)),
    // (Continue, TokenKind::Keyword(Keyword::Continue)),
    // (Crate, TokenKind::Keyword(Keyword::Crate)),
    // (Default, TokenKind::Keyword(Keyword::Default)),
    // (Do, TokenKind::Keyword(Keyword::Do)),
    // (Dyn, TokenKind::Keyword(Keyword::Dyn)),
    // (Else, TokenKind::Keyword(Keyword::Else)),
    // (Enum, TokenKind::Keyword(Keyword::Enum)),
    // (Extern, TokenKind::Keyword(Keyword::Extern)),
    // (Final, TokenKind::Keyword(Keyword::Final)),
    // (Fn, TokenKind::Keyword(Keyword::Fn)),
    // (For, TokenKind::Keyword(Keyword::For)),
    // (If, TokenKind::Keyword(Keyword::If)),
    // (Impl, TokenKind::Keyword(Keyword::Impl)),
    // (In, TokenKind::Keyword(Keyword::In)),
    // (Let, TokenKind::Keyword(Keyword::Let)),
    // (Loop, TokenKind::Keyword(Keyword::Loop)),
    // (Macro, TokenKind::Keyword(Keyword::Macro)),
    // (Match, TokenKind::Keyword(Keyword::Match)),
    // (Mod, TokenKind::Keyword(Keyword::Mod)),
    // (Move, TokenKind::Keyword(Keyword::Move)),
    // (Mut, TokenKind::Keyword(Keyword::Mut)),
    // (Override, TokenKind::Keyword(Keyword::Override)),
    // (Priv, TokenKind::Keyword(Keyword::Priv)),
    // (Pub, TokenKind::Keyword(Keyword::Pub)),
    // (Raw, TokenKind::Keyword(Keyword::Raw)),
    // (Ref, TokenKind::Keyword(Keyword::Ref)),
    // (Return, TokenKind::Keyword(Keyword::Return)),
    // (SelfType, TokenKind::Keyword(Keyword::SelfType)),
    // (SelfValue, TokenKind::Keyword(Keyword::SelfValue)),
    // (Static, TokenKind::Keyword(Keyword::Static)),
    // (Struct, TokenKind::Keyword(Keyword::Struct)),
    // (Super, TokenKind::Keyword(Keyword::Super)),
    // (Trait, TokenKind::Keyword(Keyword::Trait)),
    // (Try, TokenKind::Keyword(Keyword::Try)),
    // (Type, TokenKind::Keyword(Keyword::Type)),
    // (Typeof, TokenKind::Keyword(Keyword::Typeof)),
    // (Union, TokenKind::Keyword(Keyword::Union)),
    // (Unsafe, TokenKind::Keyword(Keyword::Unsafe)),
    // (Unsized, TokenKind::Keyword(Keyword::Unsized)),
    // (Use, TokenKind::Keyword(Keyword::Use)),
    // (Virtual, TokenKind::Keyword(Keyword::Virtual)),
    // (Where, TokenKind::Keyword(Keyword::Where)),
    // (While, TokenKind::Keyword(Keyword::While)),
    // (Yield, TokenKind::Keyword(Keyword::Yield)),
    // (And, TokenKind::And),
    // (AndAnd, TokenKind::AndAnd),
    // (AndEq, TokenKind::AndEq),
    // (At, TokenKind::At),
    // (Caret, TokenKind::Caret),
    // (CaretEq, TokenKind::CaretEq),
    (Colon, TokenKind::Colon),
    // (Comma, TokenKind::Comma),
    // (Dollar, TokenKind::Dollar),
    // (Dot, TokenKind::Dot),
    // (DotDot, TokenKind::DotDot),
    // (DotDotDot, TokenKind::DotDotDot),
    // (DotDotEq, TokenKind::DotDotEq),
    (Eq, TokenKind::Eq),
    // (EqEq, TokenKind::EqEq),
    // (FatArrow, TokenKind::FatArrow),
    // (Ge, TokenKind::Ge),
    // (Gt, TokenKind::Gt),
    // (LArrow, TokenKind::LArrow),
    // (Le, TokenKind::Le),
    // (Lt, TokenKind::Lt),
    // (Minus, TokenKind::Minus),
    // (MinusEq, TokenKind::MinusEq),
    // (Ne, TokenKind::Ne),
    // (Not, TokenKind::Not),
    // (Or, TokenKind::Or),
    // (OrEq, TokenKind::OrEq),
    // (OrOr, TokenKind::OrOr),
    // (PathSep, TokenKind::PathSep),
    // (Percent, TokenKind::Percent),
    // (PercentEq, TokenKind::PercentEq),
    // (Plus, TokenKind::Plus),
    // (PlusEq, TokenKind::PlusEq),
    // (Pound, TokenKind::Pound),
    // (Question, TokenKind::Question),
    // (RArrow, TokenKind::RArrow),
    (Semi, TokenKind::Semi),
    // (Shl, TokenKind::Shl),
    // (ShlEq, TokenKind::ShlEq),
    // (Shr, TokenKind::Shr),
    // (ShrEq, TokenKind::ShrEq),
    // (Slash, TokenKind::Slash),
    // (SlashEq, TokenKind::SlashEq),
    // (Star, TokenKind::Star),
    // (StarEq, TokenKind::StarEq),
    // (Tilde, TokenKind::Tilde),
    (Underscore, TokenKind::Caret),
}

impl IsToken for Ident {
    fn is_token(kind: TokenKind) -> bool {
        matches!(kind, TokenKind::Ident)
    }
}
