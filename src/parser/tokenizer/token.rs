pub use rustc_lexer::Base;

use crate::parser::Mode;

use super::TokenizerError;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
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

impl Token {
    pub(super) fn from_rustc_token(
        code: &str,
        mode: Mode,
        value: rustc_lexer::Token,
    ) -> Result<Option<Self>, TokenizerError> {
        TokenKind::from_rustc_token_kind(code, mode, value.kind).map(|opt| {
            opt.map(|kind| Self {
                kind,
                len: value.len,
            })
        })
    }
}

impl TokenKind {
    fn from_rustc_token_kind(
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
