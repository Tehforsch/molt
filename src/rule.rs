use crate::parser::parse::Result;
use crate::parser::parse::{Parse, ParseStream};
use crate::parser::token::Paren;
use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

#[derive(Clone, Copy, Debug)]
pub enum Rule {
    Ignore,
    Strict,
}

#[derive(Clone, Debug)]
pub struct Rules {
    rules: HashMap<RuleKey, Rule>,
}

/// Designates `CmpSyn` impls of types that have a rule
/// to toggle the behavior.
pub struct RequiresRule;

/// Designates `CmpSyn` impls of types that do not have a rule
/// to toggle the behavior.
pub struct DoesNotRequireRule;

macro_rules! rules {
    ($(($name: ident, $(($item: ident, $rule: ident)),* $(,)?),)* $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum RuleKey {
            $(
                $name($name),
            )*
        }

        pub enum RuleKeyKind {
            $(
                $name,
            )*
        }


        $(
            #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
            pub enum $name {
                $(
                    $item,
                )*
            }

            impl From<$name> for RuleKey {
                fn from(val: $name) -> RuleKey {
                    RuleKey::$name(val)
                }
            }
        )*

        impl Default for Rules {
            fn default() -> Self {
                Rules {
                    rules: [
                        $(
                            $(
                                (RuleKey::$name($name::$item), Rule::$rule),
                            )*
                        )*
                    ]
                    .into_iter()
                    .collect(),
                }
            }
        }

        pub enum ParseRuleKey {
            Blanket(crate::rule::RuleKeyKind),
            Concrete(Vec<crate::rule::RuleKey>),
        }

        impl ParseRuleKey {
            pub fn into_keys(self) -> Vec<crate::rule::RuleKey> {
                match self {
                    Self::Blanket(key) => {
                        match key {
                            $(
                                crate::rule::RuleKeyKind::$name => {
                                    vec![
                                        $(
                                            crate::rule::RuleKey::$name(crate::rule::$name::$item),
                                        )*
                                    ]
                                }
                            )*
                        }
                    }
                    Self::Concrete(keys) => {
                        keys
                    }
                }
            }
        }

        mod key_kws {
            $(
                crate::custom_keyword!($name);
            )*
            pub mod inner {
                $(
                    #[allow(non_snake_case)]
                    pub mod $name {
                        $(
                            crate::custom_keyword!($item);
                        )*
                    }
                )*
            }
        }

        impl Parse for crate::rule::RuleKeyKind {
            fn parse(input: ParseStream) -> Result<crate::rule::RuleKeyKind> {
                let lookahead = input.lookahead1();
                $(
                    if lookahead.peek(key_kws::$name) {
                        let _ = input.parse::<key_kws::$name>()?;
                        return Ok(crate::rule::RuleKeyKind::$name)
                    }
                )*
                return Err(lookahead.error());
            }
        }

        impl Parse for ParseRuleKey {
            fn parse(input: ParseStream) -> Result<Self> {
                let key = input.parse::<crate::rule::RuleKeyKind>()?;
                if input.peek(Paren) {
                    let content;
                    parenthesized!(content in input);
                    let mut types = vec![];
                    while !content.is_empty() {
                        match key {
                            $(
                                crate::rule::RuleKeyKind::$name => {
                                    let lookahead = content.lookahead1();
                                    $(
                                        if lookahead.peek(key_kws::inner::$name::$item) {
                                            let _ = content.parse::<key_kws::inner::$name::$item>();
                                            let inner = crate::rule::$name::$item;
                                            types.push(crate::rule::RuleKey::$name(inner));
                                            if content.peek(Token![,]) {
                                                let _ = content.parse::<Token![,]>()?;
                                                break
                                            }
                                            else {
                                                continue
                                            }
                                        }
                                    )*
                                    return Err(lookahead.error());
                                }
                            )*
                        }
                    }
                    Ok(ParseRuleKey::Concrete(types))
                }
                else {
                    Ok(ParseRuleKey::Blanket(key))
                }
            }
        }
    }
}

// Unfortunately, this is split over crates for now.
// Remember to update the corresponding macro call in
// src/molt_grammar/parse.rs
rules! {
    (Vis,
        (Const, Strict),
        (Enum, Strict),
        (ExternCrate, Strict),
        (Fn, Strict),
        (Mod, Strict),
        (Static, Strict),
        (Struct, Strict),
        (Trait, Strict),
        (TraitAlias, Strict),
        (Type, Strict),
        (Union, Strict),
        (Use, Strict),
        (Field, Strict),
    ),
    (Unsafe,
        (Mod, Strict),
        (Impl, Strict),
        (Trait, Strict),
        (Fn, Strict),
    ),
    (Async,
        (Fn, Strict),
        (Closure, Strict),
    ),
    (Const,
        (Fn, Strict),
        (Closure, Strict),
    ),
}

impl Index<RuleKey> for Rules {
    type Output = Rule;

    fn index(&self, index: RuleKey) -> &Self::Output {
        &self.rules[&index]
    }
}

impl IndexMut<RuleKey> for Rules {
    fn index_mut(&mut self, index: RuleKey) -> &mut Self::Output {
        self.rules.get_mut(&index).unwrap()
    }
}
