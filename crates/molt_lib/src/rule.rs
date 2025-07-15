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
