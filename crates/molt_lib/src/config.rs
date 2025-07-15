use std::{collections::HashMap, ops::Index};

#[derive(Clone)]
pub struct Config {
    pub debug_print: bool,
    pub cargo_fmt: bool,
    pub interactive: bool,
    pub check_compilation: bool,
    pub rules: Rules,
}

impl Config {
    pub fn test() -> Self {
        Self {
            debug_print: true,
            cargo_fmt: false,
            interactive: false,
            check_compilation: false,
            rules: Rules::default(),
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            debug_print: false,
            cargo_fmt: true,
            interactive: false,
            check_compilation: false,
            rules: Rules::default(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Rule {
    Ignore,
    Strict,
}

#[derive(Clone, Debug)]
pub struct Rules {
    rules: HashMap<RuleKey, Rule>,
}

macro_rules! rules {
    ($(($name: ident, $(($item: ident, $rule: ident)),* $(,)?),)* $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum RuleKey {
            $(
                $name($name),
            )*
        }

        $(
            #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
            pub enum $name {
                $(
                    $item,
                )*
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
    ),
}

impl Index<RuleKey> for Rules {
    type Output = Rule;

    fn index(&self, index: RuleKey) -> &Self::Output {
        &self.rules[&index]
    }
}
