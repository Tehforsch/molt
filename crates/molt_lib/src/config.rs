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
    ($(($name: ident, $rule: ident $(,$config: ident)?),)* $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum RuleKey {
            $(
                $name,
            )*
        }

        impl Default for Rules {
            fn default() -> Self {
                use RuleKey::*;
                use Rule::*;
                Rules {
                    rules: [
                        $(
                            ($name, $rule),
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
    (Vis, Strict),
}

impl Index<RuleKey> for Rules {
    type Output = Rule;

    fn index(&self, index: RuleKey) -> &Self::Output {
        &self.rules[&index]
    }
}
