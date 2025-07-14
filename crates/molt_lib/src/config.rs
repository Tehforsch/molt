use std::collections::HashMap;

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

#[derive(Clone, Copy)]
pub enum Rule {
    Ignore,
    Strict,
}

#[derive(Clone)]
pub struct Rules {
    rules: HashMap<RuleKey, Rule>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuleKey {
    FnAsync,
    FnConst,
    FnUnsafe,
    FnGenerics,
    // ...
}

impl Default for Rules {
    fn default() -> Self {
        use RuleKey::*;
        Rules {
            rules: [
                (FnAsync, Rule::Ignore),
                (FnConst, Rule::Ignore),
                (FnUnsafe, Rule::Ignore),
                (FnGenerics, Rule::Strict),
            ]
            .into_iter()
            .collect(),
        }
    }
}
