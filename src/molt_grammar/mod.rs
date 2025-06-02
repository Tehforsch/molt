mod parse;

use molt_lib::{Id, Var, VarDecl};
use rust_grammar::{Kind, Lit, Node};

#[derive(Debug)]
pub(crate) struct MoltFile {
    pub vars: Vec<VarDecl>,
    pub commands: Vec<Command>,
    pub sorted: bool,
}

pub(crate) enum Decl {
    VarDecl(VarDecl),
    Command(Command),
}

pub type VarId = Id;

#[derive(Debug)]
pub(crate) enum Command {
    Match(Id),
}

macro_rules! define_user_kind {
    ($(($variant_name: ident, $ty: ty)),*$(,)?) => {
        #[derive(Debug, Clone, Copy)]
        pub enum UserKind {
            $( $variant_name, )*
        }

        impl From<UserKind> for Kind {
            fn from(val: UserKind) -> Kind {
                match val {
                    $(
                        UserKind::$variant_name => Kind::$variant_name,
                    )*
                }
            }
        }

        mod kind_kws {
            $(
                rust_grammar::custom_keyword!($variant_name);
            )*
        }

        impl rust_grammar::parse::Parse for UserKind {
            fn parse(input:rust_grammar::parse:: ParseStream) -> rust_grammar::Result<Self> {
                $(
                    if input.peek(kind_kws::$variant_name) {
                        let _: kind_kws::$variant_name = input.parse()?;
                        return Ok(UserKind::$variant_name);
                    }
                )*
                Err(input.error("Invalid kind."))
            }
        }

        pub(crate) fn parse_node_with_kind(parser: rust_grammar::parse::ParseStream, kind: UserKind) -> rust_grammar::Result<molt_lib::WithSpan<Node>> {
            match kind {
                $(
                    UserKind::$variant_name => {
                        Ok(parser.parse_with_span::<$ty>()?.map(|t| Node::$variant_name(t)))
                    },
                )*
            }
        }
    }
}

define_user_kind! {
    (Lit, Lit)
}
