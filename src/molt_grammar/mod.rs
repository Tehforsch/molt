mod parse;

use rust_grammar::{Ident, Kind, Lit, Node};
use syntax_ctx::{Id, Var};

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

#[derive(Debug)]
pub(crate) struct VarDecl {
    pub id: VarId,
    pub node: Option<Id>,
}

pub type VarId = Id;

#[derive(Debug)]
pub(crate) enum Command {
    Match(VarId),
}

#[derive(Debug)]
pub(crate) struct Todo;

pub(crate) struct UntypedVar(Ident);

impl UntypedVar {
    pub(crate) fn to_var(self, user_kind: UserKind) -> Var<Node> {
        Var {
            kind: user_kind.into(),
            name: self.0.to_string(),
        }
    }
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

        pub(crate) fn parse_node_with_kind(parser: rust_grammar::parse::ParseStream, kind: UserKind) -> rust_grammar::Result<syntax_ctx::WithSpan<Node>> {
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
