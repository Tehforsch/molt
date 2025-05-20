use crate::ctx::NodeId;

pub(crate) trait AsNode {
    fn as_node(self) -> Node;
    fn from_node(node: &Node) -> Option<&Self>;
    fn from_node_mut(node: &mut Node) -> Option<&mut Self>;
}

pub(crate) trait GetKind {
    fn get_kind() -> Kind;
}

macro_rules! define_node_and_kind {
    ($(($variant_name: ident, $ty: ty, $syn_type: ty)),*$(,)?) => {
        #[derive(Copy, Clone, Debug, PartialEq)]
        pub(crate) enum Kind {
            $(
                $variant_name,
            )*
        }

        #[derive(Clone)]
        pub(crate) enum Node {
            $(
                $variant_name($ty),
            )*
        }

        impl Kind {
            #[cfg(test)]
            pub(crate) fn all_kinds() -> Vec<Kind> {
                let mut items = vec![];
                $(
                    items.push(Self::$variant_name);
                )*
                items
            }
        }

        pub(crate) mod kinds {
            $(
                syn::custom_keyword!($variant_name);
            )*
        }

        impl syn::parse::Parse for Kind {
            fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
                $(
                    if input.peek(kinds::$variant_name) {
                        input.parse::<kinds::$variant_name>()?;
                        return Ok(Kind::$variant_name);
                    }
                )*
                Err(syn::Error::new(input.span(), format!("Invalid kind.")))
            }
        }

        impl Kind {
            pub(crate) fn to_placeholder_tokens(self, mangled_str: String) -> proc_macro2::TokenStream {
                use crate::mangle::ToPlaceholderTokens;
                use quote::ToTokens;
                let mut stream = proc_macro2::TokenStream::new();
                match self {
                    $(
                        Kind::$variant_name => <$syn_type>::to_placeholder_tokens(&mangled_str).to_tokens(&mut stream),
                    )*
                }
                stream
            }
        }

        $(
            impl AsNode for $ty {
                fn as_node(self) -> Node {
                    Node::$variant_name(self)
                }

                fn from_node(node: &Node) -> Option<&Self> {
                    if let Node::$variant_name(ref item) = node {
                        Some(item)
                    } else {
                        None
                    }
                }

                fn from_node_mut(node: &mut Node) -> Option<&mut Self> {
                    if let Node::$variant_name(ref mut item) = node {
                        Some(item)
                    } else {
                        None
                    }
                }
            }
        )*

        $(
            impl GetKind for $ty {
                fn get_kind() -> Kind {
                    Kind::$variant_name
                }
            }
        )*

        #[cfg(test)]
        pub(crate) fn unmangles_as_pattern(input: proc_macro2::TokenStream, kind: Kind) -> bool {
            use crate::mangle::{FromPlaceholder, Pattern};
            match kind {
                $(
                    Kind::$variant_name => {
                        let syn_type = syn::parse2::<$syn_type>(input).unwrap();
                        let pattern = syn_type.from_placeholder();
                        matches!(pattern, Pattern::Pattern(_))
                    },
                )*
            }
        }

    }
}

define_node_and_kind! {
    (Const, ItemConst, syn::ItemConst),
    (Ident, Ident, syn::Ident),
    (Expr, Expr, syn::Expr),
    (Lit, Lit, syn::Lit),
    (Item, Item, syn::Item),
    (ExprBinary, ExprBinary, syn::ExprBinary),
    (ExprUnary, ExprUnary, syn::ExprUnary),
    (ExprLit, ExprLit, syn::ExprLit),
}

pub type Ident = syn::Ident;
pub type Lit = syn::Lit;

#[allow(dead_code)]
#[derive(Clone)]
pub enum Item {
    /// A constant item: `const MAX: u16 = 65535`.
    Const(NodeId<ItemConst>),
    /// An enum definition: `enum Foo<A, B> { A(A), B(B) }`.
    Enum(syn::ItemEnum),
    /// An `extern crate` item: `extern crate serde`.
    ExternCrate(syn::ItemExternCrate),
    /// A free-standing function: `fn process(n: usize) -> Result<()> { ...
    /// }`.
    Fn(syn::ItemFn),
    /// A block of foreign items: `extern "C" { ... }`.
    ForeignMod(syn::ItemForeignMod),
    /// An impl block providing trait or associated items: `impl<A> Trait
    /// for Data<A> { ... }`.
    Impl(syn::ItemImpl),
    /// A macro invocation, which includes `macro_rules!` definitions.
    Macro(syn::ItemMacro),
    /// A module or module declaration: `mod m` or `mod m { ... }`.
    Mod(syn::ItemMod),
    /// A static item: `static BIKE: Shed = Shed(42)`.
    Static(syn::ItemStatic),
    /// A struct definition: `struct Foo<A> { x: A }`.
    Struct(syn::ItemStruct),
    /// A trait definition: `pub trait Iterator { ... }`.
    Trait(syn::ItemTrait),
    /// A trait alias: `pub trait SharableIterator = Iterator + Sync`.
    TraitAlias(syn::ItemTraitAlias),
    /// A type alias: `type Result<T> = std::result::Result<T, MyError>`.
    Type(syn::ItemType),
    /// A union definition: `union Foo<A, B> { x: A, y: B }`.
    Union(syn::ItemUnion),
    /// A use declaration: `use std::collections::HashMap`.
    Use(syn::ItemUse),
}

#[derive(Clone)]
pub(crate) struct ItemConst {
    pub _attrs: Vec<syn::Attribute>,
    pub _vis: syn::Visibility,
    pub ident: NodeId<Ident>,
    pub _generics: syn::Generics,
    pub _ty: Box<syn::Type>,
    pub expr: NodeId<Expr>,
}

#[derive(Clone)]
#[allow(dead_code)]
pub(crate) enum Expr {
    // supported
    /// A unary operation: `!x`, `*x`.
    Unary(NodeId<ExprUnary>),
    /// A binary operation: `a + b`, `a += b`.
    Binary(NodeId<ExprBinary>),
    /// A literal in place of an expression: `1`, `"foo"`.
    Lit(NodeId<ExprLit>),

    /// A slice literal expression: `[a, b, c, d]`.
    Array(syn::ExprArray),
    /// An assignment expression: `a = compute()`.
    Assign(syn::ExprAssign),
    /// An async block: `async { ... }`.
    Async(syn::ExprAsync),
    /// An await expression: `fut.await`.
    Await(syn::ExprAwait),
    /// A blocked scope: `{ ... }`.
    Block(syn::ExprBlock),
    /// A `break`, with an optional label to break and an optional
    /// expression.
    Break(syn::ExprBreak),
    /// A function call expression: `invoke(a, b)`.
    Call(syn::ExprCall),
    /// A cast expression: `foo as f64`.
    Cast(syn::ExprCast),
    /// A closure expression: `|a, b| a + b`.
    Closure(syn::ExprClosure),
    /// A const block: `const { ... }`.
    Const(syn::ExprConst),
    /// A `continue`, with an optional label.
    Continue(syn::ExprContinue),
    /// Access of a named struct field (`obj.k`) or unnamed tuple struct
    /// field (`obj.0`).
    Field(syn::ExprField),
    /// A for loop: `for pat in expr { ... }`.
    ForLoop(syn::ExprForLoop),
    /// An expression contained within invisible delimiters.
    /// This variant is important for faithfully representing the precedence
    /// of expressions and is related to `None`-delimited spans in a
    /// `TokenStream`.
    Group(syn::ExprGroup),
    /// An `if` expression with an optional `else` block: `if expr { ... }
    /// else { ... }`.
    /// The `else` branch expression may only be an `If` or `Block`
    /// expression, not any of the other types of expression.
    If(syn::ExprIf),
    /// A square bracketed indexing expression: `vector[2]`.
    Index(syn::ExprIndex),
    /// The inferred value of a const generic argument, denoted `_`.
    Infer(syn::ExprInfer),
    /// A `let` guard: `let Some(x) = opt`.
    Let(syn::ExprLet),
    /// Conditionless loop: `loop { ... }`.
    Loop(syn::ExprLoop),
    /// A macro invocation expression: `format!("{}", q)`.
    Macro(syn::ExprMacro),
    /// A `match` expression: `match n { Some(n) => {}, None => {} }`.
    Match(syn::ExprMatch),
    /// A method call expression: `x.foo::<T>(a, b)`.
    MethodCall(syn::ExprMethodCall),
    /// A parenthesized expression: `(a + b)`.
    Paren(syn::ExprParen),
    /// A path like `std::mem::replace` possibly containing generic
    /// parameters and a qualified self-type.
    /// A plain identifier like `x` is a path of length 1.
    Path(syn::ExprPath),
    /// A range expression: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
    Range(syn::ExprRange),
    /// Address-of operation: `&raw const place` or `&raw mut place`.
    RawAddr(syn::ExprRawAddr),
    /// A referencing operation: `&a` or `&mut a`.
    Reference(syn::ExprReference),
    /// An array literal constructed from one repeated element: `[0u8; N]`.
    Repeat(syn::ExprRepeat),
    /// A `return`, with an optional value to be returned.
    Return(syn::ExprReturn),
    /// A struct literal expression: `Point { x: 1, y: 1 }`.
    /// The `rest` provides the value of the remaining fields as in `S { a:
    /// 1, b: 1, ..rest }`.
    Struct(syn::ExprStruct),
    /// A try-expression: `expr?`.
    Try(syn::ExprTry),
    /// A try block: `try { ... }`.
    TryBlock(syn::ExprTryBlock),
    /// A tuple expression: `(a, b, c, d)`.
    Tuple(syn::ExprTuple),
    /// An unsafe block: `unsafe { ... }`.
    Unsafe(syn::ExprUnsafe),
    /// A while loop: `while expr { ... }`.
    While(syn::ExprWhile),
    /// A yield expression: `yield expr`.
    Yield(syn::ExprYield),
}

#[derive(Clone)]
pub struct ExprBinary {
    pub _attrs: Vec<syn::Attribute>,
    pub left: NodeId<Expr>,
    pub op: syn::BinOp,
    pub right: NodeId<Expr>,
}

#[derive(Clone)]
pub struct ExprUnary {
    pub _attrs: Vec<syn::Attribute>,
    pub op: syn::UnOp,
    pub expr: NodeId<Expr>,
}

#[derive(Clone)]
pub struct ExprLit {
    pub _attrs: Vec<syn::Attribute>,
    pub lit: NodeId<Lit>,
}
