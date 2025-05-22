use proc_macro2::Span;
use syn::spanned::Spanned;
use syn::{token, Token};

use crate::ctx::{MatchCtx, NodeId, NodeList};
use crate::match_pattern::{CmpDirect, Match};

pub(crate) trait ToNode {
    fn to_node(self) -> Node;
    fn from_node(node: &Node) -> Option<&Self>;
}

pub(crate) trait GetSpan {
    fn get_span(&self, ctx: &MatchCtx) -> Option<Span>;
}

pub(crate) trait GetKind {
    fn get_kind() -> Kind;
}

macro_rules! define_node_and_kind {
    ($(($variant_name: ident, $ty: ty, $syn_ty: ty)),*$(,)?) => {
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub(crate) enum Kind {
            $(
                $variant_name,
            )*
        }

        pub(crate) enum Node {
            $(
                $variant_name($ty),
            )*
        }

        $(
            impl GetKind for $ty {
                fn get_kind() -> Kind {
                    Kind::$variant_name
                }
            }
        )*

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
                use crate::mangle::Mangle;
                use quote::ToTokens;
                let mut stream = proc_macro2::TokenStream::new();
                match self {
                    $(
                        Kind::$variant_name => <$syn_ty>::mangle(&mangled_str).to_tokens(&mut stream),
                    )*
                }
                stream
            }

            pub(crate) fn from_str(s: &str) -> Self {
                $(
                    if s == stringify!($variant_name) {
                        return Self::$variant_name;
                    }
                )*
                panic!();
            }

        }

        impl std::fmt::Display for Kind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$variant_name => write!(f, stringify!($variant_name)),
                    )*
                }
            }
        }

        $(
            impl ToNode for $ty {
                fn to_node(self) -> Node {
                    Node::$variant_name(self)
                }

                fn from_node(node: &Node) -> Option<&Self> {
                    if let Node::$variant_name(ref item) = node {
                        Some(item)
                    } else {
                        None
                    }
                }
            }
        )*

        impl Node {
            pub(crate) fn kind(&self) -> Kind {
                match self {
                    $(
                        Self::$variant_name(_) => Kind::$variant_name,
                    )*
                }
            }

            pub(crate) fn cmp_equal_kinds(ctx: &mut Match, ast: &Self, pat: &Self) {
                assert_eq!(ast.kind(), pat.kind());
                match ast {
                    $(
                        Node::$variant_name(s) => s.cmp_direct(ctx, <$ty>::from_node(pat).unwrap()),
                    )*
                }
            }
        }

        impl CustomDebug for Node {
            fn deb(&self, ctx: &MatchCtx) -> String {
                match self {
                    $(
                        Self::$variant_name(s) => s.deb(ctx),
                    )*
                }
            }
        }

        impl GetSpan for Node {
            fn get_span(&self, ctx: &MatchCtx) -> Option<Span> {
                match self {
                    $(
                        Self::$variant_name(s) => s.get_span(ctx),
                    )*
                }
            }
        }

        #[cfg(test)]
        pub(crate) fn unmangle_pattern_var_name(input: proc_macro2::TokenStream, kind: Kind) -> Option<String> {
            use crate::mangle::{Unmangle, Pattern};
            match kind {
                $(
                    Kind::$variant_name => {
                        let syn_type = syn::parse2::<$syn_ty>(input).unwrap();
                        let pattern = syn_type.unmangle();
                        if let Pattern::Pattern(var) = pattern {
                            Some(var.name)
                        }
                        else {
                            None
                        }
                    },
                )*
            }
        }


    }
}

define_node_and_kind! {
    (Ident, Ident, syn::Ident),
    (Expr, Expr, syn::Expr),
    (Lit, Lit, syn::Lit),
    (Item, Item, syn::Item),
    (Signature, Signature, syn::Signature),
    (FnArg, FnArg, syn::FnArg),
}

pub type Ident = syn::Ident;
pub type Lit = syn::Lit;

#[allow(dead_code)]
pub enum Item {
    // Supported
    /// A constant item: `const MAX: u16 = 65535`.
    Const(ItemConst),
    /// A free-standing function: `fn process(n: usize) -> Result<()> { ...
    /// }`.
    Fn(ItemFn),

    /// An enum definition: `enum Foo<A, B> { A(A), B(B) }`.
    Enum(syn::ItemEnum),
    /// An `extern crate` item: `extern crate serde`.
    ExternCrate(syn::ItemExternCrate),
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

pub(crate) struct ItemConst {
    pub _attrs: Vec<syn::Attribute>,
    pub _vis: syn::Visibility,
    pub ident: NodeId<Ident>,
    pub _generics: syn::Generics,
    pub _ty: Box<syn::Type>,
    pub expr: NodeId<Expr>,
    pub const_token: Token![const],
    pub semi_token: Token![;],
}

pub struct ItemFn {
    pub _attrs: Vec<syn::Attribute>,
    pub vis: syn::Visibility,
    pub sig: NodeId<Signature>,
    pub block: Box<syn::Block>,
}

pub struct Signature {
    pub constness: Option<Token![const]>,
    pub asyncness: Option<Token![async]>,
    pub unsafety: Option<Token![unsafe]>,
    pub _abi: Option<syn::Abi>,
    pub fn_token: Token![fn],
    pub ident: NodeId<Ident>,
    pub generics: syn::Generics,
    pub _paren_token: token::Paren,
    pub inputs: NodeList<FnArg>,
    pub _variadic: Option<syn::Variadic>,
    pub output: syn::ReturnType,
}

pub struct FnArg {
    pub ident: NodeId<Ident>,
}

// pub struct Receiver {
//     pub attrs: Vec<syn::Attribute>,
//     pub reference: Option<(Token![&], Option<syn::Lifetime>)>,
//     pub mutability: Option<Token![mut]>,
//     pub self_token: Token![self],
//     pub colon_token: Option<Token![:]>,
//     pub ty: Box<syn::Type>,
// }

// pub struct PatType {
//     pub attrs: Vec<syn::Attribute>,
//     pub pat: Box<syn::Pat>,
//     pub colon_token: Token![:],
//     pub ty: Box<syn::Type>,
// }

#[allow(dead_code)]
pub(crate) enum Expr {
    // supported
    /// A unary operation: `!x`, `*x`.
    Unary(ExprUnary),
    /// A binary operation: `a + b`, `a += b`.
    Binary(ExprBinary),
    /// A literal in place of an expression: `1`, `"foo"`.
    Lit(ExprLit),

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

pub struct ExprBinary {
    pub _attrs: Vec<syn::Attribute>,
    pub left: NodeId<Expr>,
    pub op: syn::BinOp,
    pub right: NodeId<Expr>,
}

pub struct ExprUnary {
    pub _attrs: Vec<syn::Attribute>,
    pub op: syn::UnOp,
    pub expr: NodeId<Expr>,
}

pub struct ExprLit {
    pub _attrs: Vec<syn::Attribute>,
    pub lit: NodeId<Lit>,
}

pub(crate) trait CustomDebug {
    fn deb(&self, ctx: &MatchCtx) -> String;
}

macro_rules! impl_deb_syn_type {
    ($ty: ty) => {
        impl CustomDebug for $ty {
            fn deb(&self, _: &MatchCtx) -> String {
                quote::quote! { #self }.to_string()
            }
        }

        impl GetSpan for $ty {
            fn get_span(&self, _: &MatchCtx) -> Option<Span> {
                Some(<Self as Spanned>::span(self))
            }
        }
    };
}

impl_deb_syn_type!(Lit);
impl_deb_syn_type!(Ident);

impl_deb_syn_type!(syn::ItemEnum);
impl_deb_syn_type!(syn::ItemExternCrate);
impl_deb_syn_type!(syn::ItemFn);
impl_deb_syn_type!(syn::ItemForeignMod);
impl_deb_syn_type!(syn::ItemImpl);
impl_deb_syn_type!(syn::ItemMacro);
impl_deb_syn_type!(syn::ItemMod);
impl_deb_syn_type!(syn::ItemStatic);
impl_deb_syn_type!(syn::ItemStruct);
impl_deb_syn_type!(syn::ItemTrait);
impl_deb_syn_type!(syn::ItemUse);
impl_deb_syn_type!(syn::ItemUnion);
impl_deb_syn_type!(syn::ItemType);
impl_deb_syn_type!(syn::ItemTraitAlias);
impl_deb_syn_type!(syn::BinOp);
impl_deb_syn_type!(syn::UnOp);
impl_deb_syn_type!(syn::ExprArray);
impl_deb_syn_type!(syn::ExprAssign);
impl_deb_syn_type!(syn::ExprAsync);
impl_deb_syn_type!(syn::ExprAwait);
impl_deb_syn_type!(syn::ExprBlock);
impl_deb_syn_type!(syn::ExprBreak);
impl_deb_syn_type!(syn::ExprCall);
impl_deb_syn_type!(syn::ExprCast);
impl_deb_syn_type!(syn::ExprClosure);
impl_deb_syn_type!(syn::ExprConst);
impl_deb_syn_type!(syn::ExprContinue);
impl_deb_syn_type!(syn::ExprField);
impl_deb_syn_type!(syn::ExprForLoop);
impl_deb_syn_type!(syn::ExprGroup);
impl_deb_syn_type!(syn::ExprIf);
impl_deb_syn_type!(syn::ExprIndex);
impl_deb_syn_type!(syn::ExprInfer);
impl_deb_syn_type!(syn::ExprLet);
impl_deb_syn_type!(syn::ExprLoop);
impl_deb_syn_type!(syn::ExprMacro);
impl_deb_syn_type!(syn::ExprMatch);
impl_deb_syn_type!(syn::ExprMethodCall);
impl_deb_syn_type!(syn::ExprParen);
impl_deb_syn_type!(syn::ExprPath);
impl_deb_syn_type!(syn::ExprRange);
impl_deb_syn_type!(syn::ExprRawAddr);
impl_deb_syn_type!(syn::ExprReference);
impl_deb_syn_type!(syn::ExprRepeat);
impl_deb_syn_type!(syn::ExprReturn);
impl_deb_syn_type!(syn::ExprStruct);
impl_deb_syn_type!(syn::ExprTry);
impl_deb_syn_type!(syn::ExprTryBlock);
impl_deb_syn_type!(syn::ExprTuple);
impl_deb_syn_type!(syn::ExprUnsafe);
impl_deb_syn_type!(syn::ExprWhile);
impl_deb_syn_type!(syn::ExprYield);
impl_deb_syn_type!(syn::Type);
impl_deb_syn_type!(syn::Visibility);
impl_deb_syn_type!(syn::ReturnType);
impl_deb_syn_type!(syn::Generics);
impl_deb_syn_type!(syn::Block);
impl_deb_syn_type!(syn::FnArg);

impl_deb_syn_type!(Token![;]);
impl_deb_syn_type!(Token![const]);
impl_deb_syn_type!(Token![async]);
impl_deb_syn_type!(Token![unsafe]);
impl_deb_syn_type!(Token![fn]);

macro_rules! impl_deb_custom_type {
    ($ty: ty, $fmt_str: literal, ($($ident: ident),*$(,)?)) => {
        impl CustomDebug for $ty {
            fn deb(&self, ctx: &MatchCtx) -> String {
                format!(
                    $fmt_str,
                    $(
                        self.$ident.deb(ctx),
                    )*
                )
            }
        }

        impl GetSpan for $ty {
            fn get_span(&self, ctx: &MatchCtx) -> Option<Span> {
                let mut span: Option<Span> = None;
                $(
                    let new_span = self.$ident.get_span(ctx);
                    if let Some(new_span) = new_span {
                        // TODO: wow this is ugly, but otherwise
                        // we get weird spans
                        if new_span.byte_range() != (0..0) {
                            if let Some(ref mut span) = span {
                                if let Some(joined) = span.join(new_span) {
                                    *span = joined
                                }
                            } else {
                                span = Some(new_span);
                            }
                        }
                    }
                )*
                span
            }
        }
    }
}

impl_deb_custom_type!(ExprLit, "{}", (lit));
impl_deb_custom_type!(ExprUnary, "{}{}", (op, expr));
impl_deb_custom_type!(ExprBinary, "({} {} {})", (left, op, right));
impl_deb_custom_type!(
    ItemConst,
    "{} {}: {} = {}{}",
    (const_token, ident, _ty, expr, semi_token)
);
impl_deb_custom_type!(ItemFn, "{}{} {}", (vis, sig, block));
impl_deb_custom_type!(
    Signature,
    "{} {} {} ({}) {}",
    (fn_token, ident, generics, inputs, output)
);
impl_deb_custom_type!(FnArg, "{}: FooType", (ident));

macro_rules! impl_deb_enum {
    ($ty: ty, ($($ident: ident),*$(,)?)) => {
        impl CustomDebug for $ty {
            fn deb(&self, ctx: &MatchCtx) -> String {
                match self {
                    $(
                        Self::$ident(s) => s.deb(ctx),
                    )*
                }
            }
        }

        impl GetSpan for $ty {
            fn get_span(&self, ctx: &MatchCtx) -> Option<Span> {
                match self {
                    $(
                        Self::$ident(s) => s.get_span(ctx),
                    )*
                }
            }
        }
    };
}

impl_deb_enum!(
    Item,
    (
        Const,
        Enum,
        ExternCrate,
        Fn,
        ForeignMod,
        Impl,
        Macro,
        Mod,
        Static,
        Struct,
        Trait,
        TraitAlias,
        Type,
        Union,
        Use,
    )
);

impl_deb_enum!(
    Expr,
    (
        Unary, Binary, Lit, Array, Assign, Async, Await, Block, Break, Call, Cast, Closure, Const,
        Continue, Field, ForLoop, Group, If, Index, Infer, Let, Loop, Macro, Match, MethodCall,
        Paren, Path, Range, RawAddr, Reference, Repeat, Return, Struct, Try, TryBlock, Tuple,
        Unsafe, While, Yield,
    )
);

impl<T: CustomDebug + ToNode> CustomDebug for NodeId<T> {
    fn deb(&self, ctx: &MatchCtx) -> String {
        match ctx.get(*self) {
            crate::mangle::Pattern::Exact(t) => t.deb(ctx),
            crate::mangle::Pattern::Pattern(var) => format!("${}", var.name),
        }
    }
}

impl<T: GetSpan + ToNode> GetSpan for NodeId<T> {
    fn get_span(&self, ctx: &MatchCtx) -> Option<Span> {
        match ctx.get(*self) {
            crate::mangle::Pattern::Exact(t) => t.get_span(ctx),
            crate::mangle::Pattern::Pattern(_) => unimplemented!(),
        }
    }
}

impl<T: CustomDebug + ToNode> CustomDebug for NodeList<T> {
    fn deb(&self, ctx: &MatchCtx) -> String {
        self.items
            .iter()
            .map(|item| item.deb(ctx))
            .collect::<Vec<_>>()
            .join(", ")
    }
}

impl<T: GetSpan + ToNode> GetSpan for NodeList<T> {
    fn get_span(&self, ctx: &MatchCtx) -> Option<Span> {
        self.items
            .iter()
            .map(|item| item.get_span(ctx))
            .fold(None, |acc, span| {
                if let Some(span) = span {
                    acc.map_or(Some(span), |acc: Span| acc.join(span))
                } else {
                    acc
                }
            })
    }
}

impl<T, P> GetSpan for syn::punctuated::Punctuated<T, P> {
    fn get_span(&self, _: &MatchCtx) -> Option<Span> {
        // TODO, but not sure how
        None
    }
}

impl<T: CustomDebug, P> CustomDebug for syn::punctuated::Punctuated<T, P> {
    fn deb(&self, ctx: &MatchCtx) -> String {
        // very much todo
        self.iter()
            .map(|item| item.deb(ctx))
            .collect::<Vec<_>>()
            .join(", ")
    }
}

impl GetKind for syn::Expr {
    fn get_kind() -> Kind {
        Kind::Expr
    }
}

impl GetKind for syn::Item {
    fn get_kind() -> Kind {
        Kind::Item
    }
}

impl GetKind for syn::Signature {
    fn get_kind() -> Kind {
        Kind::Signature
    }
}

impl GetKind for syn::FnArg {
    fn get_kind() -> Kind {
        Kind::FnArg
    }
}
