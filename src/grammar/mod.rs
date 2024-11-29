mod ast;
mod convert;

use syn::{Token, Type};

pub use ast::{Ast, PatternAst};

pub(crate) enum Item {
    // /// A constant item: `const MAX: u16 = 65535`.
    // Const(ItemConst),

    // /// An enum definition: `enum Foo<A, B> { A(A), B(B) }`.
    // Enum(ItemEnum),

    // /// An `extern crate` item: `extern crate serde`.
    // ExternCrate(ItemExternCrate),
    /// A free-standing function: `fn process(n: usize) -> Result<()> { ...
    /// }`.
    Fn(ItemFn),
    // /// A block of foreign items: `extern "C" { ... }`.
    // ForeignMod(ItemForeignMod),

    // /// An impl block providing trait or associated items: `impl<A> Trait
    // /// for Data<A> { ... }`.
    // Impl(ItemImpl),

    // /// A macro invocation, which includes `macro_rules!` definitions.
    // Macro(ItemMacro),

    // /// A module or module declaration: `mod m` or `mod m { ... }`.
    // Mod(ItemMod),

    // /// A static item: `static BIKE: Shed = Shed(42)`.
    // Static(ItemStatic),

    // /// A struct definition: `struct Foo<A> { x: A }`.
    // Struct(ItemStruct),

    // /// A trait definition: `pub trait Iterator { ... }`.
    // Trait(ItemTrait),

    // /// A trait alias: `pub trait SharableIterator = Iterator + Sync`.
    // TraitAlias(ItemTraitAlias),

    // /// A type alias: `type Result<T> = std::result::Result<T, MyError>`.
    // Type(ItemType),

    // /// A union definition: `union Foo<A, B> { x: A, y: B }`.
    // Union(ItemUnion),

    // /// A use declaration: `use std::collections::HashMap`.
    // Use(ItemUse),

    // /// Tokens forming an item not interpreted by Syn.
    // Verbatim(TokenStream),
}

pub enum Ident {
    Exact(syn::Ident),
    Pattern(String),
}

pub struct ItemFn {
    pub attrs: Vec<syn::Attribute>,
    pub vis: syn::Visibility,
    pub sig: Signature,
    pub block: Box<syn::Block>,
}

pub struct Signature {
    pub constness: Option<Token![const]>,
    pub asyncness: Option<Token![async]>,
    pub unsafety: Option<Token![unsafe]>,
    pub abi: Option<syn::Abi>,
    pub fn_token: Token![fn],
    pub ident: Ident,
    pub generics: syn::Generics,
    pub paren_token: syn::token::Paren,
    pub inputs: syn::punctuated::Punctuated<syn::FnArg, Token![,]>,
    pub variadic: Option<syn::Variadic>,
    pub output: syn::ReturnType,
}
