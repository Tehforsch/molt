mod parse;

use syn::{Ident, Token};

use crate::parser::{Parse, Parser, Result};

pub(crate) struct RustFile {
    items: Vec<Item>,
}

pub enum Item {
    /// A constant item: `const MAX: u16 = 65535`.
    Const(ItemConst),

    /// An enum definition: `enum Foo<A, B> { A(A), B(B) }`.
    Enum(ItemEnum),

    /// An `extern crate` item: `extern crate serde`.
    ExternCrate(ItemExternCrate),

    /// A free-standing function: `fn process(n: usize) -> Result<()> { ...
    /// }`.
    Fn(ItemFn),

    /// A block of foreign items: `extern "C" { ... }`.
    ForeignMod(ItemForeignMod),

    /// An impl block providing trait or associated items: `impl<A> Trait
    /// for Data<A> { ... }`.
    Impl(ItemImpl),

    /// A macro invocation, which includes `macro_rules!` definitions.
    Macro(ItemMacro),

    /// A module or module declaration: `mod m` or `mod m { ... }`.
    Mod(ItemMod),

    /// A static item: `static BIKE: Shed = Shed(42)`.
    Static(ItemStatic),

    /// A struct definition: `struct Foo<A> { x: A }`.
    Struct(ItemStruct),

    /// A trait definition: `pub trait Iterator { ... }`.
    Trait(ItemTrait),

    /// A trait alias: `pub trait SharableIterator = Iterator + Sync`.
    TraitAlias(ItemTraitAlias),

    /// A type alias: `type Result<T> = std::result::Result<T, MyError>`.
    Type(ItemType),

    /// A union definition: `union Foo<A, B> { x: A, y: B }`.
    Union(ItemUnion),

    /// A use declaration: `use std::collections::HashMap`.
    Use(ItemUse),
}

pub struct ItemConst {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub const_token: Token![const],
    pub ident: Ident,
    pub colon_token: Token![:],
    pub ty: Box<Type>,
    pub eq_token: Token![=],
    pub expr: Box<Expr>,
    pub semi_token: Token![;],
}

macro_rules! impl_temp_struct {
    ($name: ident) => {
        pub struct $name;

        impl Parse for $name {
            fn parse(_: &Parser) -> Result<Self> {
                todo!()
            }
        }
    };
}

impl_temp_struct!(Attribute);
impl_temp_struct!(Visibility);
impl_temp_struct!(Generics);
impl_temp_struct!(Type);
impl_temp_struct!(Expr);

impl_temp_struct!(ItemEnum);
impl_temp_struct!(ItemExternCrate);
impl_temp_struct!(ItemFn);
impl_temp_struct!(ItemForeignMod);
impl_temp_struct!(ItemImpl);
impl_temp_struct!(ItemMacro);
impl_temp_struct!(ItemMod);
impl_temp_struct!(ItemStatic);
impl_temp_struct!(ItemStruct);
impl_temp_struct!(ItemTrait);
impl_temp_struct!(ItemTraitAlias);
impl_temp_struct!(ItemType);
impl_temp_struct!(ItemUnion);
impl_temp_struct!(ItemUse);
