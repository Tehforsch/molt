mod parse;

use derive_macro::GetDependencies;
use syn::{Token, ext::IdentExt};

use crate::{
    ctx::{NodeId, PatCtx},
    parser::{Parse, Parser, Result},
    resolve::{Dependencies, GetDependencies},
};

use super::{Kind, ParseStream, UntypedVar};

#[derive(Clone)]
pub struct Ident(syn::Ident);

#[derive(Clone)]
pub struct Lit(syn::Ident);

pub(crate) struct RustFile {
    items: Vec<NodeId<Item>>,
}

#[derive(GetDependencies)]
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

#[derive(GetDependencies)]
pub struct ItemConst {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub const_token: Token![const],
    pub ident: NodeId<Ident>,
    pub colon_token: Token![:],
    pub ty: Box<Type>,
    pub eq_token: Token![=],
    pub expr: Box<Expr>,
    pub semi_token: Token![;],
}

macro_rules! impl_temp_struct {
    ($name: ident) => {
        pub type $name = syn::$name;

        impl crate::resolve::GetDependencies for $name {
            fn get_dependencies(
                &self,
                ctx: &crate::ctx::PatCtx,
                deps: &mut crate::resolve::Dependencies,
            ) {
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

impl_temp_struct!(ExprLit);
impl_temp_struct!(ExprUnary);
impl_temp_struct!(ExprBinary);
impl_temp_struct!(ExprParen);

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_string() == other.0.to_string()
    }
}

impl std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}

impl Ident {
    fn parse_any(input: ParseStream) -> Result<NodeId<Self>> {
        if input.peek(Token![$]) {
            Ok(input.add_var_typed(input.parse::<UntypedVar>()?.to_var(Kind::Ident)))
        } else {
            let marker = input.span_marker();
            let ident = input.call_internal(syn::Ident::parse_any)?;
            let ident = input.make_spanned(marker, ident).map(Ident);
            Ok(input.add_item(ident))
        }
    }
}

impl GetDependencies for Ident {
    fn get_dependencies(&self, ctx: &PatCtx, deps: &mut Dependencies) {}
}

impl GetDependencies for syn::Lit {
    fn get_dependencies(&self, ctx: &PatCtx, deps: &mut Dependencies) {}
}
