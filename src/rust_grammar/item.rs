use std::mem;

use crate::{CmpSyn, NodeId, NodeList, Pattern, SpannedPat, WithSpan, rule};
use derive_macro::CmpSyn;
use proc_macro2::TokenStream;

use crate::parser::error::{Error, Result};
use crate::parser::parse::ParseList;
use crate::parser::parse::discouraged::Speculative as _;
use crate::parser::parse::{Parse, ParseBuffer, ParseNode, ParseStream};
use crate::parser::punctuated::Punctuated;
use crate::parser::token;
use crate::rust_grammar::attr::{self, Attribute};
use crate::rust_grammar::data::{Fields, FieldsNamed, Variant};
use crate::rust_grammar::expr::Expr;
use crate::rust_grammar::generics::{Generics, TypeParamBound};
use crate::rust_grammar::ident::{AnyIdent, Ident, TokenIdent};
use crate::rust_grammar::lifetime::Lifetime;
use crate::rust_grammar::lit::LitStr;
use crate::rust_grammar::mac::{
    Macro, {self},
};
use crate::rust_grammar::pat::{Pat, PatSingle, PatType};
use crate::rust_grammar::path::Path;
use crate::rust_grammar::restriction::Vis;
use crate::rust_grammar::stmt::Block;
use crate::rust_grammar::ty::{Abi, ReturnType, Type, TypePath};
use crate::rust_grammar::{derive, verbatim};

use super::generics::WhereClause;
use super::restriction::{IsInherited, IsSome};

#[derive(Debug, CmpSyn)]
#[requires_rule]
pub enum Unsafety {
    Unsafe,
    Safe,
}

impl Unsafety {
    fn is_none(&self) -> bool {
        matches!(self, Unsafety::Safe)
    }

    fn is_some(&self) -> bool {
        matches!(self, Unsafety::Unsafe)
    }
}

#[derive(Debug, CmpSyn)]
#[requires_rule]
pub enum Asyncness {
    Async,
    Sync,
}

#[derive(Debug, CmpSyn)]
#[requires_rule]
pub enum Constness {
    Const,
    NotConst,
}

#[derive(Debug, CmpSyn)]
/// Things that can appear directly inside of a module or scope.
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

    /// Tokens forming an item that cannot be parsed properly
    Verbatim(TokenStream),
}

#[derive(Debug, CmpSyn)]
/// A constant item: `const MAX: u16 = 65535`.
pub struct ItemConst {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Const)]
    pub vis: NodeId<Vis>,
    pub const_token: Token![const],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub colon_token: Token![:],
    pub ty: NodeId<Type>,
    pub eq_token: Token![=],
    pub expr: NodeId<Expr>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// An enum definition: `enum Foo<A, B> { A(A), B(B) }`.
pub struct ItemEnum {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Enum)]
    pub vis: NodeId<Vis>,
    pub enum_token: Token![enum],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub brace_token: token::Brace,
    pub variants: Punctuated<Variant, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// An `extern crate` item: `extern crate serde`.
pub struct ItemExternCrate {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, ExternCrate)]
    pub vis: NodeId<Vis>,
    pub extern_token: Token![extern],
    pub crate_token: Token![crate],
    pub ident: NodeId<Ident>,
    pub rename: Option<(Token![as], NodeId<Ident>)>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// A free-standing function: `fn process(n: usize) -> Result<()> { ... }`.
pub struct ItemFn {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Fn)]
    pub vis: NodeId<Vis>,
    pub sig: Signature,
    pub block: Box<Block>,
}

#[derive(Debug, CmpSyn)]
/// A block of foreign items: `extern "C" { ... }`.
pub struct ItemForeignMod {
    pub attrs: Vec<Attribute>,
    #[rule(Unsafe, Mod)]
    pub unsafety: Unsafety,
    pub abi: Abi,
    pub brace_token: token::Brace,
    pub items: Vec<ForeignItem>,
}

#[derive(Debug, CmpSyn)]
/// An impl block providing trait or associated items: `impl<A> Trait
/// for Data<A> { ... }`.
pub struct ItemImpl {
    pub attrs: Vec<Attribute>,
    pub defaultness: Option<Token![default]>,
    #[rule(Unsafe, Impl)]
    pub unsafety: Unsafety,
    pub impl_token: Token![impl],
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    /// Trait this impl implements.
    pub trait_: Option<(Option<Token![!]>, Path, Token![for])>,
    /// The Self type of the impl.
    pub self_ty: NodeId<Type>,
    pub brace_token: token::Brace,
    pub items: NodeList<ImplItem, Token![;]>,
}

#[derive(Debug, CmpSyn)]
/// A macro invocation, which includes `macro_rules!` definitions.
pub struct ItemMacro {
    pub attrs: Vec<Attribute>,
    /// The `example` in `macro_rules! example { ... }`.
    pub ident: Option<NodeId<Ident>>,
    pub mac: Macro,
    pub semi_token: Option<Token![;]>,
}

#[derive(Debug, CmpSyn)]
/// A module or module declaration: `mod m` or `mod m { ... }`.
pub struct ItemMod {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Mod)]
    pub vis: NodeId<Vis>,
    #[rule(Unsafe, Mod)]
    pub unsafety: Unsafety,
    pub mod_token: Token![mod],
    pub ident: NodeId<Ident>,
    pub content: Option<(token::Brace, Vec<Item>)>,
    pub semi: Option<Token![;]>,
}

#[derive(Debug, CmpSyn)]
/// A static item: `static BIKE: Shed = Shed(42)`.
pub struct ItemStatic {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Static)]
    pub vis: NodeId<Vis>,
    pub static_token: Token![static],
    pub mutability: StaticMutability,
    pub ident: NodeId<Ident>,
    pub colon_token: Token![:],
    pub ty: NodeId<Type>,
    pub eq_token: Token![=],
    pub expr: NodeId<Expr>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// A struct definition: `struct Foo<A> { x: A }`.
pub struct ItemStruct {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Struct)]
    pub vis: NodeId<Vis>,
    pub struct_token: Token![struct],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub fields: Fields,
    pub semi_token: Option<Token![;]>,
}

#[derive(Debug, CmpSyn)]
/// A trait definition: `pub trait Iterator { ... }`.
pub struct ItemTrait {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Trait)]
    pub vis: NodeId<Vis>,
    #[rule(Unsafe, Trait)]
    pub unsafety: Unsafety,
    pub auto_token: Option<Token![auto]>,
    pub trait_token: Token![trait],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub colon_token: Option<Token![:]>,
    pub supertraits: Punctuated<TypeParamBound, Token![+]>,
    pub brace_token: token::Brace,
    pub items: Vec<TraitItem>,
}

#[derive(Debug, CmpSyn)]
/// A trait alias: `pub trait SharableIterator = Iterator + Sync`.
pub struct ItemTraitAlias {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, TraitAlias)]
    pub vis: NodeId<Vis>,
    pub trait_token: Token![trait],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub eq_token: Token![=],
    pub bounds: Punctuated<TypeParamBound, Token![+]>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// A type alias: `type Result<T> = std::result::Result<T, MyError>`.
pub struct ItemType {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Type)]
    pub vis: NodeId<Vis>,
    pub type_token: Token![type],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub eq_token: Token![=],
    pub ty: NodeId<Type>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// A union definition: `union Foo<A, B> { x: A, y: B }`.
pub struct ItemUnion {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Union)]
    pub vis: NodeId<Vis>,
    pub union_token: Token![union],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub fields: FieldsNamed,
}

#[derive(Debug, CmpSyn)]
/// A use declaration: `use std::collections::HashMap`.
pub struct ItemUse {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Use)]
    pub vis: NodeId<Vis>,
    pub use_token: Token![use],
    pub leading_colon: Option<Token![::]>,
    pub tree: UseTree,
    pub semi_token: Token![;],
}

impl Item {
    fn replace_attrs(&mut self, new: Vec<Attribute>) -> Vec<Attribute> {
        match self {
            Item::Const(ItemConst { attrs, .. })
            | Item::Enum(ItemEnum { attrs, .. })
            | Item::ExternCrate(ItemExternCrate { attrs, .. })
            | Item::Fn(ItemFn { attrs, .. })
            | Item::ForeignMod(ItemForeignMod { attrs, .. })
            | Item::Impl(ItemImpl { attrs, .. })
            | Item::Macro(ItemMacro { attrs, .. })
            | Item::Mod(ItemMod { attrs, .. })
            | Item::Static(ItemStatic { attrs, .. })
            | Item::Struct(ItemStruct { attrs, .. })
            | Item::Trait(ItemTrait { attrs, .. })
            | Item::TraitAlias(ItemTraitAlias { attrs, .. })
            | Item::Type(ItemType { attrs, .. })
            | Item::Union(ItemUnion { attrs, .. })
            | Item::Use(ItemUse { attrs, .. }) => mem::replace(attrs, new),
            Item::Verbatim(_) => Vec::new(),
        }
    }
}

#[derive(Debug, CmpSyn)]
/// A suffix of an import tree in a `use` item: `Type as Renamed` or `*`.
pub enum UseTree {
    /// A path prefix of imports in a `use` item: `std::...`.
    Path(UsePath),

    /// An identifier imported by a `use` item: `HashMap`.
    Name(UseName),

    /// An renamed identifier imported by a `use` item: `HashMap as Map`.
    Rename(UseRename),

    /// A glob import in a `use` item: `*`.
    Glob(UseGlob),

    /// A braced group of imports in a `use` item: `{A, B, C}`.
    Group(UseGroup),
}

#[derive(Debug, CmpSyn)]
/// A path prefix of imports in a `use` item: `std::...`.
pub struct UsePath {
    pub ident: NodeId<Ident>,
    pub colon2_token: Token![::],
    pub tree: Box<UseTree>,
}

#[derive(Debug, CmpSyn)]
/// An identifier imported by a `use` item: `HashMap`.
pub struct UseName {
    pub ident: NodeId<Ident>,
}

#[derive(Debug, CmpSyn)]
/// An renamed identifier imported by a `use` item: `HashMap as Map`.
pub struct UseRename {
    pub ident: NodeId<Ident>,
    pub as_token: Token![as],
    pub rename: Ident,
}

#[derive(Debug, CmpSyn)]
/// A glob import in a `use` item: `*`.
pub struct UseGlob {
    pub star_token: Token![*],
}

#[derive(Debug, CmpSyn)]
/// A braced group of imports in a `use` item: `{A, B, C}`.
pub struct UseGroup {
    pub brace_token: token::Brace,
    pub items: Punctuated<UseTree, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// An item within an `extern` block.
pub enum ForeignItem {
    /// A foreign function in an `extern` block.
    Fn(ForeignItemFn),

    /// A foreign static item in an `extern` block: `static ext: u8`.
    Static(ForeignItemStatic),

    /// A foreign type in an `extern` block: `type void`.
    Type(ForeignItemType),

    /// A macro invocation within an extern block.
    Macro(ForeignItemMacro),

    /// Tokens in an `extern` block not interpreted by Syn.
    Verbatim(TokenStream),
}

#[derive(Debug, CmpSyn)]
/// A foreign function in an `extern` block.
pub struct ForeignItemFn {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Fn)]
    pub vis: NodeId<Vis>,
    pub sig: Signature,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// A foreign static item in an `extern` block: `static ext: u8`.
pub struct ForeignItemStatic {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Static)]
    pub vis: NodeId<Vis>,
    pub static_token: Token![static],
    pub mutability: StaticMutability,
    pub ident: NodeId<Ident>,
    pub colon_token: Token![:],
    pub ty: NodeId<Type>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// A foreign type in an `extern` block: `type void`.
pub struct ForeignItemType {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Type)]
    pub vis: NodeId<Vis>,
    pub type_token: Token![type],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// A macro invocation within an extern block.
pub struct ForeignItemMacro {
    pub attrs: Vec<Attribute>,
    pub mac: Macro,
    pub semi_token: Option<Token![;]>,
}

#[derive(Debug, CmpSyn)]
/// An item declaration within the definition of a trait.
pub enum TraitItem {
    /// An associated constant within the definition of a trait.
    Const(TraitItemConst),

    /// An associated function within the definition of a trait.
    Fn(TraitItemFn),

    /// An associated type within the definition of a trait.
    Type(TraitItemType),

    /// A macro invocation within the definition of a trait.
    Macro(TraitItemMacro),

    /// Tokens within the definition of a trait not interpreted by Syn.
    Verbatim(TokenStream),
}

#[derive(Debug, CmpSyn)]
/// An associated constant within the definition of a trait.
pub struct TraitItemConst {
    pub attrs: Vec<Attribute>,
    pub const_token: Token![const],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub colon_token: Token![:],
    pub ty: NodeId<Type>,
    pub default: Option<(Token![=], NodeId<Expr>)>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// An associated function within the definition of a trait.
pub struct TraitItemFn {
    pub attrs: Vec<Attribute>,
    pub sig: Signature,
    pub default: Option<Block>,
    pub semi_token: Option<Token![;]>,
}

#[derive(Debug, CmpSyn)]
/// An associated type within the definition of a trait.
pub struct TraitItemType {
    pub attrs: Vec<Attribute>,
    pub type_token: Token![type],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub colon_token: Option<Token![:]>,
    pub bounds: Punctuated<TypeParamBound, Token![+]>,
    pub default: Option<(Token![=], NodeId<Type>)>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// A macro invocation within the definition of a trait.
pub struct TraitItemMacro {
    pub attrs: Vec<Attribute>,
    pub mac: Macro,
    pub semi_token: Option<Token![;]>,
}

#[derive(Debug, CmpSyn)]
/// An item within an impl block.
pub enum ImplItem {
    /// An associated constant within an impl block.
    Const(ImplItemConst),

    /// An associated function within an impl block.
    Fn(ImplItemFn),

    /// An associated type within an impl block.
    Type(ImplItemType),

    /// A macro invocation within an impl block.
    Macro(ImplItemMacro),

    /// Tokens within an impl block not interpreted by Syn.
    Verbatim(TokenStream),
}

#[derive(Debug, CmpSyn)]
/// An associated constant within an impl block.
pub struct ImplItemConst {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Const)]
    pub vis: NodeId<Vis>,
    pub defaultness: Option<Token![default]>,
    pub const_token: Token![const],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub colon_token: Token![:],
    pub ty: NodeId<Type>,
    pub eq_token: Token![=],
    pub expr: NodeId<Expr>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// An associated function within an impl block.
pub struct ImplItemFn {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Fn)]
    pub vis: NodeId<Vis>,
    pub defaultness: Option<Token![default]>,
    pub sig: Signature,
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// An associated type within an impl block.
pub struct ImplItemType {
    pub attrs: Vec<Attribute>,
    #[rule(Vis, Type)]
    pub vis: NodeId<Vis>,
    pub defaultness: Option<Token![default]>,
    pub type_token: Token![type],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub eq_token: Token![=],
    pub ty: NodeId<Type>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// A macro invocation within an impl block.
pub struct ImplItemMacro {
    pub attrs: Vec<Attribute>,
    pub mac: Macro,
    pub semi_token: Option<Token![;]>,
}

#[derive(Debug, CmpSyn)]
/// A function signature in a trait or implementation: `unsafe fn
/// initialize(&self)`.
pub struct Signature {
    #[rule(Const, Fn)]
    pub constness: Constness,
    #[rule(Async, Fn)]
    pub asyncness: Asyncness,
    #[rule(Unsafe, Fn)]
    pub unsafety: Unsafety,
    pub abi: Option<Abi>,
    pub fn_token: Token![fn],
    pub ident: NodeId<Ident>,
    pub generics: NodeId<Generics>,
    pub where_clause: Option<WhereClause>,
    pub paren_token: token::Paren,
    pub inputs: Punctuated<FnArg, Token![,]>,
    pub variadic: Option<Variadic>,
    pub output: ReturnType,
}

impl Signature {
    /// A method's `self` receiver, such as `&self` or `self: Box<Self>`.
    pub fn receiver(&self) -> Option<&Receiver> {
        let arg = self.inputs.first()?;
        match arg {
            FnArg::Receiver(receiver) => Some(receiver),
            FnArg::Typed(_) => None,
        }
    }
}

#[derive(Debug, CmpSyn)]
/// An argument in a function signature: the `n: usize` in `fn f(n: usize)`.
pub enum FnArg {
    /// The `self` argument of an associated method.
    Receiver(Receiver),

    /// A function argument accepted by pattern and type.
    Typed(PatType),
}

#[derive(Debug, CmpSyn)]
/// The `self` argument of an associated method.
///
/// If `colon_token` is present, the receiver is written with an explicit
/// type such as `self: Box<Self>`. If `colon_token` is absent, the receiver
/// is written in shorthand such as `self` or `&self` or `&mut self`. In the
/// shorthand case, the type in `ty` is reconstructed as one of `Self`,
/// `&Self`, or `&mut Self`.
pub struct Receiver {
    pub attrs: Vec<Attribute>,
    pub reference: Option<(Token![&], Option<Lifetime>)>,
    pub mutability: Option<Token![mut]>,
    pub self_token: Token![self],
    pub colon_token: Option<Token![:]>,
    pub ty: Option<NodeId<Type>>,
}

#[derive(Debug, CmpSyn)]
/// The variadic argument of a foreign function.
pub struct Variadic {
    pub attrs: Vec<Attribute>,
    pub pat: Option<(NodeId<Pat>, Token![:])>,
    pub dots: Token![...],
    pub comma: Option<Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// The mutability of an `Item::Static` or `ForeignItem::Static`.
pub enum StaticMutability {
    Mut(Token![mut]),
    None,
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        let begin = input.fork();
        let attrs = input.call(Attribute::parse_outer)?;
        parse_rest_of_item(begin, attrs, input)
    }
}

impl ParseNode for Item {
    type Target = Item;

    fn parse_node(input: ParseStream) -> Result<Item> {
        Item::parse(input)
    }
}

pub(crate) fn parse_rest_of_item(
    begin: ParseBuffer,
    mut attrs: Vec<Attribute>,
    input: ParseStream,
) -> Result<Item> {
    let ahead = input.fork();
    let vis: SpannedPat<Vis> = ahead.parse_spanned_pat::<Vis>()?;

    let lookahead = ahead.lookahead1();
    let allow_safe = false;
    let mut item = if lookahead.peek(Token![fn]) || peek_signature(&ahead, allow_safe) {
        let vis = input.parse_id::<Vis>()?;
        let sig: Signature = input.parse()?;
        if input.peek(Token![;]) {
            input.parse::<Token![;]>()?;
            Ok(Item::Verbatim(verbatim::between(&begin, input)))
        } else {
            parse_rest_of_fn(input, Vec::new(), vis, sig).map(Item::Fn)
        }
    } else if lookahead.peek(Token![extern]) {
        ahead.parse::<Token![extern]>()?;
        let lookahead = ahead.lookahead1();
        if lookahead.peek(Token![crate]) {
            input.parse().map(Item::ExternCrate)
        } else if lookahead.peek(token::Brace) {
            input.parse().map(Item::ForeignMod)
        } else if lookahead.peek(LitStr) {
            ahead.parse::<LitStr>()?;
            let lookahead = ahead.lookahead1();
            if lookahead.peek(token::Brace) {
                input.parse().map(Item::ForeignMod)
            } else {
                Err(lookahead.error())
            }
        } else {
            Err(lookahead.error())
        }
    } else if lookahead.peek(Token![use]) {
        let allow_crate_root_in_path = true;
        match parse_item_use(input, allow_crate_root_in_path)? {
            Some(item_use) => Ok(Item::Use(item_use)),
            None => Ok(Item::Verbatim(verbatim::between(&begin, input))),
        }
    } else if lookahead.peek(Token![static]) {
        let vis = input.parse_id::<Vis>()?;
        let static_token = input.parse()?;
        let mutability = input.parse()?;
        let ident = input.parse()?;
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            input.parse::<NodeId<Expr>>()?;
            input.parse::<Token![;]>()?;
            Ok(Item::Verbatim(verbatim::between(&begin, input)))
        } else {
            let colon_token = input.parse()?;
            let ty = input.parse()?;
            if input.peek(Token![;]) {
                input.parse::<Token![;]>()?;
                Ok(Item::Verbatim(verbatim::between(&begin, input)))
            } else {
                Ok(Item::Static(ItemStatic {
                    attrs: Vec::new(),
                    vis,
                    static_token,
                    mutability,
                    ident,
                    colon_token,
                    ty,
                    eq_token: input.parse()?,
                    expr: input.parse()?,
                    semi_token: input.parse()?,
                }))
            }
        }
    } else if lookahead.peek(Token![const]) {
        let vis = input.parse_id::<Vis>()?;
        let const_token: Token![const] = input.parse()?;
        let lookahead = input.lookahead1();
        let ident = if lookahead.peek_pat::<Ident>() || lookahead.peek(Token![_]) {
            input.parse_id::<AnyIdent>()?
        } else {
            return Err(lookahead.error());
        };
        let generics = input.parse_spanned_pat::<Generics>()?;
        let colon_token = input.parse()?;
        let ty = input.parse()?;
        let value = if let Some(eq_token) = input.parse::<Option<Token![=]>>()? {
            let expr: NodeId<Expr> = input.parse()?;
            Some((eq_token, expr))
        } else {
            None
        };
        let where_clause = input.parse::<Option<WhereClause>>()?;
        let semi_token: Token![;] = input.parse()?;
        match value {
            Some((eq_token, expr)) if !generics.get_property(IsSome) && where_clause.is_none() => {
                Ok(Item::Const(ItemConst {
                    attrs: Vec::new(),
                    vis,
                    const_token,
                    ident,
                    generics: input.add_pat(generics),
                    where_clause,
                    colon_token,
                    ty,
                    eq_token,
                    expr,
                    semi_token,
                }))
            }
            _ => Ok(Item::Verbatim(verbatim::between(&begin, input))),
        }
    } else if lookahead.peek(Token![unsafe]) {
        ahead.parse::<Token![unsafe]>()?;
        let lookahead = ahead.lookahead1();
        if lookahead.peek(Token![trait])
            || lookahead.peek(Token![auto]) && ahead.peek2(Token![trait])
        {
            input.parse().map(Item::Trait)
        } else if lookahead.peek(Token![impl]) {
            let allow_verbatim_impl = true;
            if let Some(item) = parse_impl(input, allow_verbatim_impl)? {
                Ok(Item::Impl(item))
            } else {
                Ok(Item::Verbatim(verbatim::between(&begin, input)))
            }
        } else if lookahead.peek(Token![extern]) {
            input.parse().map(Item::ForeignMod)
        } else if lookahead.peek(Token![mod]) {
            input.parse().map(Item::Mod)
        } else {
            Err(lookahead.error())
        }
    } else if lookahead.peek(Token![mod]) {
        input.parse().map(Item::Mod)
    } else if lookahead.peek(Token![type]) {
        parse_item_type(begin, input)
    } else if lookahead.peek(Token![struct]) {
        input.parse().map(Item::Struct)
    } else if lookahead.peek(Token![enum]) {
        input.parse().map(Item::Enum)
    } else if lookahead.peek(Token![union]) && ahead.peek2(Ident) {
        input.parse().map(Item::Union)
    } else if lookahead.peek(Token![trait]) {
        input.call(parse_trait_or_trait_alias)
    } else if lookahead.peek(Token![auto]) && ahead.peek2(Token![trait]) {
        input.parse().map(Item::Trait)
    } else if lookahead.peek(Token![impl])
        || lookahead.peek(Token![default]) && !ahead.peek2(Token![!])
    {
        let allow_verbatim_impl = true;
        if let Some(item) = parse_impl(input, allow_verbatim_impl)? {
            Ok(Item::Impl(item))
        } else {
            Ok(Item::Verbatim(verbatim::between(&begin, input)))
        }
    } else if lookahead.peek(Token![macro]) {
        input.advance_to(&ahead);
        let vis = input.parse_id::<Vis>()?;
        parse_macro2(begin, vis, input)
    } else if vis.get_property(IsInherited)
        && (lookahead.peek_pat::<Ident>()
            || lookahead.peek(Token![self])
            || lookahead.peek(Token![super])
            || lookahead.peek(Token![crate])
            || lookahead.peek(Token![::]))
    {
        input.parse().map(Item::Macro)
    } else {
        Err(lookahead.error())
    }?;

    attrs.extend(item.replace_attrs(Vec::new()));
    item.replace_attrs(attrs);
    Ok(item)
}

struct FlexibleItemType {
    vis: NodeId<Vis>,
    defaultness: Option<Token![default]>,
    type_token: Token![type],
    ident: NodeId<Ident>,
    generics: NodeId<Generics>,
    where_clause: Option<WhereClause>,
    colon_token: Option<Token![:]>,
    bounds: Punctuated<TypeParamBound, Token![+]>,
    ty: Option<(Token![=], NodeId<Type>)>,
    semi_token: Token![;],
}

enum TypeDefaultness {
    Optional,
    Disallowed,
}

enum WhereClauseLocation {
    // type Ty<T> where T: 'static = T;
    BeforeEq,
    // type Ty<T> = T where T: 'static;
    AfterEq,
    // TODO: goes away once the migration period on rust-lang/rust#89122 is over
    Both,
}

impl FlexibleItemType {
    fn parse(
        input: ParseStream,
        allow_defaultness: TypeDefaultness,
        where_clause_location: WhereClauseLocation,
    ) -> Result<Self> {
        let vis = input.parse_id::<Vis>()?;
        let defaultness: Option<Token![default]> = match allow_defaultness {
            TypeDefaultness::Optional => input.parse()?,
            TypeDefaultness::Disallowed => None,
        };
        let type_token: Token![type] = input.parse()?;
        let ident: NodeId<Ident> = input.parse()?;
        let generics = input.parse::<NodeId<Generics>>()?;
        let (colon_token, bounds) = Self::parse_optional_bounds(input)?;

        let (where_clause, ty) = match where_clause_location {
            WhereClauseLocation::BeforeEq => {
                let where_clause = input.parse::<Option<WhereClause>>()?;
                let ty = Self::parse_optional_definition(input)?;
                (where_clause, ty)
            }
            WhereClauseLocation::AfterEq => {
                let ty = Self::parse_optional_definition(input)?;
                let where_clause = input.parse::<Option<WhereClause>>()?;
                (where_clause, ty)
            }
            WhereClauseLocation::Both => {
                let where_clause = input.parse::<Option<WhereClause>>()?;
                let ty = Self::parse_optional_definition(input)?;
                let where_clause = if where_clause.is_none() {
                    input.parse::<Option<WhereClause>>()?
                } else {
                    where_clause
                };
                (where_clause, ty)
            }
        };

        let semi_token: Token![;] = input.parse()?;

        Ok(FlexibleItemType {
            vis,
            defaultness,
            type_token,
            ident,
            generics,
            where_clause,
            colon_token,
            bounds,
            ty,
            semi_token,
        })
    }

    fn parse_optional_bounds(
        input: ParseStream,
    ) -> Result<(Option<Token![:]>, Punctuated<TypeParamBound, Token![+]>)> {
        let colon_token: Option<Token![:]> = input.parse()?;

        let mut bounds = Punctuated::new();
        if colon_token.is_some() {
            loop {
                if input.peek(Token![where]) || input.peek(Token![=]) || input.peek(Token![;]) {
                    break;
                }
                bounds.push_value({
                    let allow_precise_capture = false;
                    let allow_tilde_const = true;
                    TypeParamBound::parse_single(input, allow_precise_capture, allow_tilde_const)?
                });
                if input.peek(Token![where]) || input.peek(Token![=]) || input.peek(Token![;]) {
                    break;
                }
                bounds.push_punct(input.parse::<Token![+]>()?);
            }
        }

        Ok((colon_token, bounds))
    }

    fn parse_optional_definition(input: ParseStream) -> Result<Option<(Token![=], NodeId<Type>)>> {
        let eq_token: Option<Token![=]> = input.parse()?;
        if let Some(eq_token) = eq_token {
            let definition = input.parse()?;
            Ok(Some((eq_token, definition)))
        } else {
            Ok(None)
        }
    }
}

impl Parse for ItemMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let path = input.call(Path::parse_mod_style)?;
        let bang_token: Token![!] = input.parse()?;
        let ident: Option<NodeId<Ident>> = if input.peek(Token![try]) {
            input.parse_id::<AnyIdent>().map(Some)
        } else {
            input.parse()
        }?;
        let (delimiter, tokens) = input.call(mac::parse_delimiter)?;
        let semi_token: Option<Token![;]> = if !delimiter.is_brace() {
            Some(input.parse()?)
        } else {
            None
        };
        Ok(ItemMacro {
            attrs,
            ident,
            mac: Macro {
                path,
                bang_token,
                delimiter,
                tokens,
            },
            semi_token,
        })
    }
}

fn parse_macro2(begin: ParseBuffer, _vis: NodeId<Vis>, input: ParseStream) -> Result<Item> {
    input.parse::<Token![macro]>()?;
    input.parse::<NodeId<Ident>>()?;

    let mut lookahead = input.lookahead1();
    if lookahead.peek(token::Paren) {
        let paren_content;
        parenthesized!(paren_content in input);
        paren_content.parse::<TokenStream>()?;
        lookahead = input.lookahead1();
    }

    if lookahead.peek(token::Brace) {
        let brace_content;
        braced!(brace_content in input);
        brace_content.parse::<TokenStream>()?;
    } else {
        return Err(lookahead.error());
    }

    Ok(Item::Verbatim(verbatim::between(&begin, input)))
}

impl Parse for ItemExternCrate {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ItemExternCrate {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            extern_token: input.parse()?,
            crate_token: input.parse()?,
            ident: {
                if input.peek(Token![self]) {
                    input.parse_id::<AnyIdent>()?
                } else {
                    input.parse()?
                }
            },
            rename: {
                if input.peek(Token![as]) {
                    let as_token: Token![as] = input.parse()?;
                    let rename: NodeId<Ident> = if input.peek(Token![_]) {
                        input.parse_id::<TokenIdent<Token![_]>>()?
                    } else {
                        input.parse()?
                    };
                    Some((as_token, rename))
                } else {
                    None
                }
            },
            semi_token: input.parse()?,
        })
    }
}

impl Parse for ItemUse {
    fn parse(input: ParseStream) -> Result<Self> {
        let allow_crate_root_in_path = false;
        parse_item_use(input, allow_crate_root_in_path).map(Option::unwrap)
    }
}

fn parse_item_use(input: ParseStream, allow_crate_root_in_path: bool) -> Result<Option<ItemUse>> {
    let attrs = input.call(Attribute::parse_outer)?;
    let vis = input.parse_id::<Vis>()?;
    let use_token: Token![use] = input.parse()?;
    let leading_colon: Option<Token![::]> = input.parse()?;
    let tree = parse_use_tree(input, allow_crate_root_in_path && leading_colon.is_none())?;
    let semi_token: Token![;] = input.parse()?;

    let tree = match tree {
        Some(tree) => tree,
        None => return Ok(None),
    };

    Ok(Some(ItemUse {
        attrs,
        vis,
        use_token,
        leading_colon,
        tree,
        semi_token,
    }))
}

impl Parse for UseTree {
    fn parse(input: ParseStream) -> Result<UseTree> {
        let allow_crate_root_in_path = false;
        parse_use_tree(input, allow_crate_root_in_path).map(Option::unwrap)
    }
}

fn parse_use_tree(input: ParseStream, allow_crate_root_in_path: bool) -> Result<Option<UseTree>> {
    let lookahead = input.lookahead1();
    if lookahead.peek_pat::<Ident>()
        || lookahead.peek(Token![self])
        || lookahead.peek(Token![super])
        || lookahead.peek(Token![crate])
        || lookahead.peek(Token![try])
    {
        let ident = input.parse_id::<AnyIdent>()?;
        if input.peek(Token![::]) {
            Ok(Some(UseTree::Path(UsePath {
                ident,
                colon2_token: input.parse()?,
                tree: Box::new(input.parse()?),
            })))
        } else if input.peek(Token![as]) {
            Ok(Some(UseTree::Rename(UseRename {
                ident,
                as_token: input.parse()?,
                rename: {
                    if input.peek_pat::<Ident>() {
                        input.parse()?
                    } else if input.peek(Token![_]) {
                        Ident::from(input.parse::<Token![_]>()?)
                    } else {
                        return Err(input.error("expected identifier or underscore"));
                    }
                },
            })))
        } else {
            Ok(Some(UseTree::Name(UseName { ident })))
        }
    } else if lookahead.peek(Token![*]) {
        Ok(Some(UseTree::Glob(UseGlob {
            star_token: input.parse()?,
        })))
    } else if lookahead.peek(token::Brace) {
        let content;
        let brace_token = braced!(content in input);
        let mut items = Punctuated::new();
        let mut has_any_crate_root_in_path = false;
        loop {
            if content.is_empty() {
                break;
            }
            let this_tree_starts_with_crate_root =
                allow_crate_root_in_path && content.parse::<Option<Token![::]>>()?.is_some();
            has_any_crate_root_in_path |= this_tree_starts_with_crate_root;
            match parse_use_tree(
                &content,
                allow_crate_root_in_path && !this_tree_starts_with_crate_root,
            )? {
                Some(tree) if !has_any_crate_root_in_path => items.push_value(tree),
                _ => has_any_crate_root_in_path = true,
            }
            if content.is_empty() {
                break;
            }
            let comma: Token![,] = content.parse()?;
            if !has_any_crate_root_in_path {
                items.push_punct(comma);
            }
        }
        if has_any_crate_root_in_path {
            Ok(None)
        } else {
            Ok(Some(UseTree::Group(UseGroup { brace_token, items })))
        }
    } else {
        Err(lookahead.error())
    }
}

impl Parse for ItemStatic {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ItemStatic {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            static_token: input.parse()?,
            mutability: input.parse()?,
            ident: input.parse()?,
            colon_token: input.parse()?,
            ty: input.parse()?,
            eq_token: input.parse()?,
            expr: input.parse()?,
            semi_token: input.parse()?,
        })
    }
}

impl Parse for ItemConst {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let const_token: Token![const] = input.parse()?;

        let lookahead = input.lookahead1();
        let ident = if lookahead.peek_pat::<Ident>() || lookahead.peek(Token![_]) {
            input.parse_id::<AnyIdent>()?
        } else {
            return Err(lookahead.error());
        };

        let colon_token: Token![:] = input.parse()?;
        let ty = input.parse()?;
        let generics = input.make_at_point(Generics::default());
        let eq_token: Token![=] = input.parse()?;
        let expr: NodeId<Expr> = input.parse()?;
        let semi_token: Token![;] = input.parse()?;

        Ok(ItemConst {
            attrs,
            vis,
            const_token,
            ident,
            generics: input.add_pat(generics),
            where_clause: None,
            colon_token,
            ty,
            eq_token,
            expr,
            semi_token,
        })
    }
}

fn peek_signature(input: ParseStream, allow_safe: bool) -> bool {
    let fork = input.fork();
    fork.parse::<Constness>().is_ok()
        && fork.parse::<Asyncness>().is_ok()
        && ((allow_safe
            && token::peek_keyword(fork.cursor(), "safe")
            && token::keyword(&fork, "safe").is_ok())
            || fork.parse::<Unsafety>().is_ok())
        && fork.parse::<Option<Abi>>().is_ok()
        && fork.peek(Token![fn])
}

impl Parse for Signature {
    fn parse(input: ParseStream) -> Result<Self> {
        let allow_safe = false;
        parse_signature(input, allow_safe).map(Option::unwrap)
    }
}

fn parse_signature(input: ParseStream, allow_safe: bool) -> Result<Option<Signature>> {
    let constness: Constness = input.parse()?;
    let asyncness: Asyncness = input.parse()?;
    let unsafety: Unsafety = input.parse()?;
    let safe = allow_safe && unsafety.is_none() && token::peek_keyword(input.cursor(), "safe");
    if safe {
        token::keyword(input, "safe")?;
    }
    let abi: Option<Abi> = input.parse()?;
    let fn_token: Token![fn] = input.parse()?;
    let ident = input.parse_id::<Ident>()?;
    let generics = input.parse::<NodeId<Generics>>()?;

    let content;
    let paren_token = parenthesized!(content in input);
    let (inputs, variadic) = parse_fn_args(&content)?;

    let output: ReturnType = input.parse()?;
    let where_clause = input.parse::<Option<WhereClause>>()?;

    Ok(if safe {
        None
    } else {
        Some(Signature {
            constness,
            asyncness,
            unsafety,
            abi,
            fn_token,
            ident,
            generics,
            where_clause,
            paren_token,
            inputs,
            variadic,
            output,
        })
    })
}

impl Parse for ItemFn {
    fn parse(input: ParseStream) -> Result<Self> {
        let outer_attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let sig: Signature = input.parse()?;
        parse_rest_of_fn(input, outer_attrs, vis, sig)
    }
}

fn parse_rest_of_fn(
    input: ParseStream,
    mut attrs: Vec<Attribute>,
    vis: NodeId<Vis>,
    sig: Signature,
) -> Result<ItemFn> {
    let content;
    let brace_token = braced!(content in input);
    attr::parse_inner(&content, &mut attrs)?;
    let stmts = content.parse_list::<Block>()?;

    Ok(ItemFn {
        attrs,
        vis,
        sig,
        block: Box::new(Block { brace_token, stmts }),
    })
}

impl Parse for FnArg {
    fn parse(input: ParseStream) -> Result<Self> {
        let allow_variadic = false;
        let attrs = input.call(Attribute::parse_outer)?;
        match parse_fn_arg_or_variadic(input, attrs, allow_variadic)? {
            FnArgOrVariadic::FnArg(arg) => Ok(arg),
            FnArgOrVariadic::Variadic(_) => unreachable!(),
        }
    }
}

enum FnArgOrVariadic {
    FnArg(FnArg),
    Variadic(Variadic),
}

fn parse_fn_arg_or_variadic(
    input: ParseStream,
    attrs: Vec<Attribute>,
    allow_variadic: bool,
) -> Result<FnArgOrVariadic> {
    let ahead = input.fork();
    if let Ok(mut receiver) = ahead.parse::<Receiver>() {
        input.advance_to(&ahead);
        receiver.attrs = attrs;
        return Ok(FnArgOrVariadic::FnArg(FnArg::Receiver(receiver)));
    }

    let pat = input.parse_id::<PatSingle>()?;
    let colon_token: Token![:] = input.parse()?;

    if allow_variadic {
        if let Some(dots) = input.parse::<Option<Token![...]>>()? {
            return Ok(FnArgOrVariadic::Variadic(Variadic {
                attrs,
                pat: Some((pat, colon_token)),
                dots,
                comma: None,
            }));
        }
    }

    Ok(FnArgOrVariadic::FnArg(FnArg::Typed(PatType {
        attrs,
        pat,
        colon_token,
        ty: input.parse()?,
    })))
}

impl Parse for Receiver {
    fn parse(input: ParseStream) -> Result<Self> {
        let reference = if input.peek(Token![&]) {
            let ampersand: Token![&] = input.parse()?;
            let lifetime: Option<Lifetime> = input.parse()?;
            Some((ampersand, lifetime))
        } else {
            None
        };
        let mutability: Option<Token![mut]> = input.parse()?;
        let self_token: Token![self] = input.parse()?;
        let colon_token: Option<Token![:]> = if reference.is_some() {
            None
        } else {
            input.parse()?
        };
        let ty: Option<NodeId<Type>> = if colon_token.is_some() {
            Some(input.parse()?)
        } else {
            // This differs from the syn impl which adds
            // an "artificial" Self/&Self type. Here, we
            // make the type optional and simply don't set it
            // for normal `self`/`&self`/`&mut self` receivers.
            None
        };
        Ok(Receiver {
            attrs: Vec::new(),
            reference,
            mutability,
            self_token,
            colon_token,
            ty,
        })
    }
}

fn parse_fn_args(input: ParseStream) -> Result<(Punctuated<FnArg, Token![,]>, Option<Variadic>)> {
    let mut args = Punctuated::new();
    let mut variadic = None;
    let mut has_receiver = false;

    while !input.is_empty() {
        let attrs = input.call(Attribute::parse_outer)?;

        if let Some(dots) = input.parse::<Option<Token![...]>>()? {
            variadic = Some(Variadic {
                attrs,
                pat: None,
                dots,
                comma: if input.is_empty() {
                    None
                } else {
                    Some(input.parse()?)
                },
            });
            break;
        }

        let allow_variadic = true;
        let arg = match parse_fn_arg_or_variadic(input, attrs, allow_variadic)? {
            FnArgOrVariadic::FnArg(arg) => arg,
            FnArgOrVariadic::Variadic(arg) => {
                variadic = Some(Variadic {
                    comma: if input.is_empty() {
                        None
                    } else {
                        Some(input.parse()?)
                    },
                    ..arg
                });
                break;
            }
        };

        match &arg {
            FnArg::Receiver(receiver) if has_receiver => {
                return Err(Error::new(
                    receiver.self_token.span,
                    "unexpected second method receiver",
                ));
            }
            FnArg::Receiver(receiver) if !args.is_empty() => {
                return Err(Error::new(
                    receiver.self_token.span,
                    "unexpected method receiver",
                ));
            }
            FnArg::Receiver(_) => has_receiver = true,
            FnArg::Typed(_) => {}
        }
        args.push_value(arg);

        if input.is_empty() {
            break;
        }

        let comma: Token![,] = input.parse()?;
        args.push_punct(comma);
    }

    Ok((args, variadic))
}

impl Parse for ItemMod {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let unsafety: Unsafety = input.parse()?;
        let mod_token: Token![mod] = input.parse()?;
        let ident: NodeId<Ident> = if input.peek(Token![try]) {
            input.parse_id::<AnyIdent>()
        } else {
            input.parse()
        }?;

        let lookahead = input.lookahead1();
        if lookahead.peek(Token![;]) {
            Ok(ItemMod {
                attrs,
                vis,
                unsafety,
                mod_token,
                ident,
                content: None,
                semi: Some(input.parse()?),
            })
        } else if lookahead.peek(token::Brace) {
            let content;
            let brace_token = braced!(content in input);
            attr::parse_inner(&content, &mut attrs)?;

            let mut items = Vec::new();
            while !content.is_empty() {
                items.push(content.parse()?);
            }

            Ok(ItemMod {
                attrs,
                vis,
                unsafety,
                mod_token,
                ident,
                content: Some((brace_token, items)),
                semi: None,
            })
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for ItemForeignMod {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let unsafety: Unsafety = input.parse()?;
        let abi: Abi = input.parse()?;

        let content;
        let brace_token = braced!(content in input);
        attr::parse_inner(&content, &mut attrs)?;
        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }

        Ok(ItemForeignMod {
            attrs,
            unsafety,
            abi,
            brace_token,
            items,
        })
    }
}

impl Parse for ForeignItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let begin = input.fork();
        let mut attrs = input.call(Attribute::parse_outer)?;
        let ahead = input.fork();
        let vis: SpannedPat<Vis> = ahead.parse_spanned_pat::<Vis>()?;

        let lookahead = ahead.lookahead1();
        let allow_safe = true;
        let mut item = if lookahead.peek(Token![fn]) || peek_signature(&ahead, allow_safe) {
            let vis = input.parse_id::<Vis>()?;
            let sig = parse_signature(input, allow_safe)?;
            let has_safe = sig.is_none();
            let has_body = input.peek(token::Brace);
            let semi_token: Option<Token![;]> = if has_body {
                let content;
                braced!(content in input);
                content.call(Attribute::parse_inner)?;
                content.parse_list::<Block>()?;
                None
            } else {
                Some(input.parse()?)
            };
            if has_safe || has_body {
                Ok(ForeignItem::Verbatim(verbatim::between(&begin, input)))
            } else {
                Ok(ForeignItem::Fn(ForeignItemFn {
                    attrs: Vec::new(),
                    vis,
                    sig: sig.unwrap(),
                    semi_token: semi_token.unwrap(),
                }))
            }
        } else if lookahead.peek(Token![static])
            || ((ahead.peek(Token![unsafe]) || token::peek_keyword(ahead.cursor(), "safe"))
                && ahead.peek2(Token![static]))
        {
            let vis = input.parse_id::<Vis>()?;
            let unsafety: Unsafety = input.parse()?;
            let safe = unsafety.is_none() && token::peek_keyword(input.cursor(), "safe");
            if safe {
                token::keyword(input, "safe")?;
            }
            let static_token = input.parse()?;
            let mutability = input.parse()?;
            let ident = input.parse()?;
            let colon_token = input.parse()?;
            let ty = input.parse()?;
            let has_value = input.peek(Token![=]);
            if has_value {
                input.parse::<Token![=]>()?;
                input.parse::<NodeId<Expr>>()?;
            }
            let semi_token: Token![;] = input.parse()?;
            if unsafety.is_some() || safe || has_value {
                Ok(ForeignItem::Verbatim(verbatim::between(&begin, input)))
            } else {
                Ok(ForeignItem::Static(ForeignItemStatic {
                    attrs: Vec::new(),
                    vis,
                    static_token,
                    mutability,
                    ident,
                    colon_token,
                    ty,
                    semi_token,
                }))
            }
        } else if lookahead.peek(Token![type]) {
            parse_foreign_item_type(begin, input)
        } else if vis.get_property(IsInherited)
            && (lookahead.peek_pat::<Ident>()
                || lookahead.peek(Token![self])
                || lookahead.peek(Token![super])
                || lookahead.peek(Token![crate])
                || lookahead.peek(Token![::]))
        {
            input.parse().map(ForeignItem::Macro)
        } else {
            Err(lookahead.error())
        }?;

        let item_attrs = match &mut item {
            ForeignItem::Fn(item) => &mut item.attrs,
            ForeignItem::Static(item) => &mut item.attrs,
            ForeignItem::Type(item) => &mut item.attrs,
            ForeignItem::Macro(item) => &mut item.attrs,
            ForeignItem::Verbatim(_) => return Ok(item),
        };
        attrs.append(item_attrs);
        *item_attrs = attrs;

        Ok(item)
    }
}

impl Parse for ForeignItemFn {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let sig: Signature = input.parse()?;
        let semi_token: Token![;] = input.parse()?;
        Ok(ForeignItemFn {
            attrs,
            vis,
            sig,
            semi_token,
        })
    }
}

impl Parse for ForeignItemStatic {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ForeignItemStatic {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            static_token: input.parse()?,
            mutability: input.parse()?,
            ident: input.parse()?,
            colon_token: input.parse()?,
            ty: input.parse()?,
            semi_token: input.parse()?,
        })
    }
}

impl Parse for ForeignItemType {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ForeignItemType {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            type_token: input.parse()?,
            ident: input.parse()?,
            generics: input.parse()?,
            where_clause: input.parse::<Option<WhereClause>>()?,
            semi_token: input.parse()?,
        })
    }
}

fn parse_foreign_item_type(begin: ParseBuffer, input: ParseStream) -> Result<ForeignItem> {
    let FlexibleItemType {
        vis,
        defaultness: _,
        type_token,
        ident,
        generics,
        where_clause,
        colon_token,
        bounds: _,
        ty,
        semi_token,
    } = FlexibleItemType::parse(
        input,
        TypeDefaultness::Disallowed,
        WhereClauseLocation::Both,
    )?;

    if colon_token.is_some() || ty.is_some() {
        Ok(ForeignItem::Verbatim(verbatim::between(&begin, input)))
    } else {
        Ok(ForeignItem::Type(ForeignItemType {
            attrs: Vec::new(),
            vis,
            type_token,
            ident,
            generics,
            where_clause,
            semi_token,
        }))
    }
}

impl Parse for ForeignItemMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let mac: Macro = input.parse()?;
        let semi_token: Option<Token![;]> = if mac.delimiter.is_brace() {
            None
        } else {
            Some(input.parse()?)
        };
        Ok(ForeignItemMacro {
            attrs,
            mac,
            semi_token,
        })
    }
}

impl Parse for ItemType {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ItemType {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            type_token: input.parse()?,
            ident: input.parse()?,
            generics: input.parse()?,
            where_clause: input.parse::<Option<WhereClause>>()?,
            eq_token: input.parse()?,
            ty: input.parse()?,
            semi_token: input.parse()?,
        })
    }
}

fn parse_item_type(begin: ParseBuffer, input: ParseStream) -> Result<Item> {
    let FlexibleItemType {
        vis,
        defaultness: _,
        type_token,
        ident,
        generics,
        where_clause,
        colon_token,
        bounds: _,
        ty,
        semi_token,
    } = FlexibleItemType::parse(
        input,
        TypeDefaultness::Disallowed,
        WhereClauseLocation::BeforeEq,
    )?;

    let (eq_token, ty) = match ty {
        Some(ty) if colon_token.is_none() => ty,
        _ => return Ok(Item::Verbatim(verbatim::between(&begin, input))),
    };

    Ok(Item::Type(ItemType {
        attrs: Vec::new(),
        vis,
        type_token,
        ident,
        generics,
        where_clause,
        eq_token,
        ty,
        semi_token,
    }))
}

impl Parse for ItemStruct {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let struct_token = input.parse::<Token![struct]>()?;
        let ident = input.parse_id::<Ident>()?;
        let generics = input.parse::<NodeId<Generics>>()?;
        let (where_clause, fields, semi_token) = derive::data_struct(input)?;
        Ok(ItemStruct {
            attrs,
            vis,
            struct_token,
            ident,
            generics,
            where_clause,
            fields,
            semi_token,
        })
    }
}

impl Parse for ItemEnum {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let enum_token = input.parse::<Token![enum]>()?;
        let ident = input.parse_id::<Ident>()?;
        let generics = input.parse::<NodeId<Generics>>()?;
        let (where_clause, brace_token, variants) = derive::data_enum(input)?;
        Ok(ItemEnum {
            attrs,
            vis,
            enum_token,
            ident,
            generics,
            where_clause,
            brace_token,
            variants,
        })
    }
}

impl Parse for ItemUnion {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let union_token = input.parse::<Token![union]>()?;
        let ident = input.parse_id::<Ident>()?;
        let generics = input.parse::<NodeId<Generics>>()?;
        let (where_clause, fields) = derive::data_union(input)?;
        Ok(ItemUnion {
            attrs,
            vis,
            union_token,
            ident,
            generics,
            where_clause,
            fields,
        })
    }
}

fn parse_trait_or_trait_alias(input: ParseStream) -> Result<Item> {
    let (attrs, vis, trait_token, ident, generics) = parse_start_of_trait_alias(input)?;
    let lookahead = input.lookahead1();
    if lookahead.peek(token::Brace) || lookahead.peek(Token![:]) || lookahead.peek(Token![where]) {
        let unsafety = Unsafety::Safe;
        let auto_token = None;
        parse_rest_of_trait(
            input,
            attrs,
            vis,
            unsafety,
            auto_token,
            trait_token,
            ident,
            generics,
        )
        .map(Item::Trait)
    } else if lookahead.peek(Token![=]) {
        parse_rest_of_trait_alias(input, attrs, vis, trait_token, ident, generics)
            .map(Item::TraitAlias)
    } else {
        Err(lookahead.error())
    }
}

impl Parse for ItemTrait {
    fn parse(input: ParseStream) -> Result<Self> {
        let outer_attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let unsafety: Unsafety = input.parse()?;
        let auto_token: Option<Token![auto]> = input.parse()?;
        let trait_token: Token![trait] = input.parse()?;
        let ident = input.parse_id::<Ident>()?;
        let generics = input.parse_id::<Generics>()?;
        parse_rest_of_trait(
            input,
            outer_attrs,
            vis,
            unsafety,
            auto_token,
            trait_token,
            ident,
            generics,
        )
    }
}

#[allow(clippy::too_many_arguments)]
fn parse_rest_of_trait(
    input: ParseStream,
    mut attrs: Vec<Attribute>,
    vis: NodeId<Vis>,
    unsafety: Unsafety,
    auto_token: Option<Token![auto]>,
    trait_token: Token![trait],
    ident: NodeId<Ident>,
    generics: NodeId<Generics>,
) -> Result<ItemTrait> {
    let colon_token: Option<Token![:]> = input.parse()?;

    let mut supertraits = Punctuated::new();
    if colon_token.is_some() {
        loop {
            if input.peek(Token![where]) || input.peek(token::Brace) {
                break;
            }
            supertraits.push_value({
                let allow_precise_capture = false;
                let allow_tilde_const = true;
                TypeParamBound::parse_single(input, allow_precise_capture, allow_tilde_const)?
            });
            if input.peek(Token![where]) || input.peek(token::Brace) {
                break;
            }
            supertraits.push_punct(input.parse()?);
        }
    }

    let where_clause = input.parse::<Option<WhereClause>>()?;

    let content;
    let brace_token = braced!(content in input);
    attr::parse_inner(&content, &mut attrs)?;
    let mut items = Vec::new();
    while !content.is_empty() {
        items.push(content.parse()?);
    }

    Ok(ItemTrait {
        attrs,
        vis,
        unsafety,
        auto_token,
        trait_token,
        ident,
        generics,
        where_clause,
        colon_token,
        supertraits,
        brace_token,
        items,
    })
}

impl Parse for ItemTraitAlias {
    fn parse(input: ParseStream) -> Result<Self> {
        let (attrs, vis, trait_token, ident, generics) = parse_start_of_trait_alias(input)?;
        parse_rest_of_trait_alias(input, attrs, vis, trait_token, ident, generics)
    }
}

#[allow(clippy::type_complexity)]
fn parse_start_of_trait_alias(
    input: ParseStream,
) -> Result<(
    Vec<Attribute>,
    NodeId<Vis>,
    Token![trait],
    NodeId<Ident>,
    NodeId<Generics>,
)> {
    let attrs = input.call(Attribute::parse_outer)?;
    let vis = input.parse_id::<Vis>()?;
    let trait_token: Token![trait] = input.parse()?;
    let ident: NodeId<Ident> = input.parse()?;
    let generics = input.parse_id::<Generics>()?;
    Ok((attrs, vis, trait_token, ident, generics))
}

fn parse_rest_of_trait_alias(
    input: ParseStream,
    attrs: Vec<Attribute>,
    vis: NodeId<Vis>,
    trait_token: Token![trait],
    ident: NodeId<Ident>,
    generics: NodeId<Generics>,
) -> Result<ItemTraitAlias> {
    let eq_token: Token![=] = input.parse()?;

    let mut bounds = Punctuated::new();
    loop {
        if input.peek(Token![where]) || input.peek(Token![;]) {
            break;
        }
        bounds.push_value({
            let allow_precise_capture = false;
            let allow_tilde_const = false;
            TypeParamBound::parse_single(input, allow_precise_capture, allow_tilde_const)?
        });
        if input.peek(Token![where]) || input.peek(Token![;]) {
            break;
        }
        bounds.push_punct(input.parse()?);
    }

    let where_clause = input.parse::<Option<WhereClause>>()?;
    let semi_token: Token![;] = input.parse()?;

    Ok(ItemTraitAlias {
        attrs,
        vis,
        trait_token,
        ident,
        generics,
        where_clause,
        eq_token,
        bounds,
        semi_token,
    })
}

impl Parse for TraitItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let begin = input.fork();
        let mut attrs = input.call(Attribute::parse_outer)?;
        let vis: SpannedPat<Vis> = input.parse_spanned_pat::<Vis>()?;
        let defaultness: Option<Token![default]> = input.parse()?;
        let ahead = input.fork();

        let lookahead = ahead.lookahead1();
        let allow_safe = false;
        let mut item = if lookahead.peek(Token![fn]) || peek_signature(&ahead, allow_safe) {
            input.parse().map(TraitItem::Fn)
        } else if lookahead.peek(Token![const]) {
            let const_token: Token![const] = ahead.parse()?;
            let lookahead = ahead.lookahead1();
            if lookahead.peek_pat::<Ident>() || lookahead.peek(Token![_]) {
                input.advance_to(&ahead);
                let ident = input.parse_id::<AnyIdent>()?;
                let generics = input.parse_spanned_pat::<Generics>()?;
                let colon_token: Token![:] = input.parse()?;
                let ty: NodeId<Type> = input.parse()?;
                let default = if let Some(eq_token) = input.parse::<Option<Token![=]>>()? {
                    let expr: NodeId<Expr> = input.parse()?;
                    Some((eq_token, expr))
                } else {
                    None
                };
                let where_clause = input.parse::<Option<WhereClause>>()?;
                let semi_token: Token![;] = input.parse()?;
                if !generics.get_property(IsSome) && where_clause.is_none() {
                    Ok(TraitItem::Const(TraitItemConst {
                        attrs: Vec::new(),
                        const_token,
                        ident,
                        generics: input.add_pat(generics),
                        where_clause,
                        colon_token,
                        ty,
                        default,
                        semi_token,
                    }))
                } else {
                    return Ok(TraitItem::Verbatim(verbatim::between(&begin, input)));
                }
            } else if lookahead.peek(Token![async])
                || lookahead.peek(Token![unsafe])
                || lookahead.peek(Token![extern])
                || lookahead.peek(Token![fn])
            {
                input.parse().map(TraitItem::Fn)
            } else {
                Err(lookahead.error())
            }
        } else if lookahead.peek(Token![type]) {
            parse_trait_item_type(begin.fork(), input)
        } else if vis.get_property(IsInherited)
            && defaultness.is_none()
            && (lookahead.peek_pat::<Ident>()
                || lookahead.peek(Token![self])
                || lookahead.peek(Token![super])
                || lookahead.peek(Token![crate])
                || lookahead.peek(Token![::]))
        {
            input.parse().map(TraitItem::Macro)
        } else {
            Err(lookahead.error())
        }?;

        match (&*vis, defaultness) {
            (Pattern::Real(Vis::Inherited), None) => {}
            _ => return Ok(TraitItem::Verbatim(verbatim::between(&begin, input))),
        }

        let item_attrs = match &mut item {
            TraitItem::Const(item) => &mut item.attrs,
            TraitItem::Fn(item) => &mut item.attrs,
            TraitItem::Type(item) => &mut item.attrs,
            TraitItem::Macro(item) => &mut item.attrs,
            TraitItem::Verbatim(_) => unreachable!(),
        };
        attrs.append(item_attrs);
        *item_attrs = attrs;
        Ok(item)
    }
}

impl Parse for TraitItemConst {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let const_token: Token![const] = input.parse()?;

        let lookahead = input.lookahead1();
        let ident = if lookahead.peek_pat::<Ident>() || lookahead.peek(Token![_]) {
            input.parse_id::<AnyIdent>()?
        } else {
            return Err(lookahead.error());
        };

        let colon_token: Token![:] = input.parse()?;
        let ty: NodeId<Type> = input.parse()?;
        let generics = input.make_at_point(Generics::default());
        let default = if input.peek(Token![=]) {
            let eq_token: Token![=] = input.parse()?;
            let default: NodeId<Expr> = input.parse()?;
            Some((eq_token, default))
        } else {
            None
        };
        let semi_token: Token![;] = input.parse()?;

        Ok(TraitItemConst {
            attrs,
            const_token,
            ident,
            generics: input.add_pat(generics),
            where_clause: None,
            colon_token,
            ty,
            default,
            semi_token,
        })
    }
}

impl Parse for TraitItemFn {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let sig: Signature = input.parse()?;

        let lookahead = input.lookahead1();
        let (brace_token, stmts, semi_token) = if lookahead.peek(token::Brace) {
            let content;
            let brace_token = braced!(content in input);
            attr::parse_inner(&content, &mut attrs)?;
            let stmts = content.parse_list::<Block>()?;
            (Some(brace_token), stmts, None)
        } else if lookahead.peek(Token![;]) {
            let semi_token: Token![;] = input.parse()?;
            (None, NodeList::empty_real(), Some(semi_token))
        } else {
            return Err(lookahead.error());
        };

        Ok(TraitItemFn {
            attrs,
            sig,
            default: brace_token.map(|brace_token| Block { brace_token, stmts }),
            semi_token,
        })
    }
}

impl Parse for TraitItemType {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let type_token: Token![type] = input.parse()?;
        let ident: NodeId<Ident> = input.parse()?;
        let generics = input.parse::<NodeId<Generics>>()?;
        let (colon_token, bounds) = FlexibleItemType::parse_optional_bounds(input)?;
        let default = FlexibleItemType::parse_optional_definition(input)?;
        let where_clause = input.parse::<Option<WhereClause>>()?;
        let semi_token: Token![;] = input.parse()?;
        Ok(TraitItemType {
            attrs,
            type_token,
            ident,
            generics,
            where_clause,
            colon_token,
            bounds,
            default,
            semi_token,
        })
    }
}

fn parse_trait_item_type(begin: ParseBuffer, input: ParseStream) -> Result<TraitItem> {
    let FlexibleItemType {
        vis,
        defaultness: _,
        type_token,
        ident,
        generics,
        where_clause,
        colon_token,
        bounds,
        ty,
        semi_token,
    } = FlexibleItemType::parse(
        input,
        TypeDefaultness::Disallowed,
        WhereClauseLocation::AfterEq,
    )?;

    let vis_explicit = input.ctx().get::<Vis>(vis).get_property_ref(IsSome);
    if vis_explicit {
        Ok(TraitItem::Verbatim(verbatim::between(&begin, input)))
    } else {
        Ok(TraitItem::Type(TraitItemType {
            attrs: Vec::new(),
            type_token,
            ident,
            generics,
            where_clause,
            colon_token,
            bounds,
            default: ty,
            semi_token,
        }))
    }
}

impl Parse for TraitItemMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let mac: Macro = input.parse()?;
        let semi_token: Option<Token![;]> = if mac.delimiter.is_brace() {
            None
        } else {
            Some(input.parse()?)
        };
        Ok(TraitItemMacro {
            attrs,
            mac,
            semi_token,
        })
    }
}

impl Parse for ItemImpl {
    fn parse(input: ParseStream) -> Result<Self> {
        let allow_verbatim_impl = false;
        parse_impl(input, allow_verbatim_impl).map(Option::unwrap)
    }
}

fn parse_impl(input: ParseStream, allow_verbatim_impl: bool) -> Result<Option<ItemImpl>> {
    let mut attrs = input.call(Attribute::parse_outer)?;
    let has_visibility =
        allow_verbatim_impl && input.parse_spanned_pat::<Vis>()?.get_property(IsSome);
    let defaultness: Option<Token![default]> = input.parse()?;
    let unsafety: Unsafety = input.parse()?;
    let impl_token: Token![impl] = input.parse()?;

    let has_generics = input.peek_var::<Generics>()
        || input.peek(Token![<])
            && (input.peek2(Token![>])
                || input.peek2(Token![#])
                || (input.peek2(Ident) || input.peek2(Lifetime))
                    && (input.peek3(Token![:])
                        || input.peek3(Token![,])
                        || input.peek3(Token![>])
                        || input.peek3(Token![=]))
                || input.peek2(Token![const]));
    let generics: NodeId<Generics> = if has_generics {
        input.parse_id::<Generics>()?
    } else {
        input.add_pat(input.make_at_point(Generics::default()))
    };

    let is_const_impl = allow_verbatim_impl
        && (input.peek(Token![const]) || input.peek(Token![?]) && input.peek2(Token![const]));
    if is_const_impl {
        input.parse::<Option<Token![?]>>()?;
        input.parse::<Token![const]>()?;
    }

    let begin = input.fork();
    let polarity = if input.peek(Token![!]) && !input.peek2(token::Brace) {
        Some(input.parse::<Token![!]>()?)
    } else {
        None
    };

    let first_ty_span = input.span();
    let (span, first_ty) = input.parse_spanned_pat::<Type>()?.decompose();
    let self_ty: NodeId<Type>;
    let trait_;

    let is_impl_for = input.peek(Token![for]);
    if is_impl_for {
        {
            let ctx = input.ctx();
            let for_token: Token![for] = input.parse()?;
            let mut first_ty_ref = first_ty.as_ref();
            while let Pattern::Real(Type::Group(ty)) = first_ty_ref {
                first_ty_ref = ctx.get(ty.elem);
            }
            if let Pattern::Real(Type::Path(TypePath { qself: None, .. })) = first_ty_ref {
                if let Pattern::Real(Type::Group(_)) = first_ty {
                    // This is related to none-delimited types.
                    // These may occur in the result from macro expansion,
                    // which we currently do not treat anyways.
                    unimplemented!("Parsing macro output not supported")
                }
                if let Pattern::Real(Type::Path(TypePath { qself: None, path })) = first_ty {
                    trait_ = Some((polarity, path, for_token));
                } else {
                    unreachable!();
                }
            } else if !allow_verbatim_impl {
                return Err(Error::new(first_ty_span, "expected trait path"));
            } else {
                trait_ = None;
            }
        }
        self_ty = input.parse()?;
    } else {
        trait_ = None;
        self_ty = if polarity.is_none() {
            input.add_pat(first_ty.with_span(span))
        } else {
            let marker = begin.marker();
            input.add(input.make_spanned(marker, Type::Verbatim(verbatim::between(&begin, input))))
        };
    }

    let where_clause = input.parse::<Option<WhereClause>>()?;

    let content;
    let brace_token = braced!(content in input);
    attr::parse_inner(&content, &mut attrs)?;

    let items = content.parse_list::<ImplItems>()?;

    if has_visibility || is_const_impl || is_impl_for && trait_.is_none() {
        Ok(None)
    } else {
        Ok(Some(ItemImpl {
            attrs,
            defaultness,
            unsafety,
            impl_token,
            generics,
            where_clause,
            trait_,
            self_ty,
            brace_token,
            items,
        }))
    }
}

impl ParseNode for ImplItem {
    type Target = ImplItem;

    fn parse_node(input: ParseStream) -> Result<Self> {
        let begin = input.fork();
        let mut attrs = input.call(Attribute::parse_outer)?;
        let ahead = input.fork();
        let vis: SpannedPat<Vis> = ahead.parse_spanned_pat::<Vis>()?;

        let mut lookahead = ahead.lookahead1();
        let defaultness = if lookahead.peek(Token![default]) && !ahead.peek2(Token![!]) {
            let defaultness: Token![default] = ahead.parse()?;
            lookahead = ahead.lookahead1();
            Some(defaultness)
        } else {
            None
        };

        let allow_safe = false;
        let mut item = if lookahead.peek(Token![fn]) || peek_signature(&ahead, allow_safe) {
            let allow_omitted_body = true;
            if let Some(item) = parse_impl_item_fn(input, allow_omitted_body)? {
                Ok(ImplItem::Fn(item))
            } else {
                Ok(ImplItem::Verbatim(verbatim::between(&begin, input)))
            }
        } else if lookahead.peek(Token![const]) {
            let vis = input.parse_id::<Vis>()?;
            let defaultness: Option<Token![default]> = input.parse()?;
            let const_token: Token![const] = input.parse()?;
            let lookahead = input.lookahead1();
            let ident = if lookahead.peek_pat::<Ident>() || lookahead.peek(Token![_]) {
                input.parse_id::<AnyIdent>()?
            } else {
                return Err(lookahead.error());
            };
            let generics = input.parse_spanned_pat::<Generics>()?;
            let colon_token: Token![:] = input.parse()?;
            let ty: NodeId<Type> = input.parse()?;
            let value = if let Some(eq_token) = input.parse::<Option<Token![=]>>()? {
                let expr: NodeId<Expr> = input.parse()?;
                Some((eq_token, expr))
            } else {
                None
            };
            let where_clause = input.parse::<Option<WhereClause>>()?;
            let semi_token: Token![;] = input.parse()?;
            return match value {
                Some((eq_token, expr))
                    if !generics.get_property(IsSome) && where_clause.is_none() =>
                {
                    Ok(ImplItem::Const(ImplItemConst {
                        attrs,
                        vis,
                        defaultness,
                        const_token,
                        ident,
                        generics: input.add_pat(generics),
                        where_clause,
                        colon_token,
                        ty,
                        eq_token,
                        expr,
                        semi_token,
                    }))
                }
                _ => Ok(ImplItem::Verbatim(verbatim::between(&begin, input))),
            };
        } else if lookahead.peek(Token![type]) {
            parse_impl_item_type(begin, input)
        } else if vis.get_property(IsInherited)
            && defaultness.is_none()
            && (lookahead.peek_pat::<Ident>()
                || lookahead.peek(Token![self])
                || lookahead.peek(Token![super])
                || lookahead.peek(Token![crate])
                || lookahead.peek(Token![::]))
        {
            input.parse().map(ImplItem::Macro)
        } else {
            Err(lookahead.error())
        }?;

        {
            let item_attrs = match &mut item {
                ImplItem::Const(item) => &mut item.attrs,
                ImplItem::Fn(item) => &mut item.attrs,
                ImplItem::Type(item) => &mut item.attrs,
                ImplItem::Macro(item) => &mut item.attrs,
                ImplItem::Verbatim(_) => return Ok(item),
            };
            attrs.append(item_attrs);
            *item_attrs = attrs;
        }

        Ok(item)
    }
}

impl Parse for ImplItemConst {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let defaultness: Option<Token![default]> = input.parse()?;
        let const_token: Token![const] = input.parse()?;

        let lookahead = input.lookahead1();
        let ident = if lookahead.peek_pat::<Ident>() || lookahead.peek(Token![_]) {
            input.parse_id::<AnyIdent>()?
        } else {
            return Err(lookahead.error());
        };

        let colon_token: Token![:] = input.parse()?;
        let ty: NodeId<Type> = input.parse()?;
        let generics = input.make_at_point(Generics::default());
        let eq_token: Token![=] = input.parse()?;
        let expr: NodeId<Expr> = input.parse()?;
        let semi_token: Token![;] = input.parse()?;

        Ok(ImplItemConst {
            attrs,
            vis,
            defaultness,
            const_token,
            ident,
            generics: input.add_pat(generics),
            where_clause: None,
            colon_token,
            ty,
            eq_token,
            expr,
            semi_token,
        })
    }
}

impl Parse for ImplItemFn {
    fn parse(input: ParseStream) -> Result<Self> {
        let allow_omitted_body = false;
        parse_impl_item_fn(input, allow_omitted_body).map(Option::unwrap)
    }
}

fn parse_impl_item_fn(input: ParseStream, allow_omitted_body: bool) -> Result<Option<ImplItemFn>> {
    let mut attrs = input.call(Attribute::parse_outer)?;
    let vis = input.parse_id::<Vis>()?;
    let defaultness: Option<Token![default]> = input.parse()?;
    let sig: Signature = input.parse()?;

    // Accept functions without a body in an impl block because rustc's
    // *parser* does not reject them (the compilation error is emitted later
    // than parsing) and it can be useful for macro DSLs.
    if allow_omitted_body && input.parse::<Option<Token![;]>>()?.is_some() {
        return Ok(None);
    }

    let content;
    let brace_token = braced!(content in input);
    attrs.extend(content.call(Attribute::parse_inner)?);
    let block = Block {
        brace_token,
        stmts: content.parse_list::<Block>()?,
    };

    Ok(Some(ImplItemFn {
        attrs,
        vis,
        defaultness,
        sig,
        block,
    }))
}

impl Parse for ImplItemType {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse_id::<Vis>()?;
        let defaultness: Option<Token![default]> = input.parse()?;
        let type_token: Token![type] = input.parse()?;
        let ident: NodeId<Ident> = input.parse()?;
        let generics = input.parse::<NodeId<Generics>>()?;
        let eq_token: Token![=] = input.parse()?;
        let ty = input.parse()?;
        let where_clause = input.parse::<Option<WhereClause>>()?;
        let semi_token: Token![;] = input.parse()?;
        Ok(ImplItemType {
            attrs,
            vis,
            defaultness,
            type_token,
            ident,
            generics,
            where_clause,
            eq_token,
            ty,
            semi_token,
        })
    }
}

fn parse_impl_item_type(begin: ParseBuffer, input: ParseStream) -> Result<ImplItem> {
    let FlexibleItemType {
        vis,
        defaultness,
        type_token,
        ident,
        generics,
        where_clause,
        colon_token,
        bounds: _,
        ty,
        semi_token,
    } = FlexibleItemType::parse(
        input,
        TypeDefaultness::Optional,
        WhereClauseLocation::AfterEq,
    )?;

    let (eq_token, ty) = match ty {
        Some(ty) if colon_token.is_none() => ty,
        _ => return Ok(ImplItem::Verbatim(verbatim::between(&begin, input))),
    };

    Ok(ImplItem::Type(ImplItemType {
        attrs: Vec::new(),
        vis,
        defaultness,
        type_token,
        ident,
        generics,
        where_clause,
        eq_token,
        ty,
        semi_token,
    }))
}

impl Parse for ImplItemMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let mac: Macro = input.parse()?;
        let semi_token: Option<Token![;]> = if mac.delimiter.is_brace() {
            None
        } else {
            Some(input.parse()?)
        };
        Ok(ImplItemMacro {
            attrs,
            mac,
            semi_token,
        })
    }
}

impl Parse for StaticMutability {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut_token: Option<Token![mut]> = input.parse()?;
        Ok(mut_token.map_or(StaticMutability::None, StaticMutability::Mut))
    }
}

impl Parse for Unsafety {
    fn parse(input: ParseStream) -> Result<Self> {
        let token: Option<Token![unsafe]> = input.parse()?;
        Ok(if token.is_some() {
            Unsafety::Unsafe
        } else {
            Unsafety::Safe
        })
    }
}

impl Parse for Asyncness {
    fn parse(input: ParseStream) -> Result<Self> {
        let token: Option<Token![async]> = input.parse()?;
        Ok(if token.is_some() {
            Asyncness::Async
        } else {
            Asyncness::Sync
        })
    }
}

impl Parse for Constness {
    fn parse(input: ParseStream) -> Result<Self> {
        let token: Option<Token![const]> = input.parse()?;
        Ok(if token.is_some() {
            Constness::Const
        } else {
            Constness::NotConst
        })
    }
}

pub struct Items;

impl ParseList for Items {
    type Item = Item;

    type ParseItem = Item;

    type Punct = Token![;];

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Self::Item>>> {
        let mut items = vec![];
        while !input.is_empty() {
            items.push(input.parse()?);
        }
        Ok(items)
    }
}

struct ImplItems;

impl ParseList for ImplItems {
    type Item = ImplItem;

    type ParseItem = ImplItem;

    type Punct = Token![;];

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Self::Item>>> {
        let mut items = vec![];
        while !input.is_empty() {
            items.push(input.parse()?);
        }
        Ok(items)
    }
}

impl CmpSyn<Item> for ImplItem {
    fn cmp_syn(&self, ctx: &mut crate::Matcher, pat: &Item) {
        match (self, pat) {
            (ImplItem::Const(t1), Item::Const(t2)) => ctx.cmp_syn(t1, t2),
            (ImplItem::Fn(t1), Item::Fn(t2)) => ctx.cmp_syn(t1, t2),
            (ImplItem::Type(t1), Item::Type(t2)) => ctx.cmp_syn(t1, t2),
            (ImplItem::Macro(t1), Item::Macro(t2)) => ctx.cmp_syn(t1, t2),
            _ => ctx.no_match(),
        }
    }
}

impl CmpSyn<ItemConst> for ImplItemConst {
    fn cmp_syn(&self, ctx: &mut crate::Matcher, pat: &ItemConst) {
        ctx.cmp_syn(&self.attrs, &pat.attrs);
        ctx.cmp_syn_with_rule(&self.vis, &pat.vis, rule::Vis::Const);
        ctx.cmp_syn(&self.const_token, &pat.const_token);
        ctx.cmp_syn(&self.ident, &pat.ident);
        ctx.cmp_syn(&self.generics, &pat.generics);
        ctx.cmp_syn(&self.colon_token, &pat.colon_token);
        ctx.cmp_syn(&self.ty, &pat.ty);
        ctx.cmp_syn(&self.eq_token, &pat.eq_token);
        ctx.cmp_syn(&self.expr, &pat.expr);
        ctx.cmp_syn(&self.semi_token, &pat.semi_token);
        ctx.cmp_syn(&self.defaultness, &None);
    }
}

impl CmpSyn<ItemFn> for ImplItemFn {
    fn cmp_syn(&self, ctx: &mut crate::Matcher, pat: &ItemFn) {
        ctx.cmp_syn(&self.attrs, &pat.attrs);
        ctx.cmp_syn_with_rule(&self.vis, &pat.vis, rule::Vis::Fn);
        ctx.cmp_syn(&self.sig, &pat.sig);
        ctx.cmp_syn(&self.block, &pat.block);
        ctx.cmp_syn(&self.defaultness, &None);
    }
}

impl CmpSyn<ItemType> for ImplItemType {
    fn cmp_syn(&self, ctx: &mut crate::Matcher, pat: &ItemType) {
        ctx.cmp_syn(&self.attrs, &pat.attrs);
        ctx.cmp_syn_with_rule(&self.vis, &pat.vis, rule::Vis::Type);
        ctx.cmp_syn(&self.type_token, &pat.type_token);
        ctx.cmp_syn(&self.ident, &pat.ident);
        ctx.cmp_syn(&self.generics, &pat.generics);
        ctx.cmp_syn(&self.eq_token, &pat.eq_token);
        ctx.cmp_syn(&self.ty, &pat.ty);
        ctx.cmp_syn(&self.semi_token, &pat.semi_token);
        ctx.cmp_syn(&self.defaultness, &None);
    }
}

impl CmpSyn<ItemMacro> for ImplItemMacro {
    fn cmp_syn(&self, ctx: &mut crate::Matcher, pat: &ItemMacro) {
        ctx.cmp_syn(&None, &pat.ident);
        ctx.cmp_syn(&self.mac, &pat.mac);
        ctx.cmp_syn(&self.semi_token, &pat.semi_token);
    }
}
