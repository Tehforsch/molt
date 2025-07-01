use derive_macro::CmpSyn;
use molt_lib::{NodeId, NodeList};

use crate::attr::Attribute;
#[cfg(feature = "full")]
use crate::generics::BoundLifetimes;
use crate::ident::{AnyIdent, Ident};
#[cfg(any(feature = "parsing", feature = "full"))]
use crate::lifetime::Lifetime;
use crate::lit::Lit;
use crate::mac::Macro;
use crate::op::{BinOp, UnOp};
#[cfg(feature = "parsing")]
use crate::parse::ParseStream;
#[cfg(feature = "full")]
use crate::pat::Pat;
use crate::path::{AngleBracketedGenericArguments, Path, QSelf};
use crate::punctuated::Punctuated;
#[cfg(feature = "full")]
use crate::stmt::Block;
use crate::token;
#[cfg(feature = "full")]
use crate::ty::ReturnType;
use crate::ty::Type;
use proc_macro2::{Span, TokenStream};
#[cfg(feature = "printing")]
use quote::IdentFragment;
#[cfg(feature = "printing")]
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};
#[cfg(all(feature = "parsing", feature = "full"))]
use std::mem;

/// An alternative to the primary `Expr::parse` parser (from the [`Parse`]
/// trait) for ambiguous syntactic positions in which a trailing brace
/// should not be taken as part of the expression.
///
/// [`Parse`]: crate::parse::Parse
///
/// Rust grammar has an ambiguity where braces sometimes turn a path
/// expression into a struct initialization and sometimes do not. In the
/// following code, the expression `S {}` is one expression. Presumably
/// there is an empty struct `struct S {}` defined somewhere which it is
/// instantiating.
///
/// ```
/// # struct S;
/// # impl std::ops::Deref for S {
/// #     type Target = bool;
/// #     fn deref(&self) -> &Self::Target {
/// #         &true
/// #     }
/// # }
/// let _ = *S {};
///
/// // parsed by rustc as: `*(S {})`
/// ```
///
/// We would want to parse the above using `Expr::parse` after the `=`
/// token.
///
/// But in the following, `S {}` is *not* a struct init expression.
///
/// ```
/// # const S: &bool = &true;
/// if *S {} {}
///
/// // parsed by rustc as:
/// //
/// //    if (*S) {
/// //        /* empty block */
/// //    }
/// //    {
/// //        /* another empty block */
/// //    }
/// ```
///
/// For that reason we would want to parse if-conditions using
/// `Expr::parse_without_eager_brace` after the `if` token. Same for similar
/// syntactic positions such as the condition expr after a `while` token or
/// the expr at the top of a `match`.
///
/// The Rust grammar's choices around which way this ambiguity is resolved
/// at various syntactic positions is fairly arbitrary. Really either parse
/// behavior could work in most positions, and language designers just
/// decide each case based on which is more likely to be what the programmer
/// had in mind most of the time.
///
/// ```
/// # struct S;
/// # fn doc() -> S {
/// if return S {} {}
/// # unreachable!()
/// # }
///
/// // parsed by rustc as:
/// //
/// //    if (return (S {})) {
/// //    }
/// //
/// // but could equally well have been this other arbitrary choice:
/// //
/// //    if (return S) {
/// //    }
/// //    {}
/// ```
///
/// Note the grammar ambiguity on trailing braces is distinct from
/// precedence and is not captured by assigning a precedence level to the
/// braced struct init expr in relation to other operators. This can be
/// illustrated by `return 0..S {}` vs `match 0..S {}`. The former parses as
/// `return (0..(S {}))` implying tighter precedence for struct init than
/// `..`, while the latter parses as `match (0..S) {}` implying tighter
/// precedence for `..` than struct init, a contradiction.
pub struct ExprNoEagerBrace;

/// An alternative to the primary `Expr::parse` parser (from the [`Parse`]
/// trait) for syntactic positions in which expression boundaries are placed
/// more eagerly than done by the typical expression grammar. This includes
/// expressions at the head of a statement or in the right-hand side of a
/// `match` arm.
///
/// [`Parse`]: crate::parse::Parse
///
/// Compare the following cases:
///
/// 1.
///   ```
///   # let result = ();
///   # let guard = false;
///   # let cond = true;
///   # let f = true;
///   # let g = f;
///   #
///   let _ = match result {
///       () if guard => if cond { f } else { g }
///       () => false,
///   };
///   ```
///
/// 2.
///   ```
///   # let cond = true;
///   # let f = ();
///   # let g = f;
///   #
///   let _ = || {
///       if cond { f } else { g }
///       ()
///   };
///   ```
///
/// 3.
///   ```
///   # let cond = true;
///   # let f = || ();
///   # let g = f;
///   #
///   let _ = [if cond { f } else { g } ()];
///   ```
///
/// The same sequence of tokens `if cond { f } else { g } ()` appears in
/// expression position 3 times. The first two syntactic positions use eager
/// placement of expression boundaries, and parse as `Expr::If`, with the
/// adjacent `()` becoming `Pat::Tuple` or `Expr::Tuple`. In contrast, the
/// third case uses standard expression boundaries and parses as
/// `Expr::Call`.
///
/// As with [`parse_without_eager_brace`], this ambiguity in the Rust
/// grammar is independent of precedence.
///
/// [`parse_without_eager_brace`]: Self::parse_without_eager_brace
pub struct ExprEarlierBoundaryRule;

#[derive(Debug, CmpSyn)]
/// A Rust expression.
///
/// *This type is available only if Syn is built with the `"derive"` or `"full"`
/// feature, but most of the variants are not available unless "full" is enabled.*
///
/// # Syntax tree enums
///
/// This type is a syntax tree enum. In Syn this and other syntax tree enums
/// are designed to be traversed using the following rebinding idiom.
///
/// ```
/// # use syn::Expr;
/// #
/// # fn example(expr: Expr) {
/// # const IGNORE: &str = stringify! {
/// let expr: Expr = /* ... */;
/// # };
/// match expr {
///     Expr::MethodCall(expr) => {
///         /* ... */
///     }
///     Expr::Cast(expr) => {
///         /* ... */
///     }
///     Expr::If(expr) => {
///         /* ... */
///     }
///
///     /* ... */
///     # _ => {}
/// # }
/// # }
/// ```
///
/// We begin with a variable `expr` of type `Expr` that has no fields
/// (because it is an enum), and by matching on it and rebinding a variable
/// with the same name `expr` we effectively imbue our variable with all of
/// the data fields provided by the variant that it turned out to be. So for
/// example above if we ended up in the `MethodCall` case then we get to use
/// `expr.receiver`, `expr.args` etc; if we ended up in the `If` case we get
/// to use `expr.cond`, `expr.then_branch`, `expr.else_branch`.
///
/// This approach avoids repeating the variant names twice on every line.
///
/// ```
/// # use syn::{Expr, ExprMethodCall};
/// #
/// # fn example(expr: Expr) {
/// // Repetitive; recommend not doing this.
/// match expr {
///     Expr::MethodCall(ExprMethodCall { method, args, .. }) => {
/// # }
/// # _ => {}
/// # }
/// # }
/// ```
///
/// In general, the name to which a syntax tree enum variant is bound should
/// be a suitable name for the complete syntax tree enum type.
///
/// ```
/// # use syn::{Expr, ExprField};
/// #
/// # fn example(discriminant: ExprField) {
/// // Binding is called `base` which is the name I would use if I were
/// // assigning `*discriminant.base` without an `if let`.
/// if let Expr::Tuple(base) = *discriminant.base {
/// # }
/// # }
/// ```
///
/// A sign that you may not be choosing the right variable names is if you
/// see names getting repeated in your code, like accessing
/// `receiver.receiver` or `pat.pat` or `cond.cond`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
#[non_exhaustive]
pub enum Expr {
    /// A slice literal expression: `[a, b, c, d]`.
    Array(ExprArray),

    /// An assignment expression: `a = compute()`.
    Assign(ExprAssign),

    /// An async block: `async { ... }`.
    Async(ExprAsync),

    /// An await expression: `fut.await`.
    Await(ExprAwait),

    /// A binary operation: `a + b`, `a += b`.
    Binary(ExprBinary),

    /// A blocked scope: `{ ... }`.
    Block(ExprBlock),

    /// A `break`, with an optional label to break and an optional
    /// expression.
    Break(ExprBreak),

    /// A function call expression: `invoke(a, b)`.
    Call(ExprCall),

    /// A cast expression: `foo as f64`.
    Cast(ExprCast),

    /// A closure expression: `|a, b| a + b`.
    Closure(ExprClosure),

    /// A const block: `const { ... }`.
    Const(ExprConst),

    /// A `continue`, with an optional label.
    Continue(ExprContinue),

    /// Access of a named struct field (`obj.k`) or unnamed tuple struct
    /// field (`obj.0`).
    Field(ExprField),

    /// A for loop: `for pat in expr { ... }`.
    ForLoop(ExprForLoop),

    /// An expression contained within invisible delimiters.
    ///
    /// This variant is important for faithfully representing the precedence
    /// of expressions and is related to `None`-delimited spans in a
    /// `TokenStream`.
    Group(ExprGroup),

    /// An `if` expression with an optional `else` block: `if expr { ... }
    /// else { ... }`.
    ///
    /// The `else` branch expression may only be an `If` or `Block`
    /// expression, not any of the other types of expression.
    If(ExprIf),

    /// A square bracketed indexing expression: `vector[2]`.
    Index(ExprIndex),

    /// The inferred value of a const generic argument, denoted `_`.
    Infer(ExprInfer),

    /// A `let` guard: `let Some(x) = opt`.
    Let(ExprLet),

    /// A literal in place of an expression: `1`, `"foo"`.
    Lit(ExprLit),

    /// Conditionless loop: `loop { ... }`.
    Loop(ExprLoop),

    /// A macro invocation expression: `format!("{}", q)`.
    Macro(ExprMacro),

    /// A `match` expression: `match n { Some(n) => {}, None => {} }`.
    Match(ExprMatch),

    /// A method call expression: `x.foo::<T>(a, b)`.
    MethodCall(ExprMethodCall),

    /// A parenthesized expression: `(a + b)`.
    Paren(ExprParen),

    /// A path like `std::mem::replace` possibly containing generic
    /// parameters and a qualified self-type.
    ///
    /// A plain identifier like `x` is a path of length 1.
    Path(ExprPath),

    /// A range expression: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
    Range(ExprRange),

    /// Address-of operation: `&raw const place` or `&raw mut place`.
    RawAddr(ExprRawAddr),

    /// A referencing operation: `&a` or `&mut a`.
    Reference(ExprReference),

    /// An array literal constructed from one repeated element: `[0u8; N]`.
    Repeat(ExprRepeat),

    /// A `return`, with an optional value to be returned.
    Return(ExprReturn),

    /// A struct literal expression: `Point { x: 1, y: 1 }`.
    ///
    /// The `rest` provides the value of the remaining fields as in `S { a:
    /// 1, b: 1, ..rest }`.
    Struct(ExprStruct),

    /// A try-expression: `expr?`.
    Try(ExprTry),

    /// A try block: `try { ... }`.
    TryBlock(ExprTryBlock),

    /// A tuple expression: `(a, b, c, d)`.
    Tuple(ExprTuple),

    /// A unary operation: `!x`, `*x`.
    Unary(ExprUnary),

    /// An unsafe block: `unsafe { ... }`.
    Unsafe(ExprUnsafe),

    /// Tokens in expression position not interpreted by Syn.
    Verbatim(TokenStream),

    /// A while loop: `while expr { ... }`.
    While(ExprWhile),

    /// A yield expression: `yield expr`.
    Yield(ExprYield),
    // For testing exhaustiveness in downstream code, use the following idiom:
    //
    //     match expr {
    //         #![cfg_attr(test, deny(non_exhaustive_omitted_patterns))]
    //
    //         Expr::Array(expr) => {...}
    //         Expr::Assign(expr) => {...}
    //         ...
    //         Expr::Yield(expr) => {...}
    //
    //         _ => { /* some sane fallback */ }
    //     }
    //
    // This way we fail your tests but don't break your library when adding
    // a variant. You will be notified by a test failure when a variant is
    // added, so that you can add code to handle it, but your library will
    // continue to compile and work for downstream users in the interim.
}

#[derive(Debug, CmpSyn)]
/// A slice literal expression: `[a, b, c, d]`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprArray {
    pub attrs: Vec<Attribute>,
    pub bracket_token: token::Bracket,
    pub elems: NodeList<Expr, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// An assignment expression: `a = compute()`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprAssign {
    pub attrs: Vec<Attribute>,
    pub left: NodeId<Expr>,
    pub eq_token: Token![=],
    pub right: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// An async block: `async { ... }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprAsync {
    pub attrs: Vec<Attribute>,
    pub async_token: Token![async],
    pub capture: Option<Token![move]>,
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// An await expression: `fut.await`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprAwait {
    pub attrs: Vec<Attribute>,
    pub base: NodeId<Expr>,
    pub dot_token: Token![.],
    pub await_token: Token![await],
}

#[derive(Debug, CmpSyn)]
/// A binary operation: `a + b`, `a += b`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprBinary {
    pub attrs: Vec<Attribute>,
    pub left: NodeId<Expr>,
    pub op: BinOp,
    pub right: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A blocked scope: `{ ... }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprBlock {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// A `break`, with an optional label to break and an optional
/// expression.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprBreak {
    pub attrs: Vec<Attribute>,
    pub break_token: Token![break],
    pub label: Option<Lifetime>,
    pub expr: Option<NodeId<Expr>>,
}

#[derive(Debug, CmpSyn)]
/// A function call expression: `invoke(a, b)`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprCall {
    pub attrs: Vec<Attribute>,
    pub func: NodeId<Expr>,
    pub paren_token: token::Paren,
    pub args: NodeList<Expr, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A cast expression: `foo as f64`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprCast {
    pub attrs: Vec<Attribute>,
    pub expr: NodeId<Expr>,
    pub as_token: Token![as],
    pub ty: NodeId<Type>,
}

#[derive(Debug, CmpSyn)]
/// A closure expression: `|a, b| a + b`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprClosure {
    pub attrs: Vec<Attribute>,
    pub lifetimes: Option<BoundLifetimes>,
    pub constness: Option<Token![const]>,
    pub movability: Option<Token![static]>,
    pub asyncness: Option<Token![async]>,
    pub capture: Option<Token![move]>,
    pub or1_token: Token![|],
    pub inputs: Punctuated<Pat, Token![,]>,
    pub or2_token: Token![|],
    pub output: ReturnType,
    pub body: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A const block: `const { ... }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprConst {
    pub attrs: Vec<Attribute>,
    pub const_token: Token![const],
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// A `continue`, with an optional label.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprContinue {
    pub attrs: Vec<Attribute>,
    pub continue_token: Token![continue],
    pub label: Option<Lifetime>,
}

#[derive(Debug, CmpSyn)]
/// Access of a named struct field (`obj.k`) or unnamed tuple struct
/// field (`obj.0`).
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprField {
    pub attrs: Vec<Attribute>,
    pub base: NodeId<Expr>,
    pub dot_token: Token![.],
    pub member: Member,
}

#[derive(Debug, CmpSyn)]
/// A for loop: `for pat in expr { ... }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprForLoop {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub for_token: Token![for],
    pub pat: Box<Pat>,
    pub in_token: Token![in],
    pub expr: NodeId<Expr>,
    pub body: Block,
}

#[derive(Debug, CmpSyn)]
/// An expression contained within invisible delimiters.
///
/// This variant is important for faithfully representing the precedence
/// of expressions and is related to `None`-delimited spans in a
/// `TokenStream`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprGroup {
    pub attrs: Vec<Attribute>,
    pub group_token: token::Group,
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// An `if` expression with an optional `else` block: `if expr { ... }
/// else { ... }`.
///
/// The `else` branch expression may only be an `If` or `Block`
/// expression, not any of the other types of expression.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprIf {
    pub attrs: Vec<Attribute>,
    pub if_token: Token![if],
    pub cond: NodeId<Expr>,
    pub then_branch: Block,
    pub else_branch: Option<(Token![else], NodeId<Expr>)>,
}

#[derive(Debug, CmpSyn)]
/// A square bracketed indexing expression: `vector[2]`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprIndex {
    pub attrs: Vec<Attribute>,
    pub expr: NodeId<Expr>,
    pub bracket_token: token::Bracket,
    pub index: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// The inferred value of a const generic argument, denoted `_`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprInfer {
    pub attrs: Vec<Attribute>,
    pub underscore_token: Token![_],
}

#[derive(Debug, CmpSyn)]
/// A `let` guard: `let Some(x) = opt`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprLet {
    pub attrs: Vec<Attribute>,
    pub let_token: Token![let],
    pub pat: Box<Pat>,
    pub eq_token: Token![=],
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A literal in place of an expression: `1`, `"foo"`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprLit {
    pub attrs: Vec<Attribute>,
    pub lit: NodeId<Lit>,
}

#[derive(Debug, CmpSyn)]
/// Conditionless loop: `loop { ... }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprLoop {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub loop_token: Token![loop],
    pub body: Block,
}

#[derive(Debug, CmpSyn)]
/// A macro invocation expression: `format!("{}", q)`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprMacro {
    pub attrs: Vec<Attribute>,
    pub mac: Macro,
}

#[derive(Debug, CmpSyn)]
/// A `match` expression: `match n { Some(n) => {}, None => {} }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprMatch {
    pub attrs: Vec<Attribute>,
    pub match_token: Token![match],
    pub expr: NodeId<Expr>,
    pub brace_token: token::Brace,
    pub arms: NodeList<Arm, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A method call expression: `x.foo::<T>(a, b)`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprMethodCall {
    pub attrs: Vec<Attribute>,
    pub receiver: NodeId<Expr>,
    pub dot_token: Token![.],
    pub method: NodeId<Ident>,
    pub turbofish: Option<AngleBracketedGenericArguments>,
    pub paren_token: token::Paren,
    pub args: NodeList<Expr, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A parenthesized expression: `(a + b)`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprParen {
    pub attrs: Vec<Attribute>,
    pub paren_token: token::Paren,
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A path like `std::mem::replace` possibly containing generic
/// parameters and a qualified self-type.
///
/// A plain identifier like `x` is a path of length 1.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprPath {
    pub attrs: Vec<Attribute>,
    pub qself: Option<QSelf>,
    pub path: Path,
}

#[derive(Debug, CmpSyn)]
/// A range expression: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprRange {
    pub attrs: Vec<Attribute>,
    pub start: Option<NodeId<Expr>>,
    pub limits: RangeLimits,
    pub end: Option<NodeId<Expr>>,
}

#[derive(Debug, CmpSyn)]
/// Address-of operation: `&raw const place` or `&raw mut place`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprRawAddr {
    pub attrs: Vec<Attribute>,
    pub and_token: Token![&],
    pub raw: Token![raw],
    pub mutability: PointerMutability,
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A referencing operation: `&a` or `&mut a`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprReference {
    pub attrs: Vec<Attribute>,
    pub and_token: Token![&],
    pub mutability: Option<Token![mut]>,
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// An array literal constructed from one repeated element: `[0u8; N]`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprRepeat {
    pub attrs: Vec<Attribute>,
    pub bracket_token: token::Bracket,
    pub expr: NodeId<Expr>,
    pub semi_token: Token![;],
    pub len: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A `return`, with an optional value to be returned.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprReturn {
    pub attrs: Vec<Attribute>,
    pub return_token: Token![return],
    pub expr: Option<NodeId<Expr>>,
}

#[derive(Debug, CmpSyn)]
/// A struct literal expression: `Point { x: 1, y: 1 }`.
///
/// The `rest` provides the value of the remaining fields as in `S { a:
/// 1, b: 1, ..rest }`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprStruct {
    pub attrs: Vec<Attribute>,
    pub qself: Option<QSelf>,
    pub path: Path,
    pub brace_token: token::Brace,
    pub fields: Punctuated<FieldValue, Token![,]>,
    pub dot2_token: Option<Token![..]>,
    pub rest: Option<NodeId<Expr>>,
}

#[derive(Debug, CmpSyn)]
/// A try-expression: `expr?`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprTry {
    pub attrs: Vec<Attribute>,
    pub expr: NodeId<Expr>,
    pub question_token: Token![?],
}

#[derive(Debug, CmpSyn)]
/// A try block: `try { ... }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprTryBlock {
    pub attrs: Vec<Attribute>,
    pub try_token: Token![try],
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// A tuple expression: `(a, b, c, d)`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprTuple {
    pub attrs: Vec<Attribute>,
    pub paren_token: token::Paren,
    pub elems: NodeList<Expr, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A unary operation: `!x`, `*x`.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct ExprUnary {
    pub attrs: Vec<Attribute>,
    pub op: UnOp,
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// An unsafe block: `unsafe { ... }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprUnsafe {
    pub attrs: Vec<Attribute>,
    pub unsafe_token: Token![unsafe],
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// A while loop: `while expr { ... }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprWhile {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub while_token: Token![while],
    pub cond: NodeId<Expr>,
    pub body: Block,
}

#[derive(Debug, CmpSyn)]
/// A yield expression: `yield expr`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct ExprYield {
    pub attrs: Vec<Attribute>,
    pub yield_token: Token![yield],
    pub expr: Option<NodeId<Expr>>,
}

impl Expr {
    /// An unspecified invalid expression.
    ///
    /// ```
    /// use quote::ToTokens;
    /// use std::mem;
    /// use syn::{parse_quote, Expr};
    ///
    /// fn unparenthesize(e: &mut Expr) {
    ///     while let Expr::Paren(paren) = e {
    ///         *e = mem::replace(&mut *paren.expr, Expr::PLACEHOLDER);
    ///     }
    /// }
    ///
    /// fn main() {
    ///     let mut e: Expr = parse_quote! { ((1 + 1)) };
    ///     unparenthesize(&mut e);
    ///     assert_eq!("1 + 1", e.to_token_stream().to_string());
    /// }
    /// ```
    pub const PLACEHOLDER: Self = Expr::Path(ExprPath {
        attrs: Vec::new(),
        qself: None,
        path: Path {
            leading_colon: None,
            segments: Punctuated::new(),
        },
    });

    /// Returns whether the next token in the parse stream is one that might
    /// possibly form the beginning of an expr.
    ///
    /// This classification is a load-bearing part of the grammar of some Rust
    /// expressions, notably `return` and `break`. For example `return < …` will
    /// never parse `<` as a binary operator regardless of what comes after,
    /// because `<` is a legal starting token for an expression and so it's
    /// required to be continued as a return value, such as `return <Struct as
    /// Trait>::CONST`. Meanwhile `return > …` treats the `>` as a binary
    /// operator because it cannot be a starting token for any Rust expression.
    pub fn peek(input: ParseStream) -> bool {
        input.peek_var::<Expr> ()
            || input.peek_pat::<AnyIdent>() && !input.peek(Token![as]) // value name or keyword
            || input.peek(token::Paren) // tuple
            || input.peek(token::Bracket) // array
            || input.peek(token::Brace) // block
            || input.peek(Lit) // literal
            || input.peek(Token![!]) && !input.peek(Token![!=]) // operator not
            || input.peek(Token![-]) && !input.peek(Token![-=]) && !input.peek(Token![->]) // unary minus
            || input.peek(Token![*]) && !input.peek(Token![*=]) // dereference
            || input.peek(Token![|]) && !input.peek(Token![|=]) // closure
            || input.peek(Token![&]) && !input.peek(Token![&=]) // reference
            || input.peek(Token![..]) // range
            || input.peek(Token![<]) && !input.peek(Token![<=]) && !input.peek(Token![<<=]) // associated path
            || input.peek(Token![::]) // absolute path
            || input.peek(Lifetime) // labeled loop
            || input.peek(Token![#]) // expression attributes
    }

    #[cfg(all(feature = "parsing", feature = "full"))]
    pub(crate) fn replace_attrs(&mut self, new: Vec<Attribute>) -> Vec<Attribute> {
        match self {
            Expr::Array(ExprArray { attrs, .. })
            | Expr::Assign(ExprAssign { attrs, .. })
            | Expr::Async(ExprAsync { attrs, .. })
            | Expr::Await(ExprAwait { attrs, .. })
            | Expr::Binary(ExprBinary { attrs, .. })
            | Expr::Block(ExprBlock { attrs, .. })
            | Expr::Break(ExprBreak { attrs, .. })
            | Expr::Call(ExprCall { attrs, .. })
            | Expr::Cast(ExprCast { attrs, .. })
            | Expr::Closure(ExprClosure { attrs, .. })
            | Expr::Const(ExprConst { attrs, .. })
            | Expr::Continue(ExprContinue { attrs, .. })
            | Expr::Field(ExprField { attrs, .. })
            | Expr::ForLoop(ExprForLoop { attrs, .. })
            | Expr::Group(ExprGroup { attrs, .. })
            | Expr::If(ExprIf { attrs, .. })
            | Expr::Index(ExprIndex { attrs, .. })
            | Expr::Infer(ExprInfer { attrs, .. })
            | Expr::Let(ExprLet { attrs, .. })
            | Expr::Lit(ExprLit { attrs, .. })
            | Expr::Loop(ExprLoop { attrs, .. })
            | Expr::Macro(ExprMacro { attrs, .. })
            | Expr::Match(ExprMatch { attrs, .. })
            | Expr::MethodCall(ExprMethodCall { attrs, .. })
            | Expr::Paren(ExprParen { attrs, .. })
            | Expr::Path(ExprPath { attrs, .. })
            | Expr::Range(ExprRange { attrs, .. })
            | Expr::RawAddr(ExprRawAddr { attrs, .. })
            | Expr::Reference(ExprReference { attrs, .. })
            | Expr::Repeat(ExprRepeat { attrs, .. })
            | Expr::Return(ExprReturn { attrs, .. })
            | Expr::Struct(ExprStruct { attrs, .. })
            | Expr::Try(ExprTry { attrs, .. })
            | Expr::TryBlock(ExprTryBlock { attrs, .. })
            | Expr::Tuple(ExprTuple { attrs, .. })
            | Expr::Unary(ExprUnary { attrs, .. })
            | Expr::Unsafe(ExprUnsafe { attrs, .. })
            | Expr::While(ExprWhile { attrs, .. })
            | Expr::Yield(ExprYield { attrs, .. }) => mem::replace(attrs, new),
            Expr::Verbatim(_) => Vec::new(),
        }
    }
}

#[derive(Debug, CmpSyn)]
/// A struct or tuple struct field accessed in a struct literal or field
/// expression.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub enum Member {
    /// A named field like `self.x`.
    Named(NodeId<Ident>),
    /// An unnamed field like `self.0`.
    Unnamed(Index),
}

impl From<NodeId<Ident>> for Member {
    fn from(ident: NodeId<Ident>) -> Member {
        Member::Named(ident)
    }
}

impl From<Index> for Member {
    fn from(index: Index) -> Member {
        Member::Unnamed(index)
    }
}

impl From<usize> for Member {
    fn from(index: usize) -> Member {
        Member::Unnamed(Index::from(index))
    }
}

impl Eq for Member {}

impl PartialEq for Member {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Member::Named(this), Member::Named(other)) => this == other,
            (Member::Unnamed(this), Member::Unnamed(other)) => this == other,
            _ => false,
        }
    }
}

impl Hash for Member {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Member::Named(m) => m.hash(state),
            Member::Unnamed(m) => m.hash(state),
        }
    }
}

#[cfg(feature = "printing")]
impl IdentFragment for Member {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Member::Named(m) => Display::fmt(m, formatter),
            Member::Unnamed(m) => Display::fmt(&m.index, formatter),
        }
    }

    fn span(&self) -> Option<Span> {
        match self {
            Member::Named(m) => Some(m.span()),
            Member::Unnamed(m) => Some(m.span),
        }
    }
}

#[cfg(any(feature = "parsing", feature = "printing"))]
impl Member {
    pub(crate) fn is_named(&self) -> bool {
        match self {
            Member::Named(_) => true,
            Member::Unnamed(_) => false,
        }
    }
}

#[derive(Debug, CmpSyn)]
/// The index of an unnamed tuple struct field.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct Index {
    pub index: u32,
    pub span: Span,
}

impl From<usize> for Index {
    fn from(index: usize) -> Index {
        assert!(index < u32::MAX as usize);
        Index {
            index: index as u32,
            span: Span::call_site(),
        }
    }
}

impl Eq for Index {}

impl PartialEq for Index {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Hash for Index {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

#[cfg(feature = "printing")]
impl IdentFragment for Index {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.index, formatter)
    }

    fn span(&self) -> Option<Span> {
        Some(self.span)
    }
}

#[derive(Debug, CmpSyn)]
/// A field-value pair in a struct literal.
#[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
pub struct FieldValue {
    pub attrs: Vec<Attribute>,
    pub member: Member,

    /// The colon in `Struct { x: x }`. If written in shorthand like
    /// `Struct { x }`, there is no colon.
    pub colon_token: Option<Token![:]>,

    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A lifetime labeling a `for`, `while`, or `loop`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct Label {
    pub name: Lifetime,
    pub colon_token: Token![:],
}

#[derive(Debug, CmpSyn)]
/// One arm of a `match` expression: `0..=10 => { return true; }`.
///
/// As in:
///
/// ```
/// # fn f() -> bool {
/// #     let n = 0;
/// match n {
///     0..=10 => {
///         return true;
///     }
///     // ...
///     # _ => {}
/// }
/// #   false
/// # }
/// ```
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct Arm {
    pub attrs: Vec<Attribute>,
    pub pat: Pat,
    pub guard: Option<(Token![if], NodeId<Expr>)>,
    pub fat_arrow_token: Token![=>],
    pub body: NodeId<Expr>,
    pub comma: Option<Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// Limit types of a range, inclusive or exclusive.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub enum RangeLimits {
    /// Inclusive at the beginning, exclusive at the end.
    HalfOpen(Token![..]),
    /// Inclusive at the beginning and end.
    Closed(Token![..=]),
}

#[derive(Debug, CmpSyn)]
/// Mutability of a raw pointer (`*const T`, `*mut T`), in which non-mutable
/// isn't the implicit default.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub enum PointerMutability {
    Const(Token![const]),
    Mut(Token![mut]),
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    #[cfg(feature = "full")]
    use crate::attr;
    use crate::attr::Attribute;
    #[cfg(feature = "full")]
    use crate::classify;
    use crate::error::{Error, Result};
    #[cfg(feature = "full")]
    use crate::expr::{
        Arm, ExprArray, ExprAssign, ExprAsync, ExprAwait, ExprBlock, ExprBreak, ExprClosure,
        ExprConst, ExprContinue, ExprForLoop, ExprIf, ExprInfer, ExprLet, ExprLoop, ExprMatch,
        ExprRange, ExprRawAddr, ExprRepeat, ExprReturn, ExprTry, ExprTryBlock, ExprUnsafe,
        ExprWhile, ExprYield, Label, PointerMutability, RangeLimits,
    };
    use crate::expr::{
        Expr, ExprBinary, ExprCall, ExprCast, ExprField, ExprGroup, ExprIndex, ExprLit, ExprMacro,
        ExprMethodCall, ExprParen, ExprPath, ExprReference, ExprStruct, ExprTuple, ExprUnary,
        FieldValue, Index, Member,
    };
    #[cfg(feature = "full")]
    use crate::generics::BoundLifetimes;
    use crate::ident::Ident;
    #[cfg(feature = "full")]
    use crate::lifetime::Lifetime;
    use crate::lit::{Lit, LitFloat, LitInt};
    use crate::mac::{self, Macro};
    use crate::op::BinOp;
    use crate::parse::discouraged::Speculative as _;
    #[cfg(feature = "full")]
    use crate::parse::ParseBuffer;
    use crate::parse::{ListOrItem, Parse, ParseList, ParseListOrItem, ParsePat, ParseStream};
    #[cfg(feature = "full")]
    use crate::pat::{Pat, PatType};
    use crate::path::{self, AngleBracketedGenericArguments, Path, QSelf};
    use crate::precedence::Precedence;
    use crate::punctuated::Punctuated;
    #[cfg(feature = "full")]
    use crate::stmt::Block;
    use crate::ty::parsing::NoPlus;
    #[cfg(feature = "full")]
    use crate::ty::ReturnType;
    use crate::{token, Type};
    use crate::{verbatim, Stmt};
    use molt_lib::{Id, NodeId, NodeList, Pattern, Spanned, SpannedPat, WithSpan};
    use std::mem;

    use super::{ExprEarlierBoundaryRule, ExprNoEagerBrace};

    // When we're parsing expressions which occur before blocks, like in an if
    // statement's condition, we cannot parse a struct literal.
    //
    // Struct literals are ambiguous in certain positions
    // https://github.com/rust-lang/rfcs/pull/92
    pub(super) struct AllowStruct(pub bool);

    impl ParsePat for Expr {
        type Target = Expr;

        fn parse_pat(input: ParseStream) -> Result<SpannedPat<Self::Target>> {
            ambiguous_expr(input, AllowStruct(true))
        }
    }

    impl ParseList for Expr {
        type Punct = Token![,];
    }

    impl ParsePat for ExprNoEagerBrace {
        type Target = Expr;

        fn parse_pat(input: ParseStream) -> Result<SpannedPat<Self::Target>> {
            ambiguous_expr(input, AllowStruct(false))
        }
    }

    impl ParsePat for ExprEarlierBoundaryRule {
        type Target = Expr;

        fn parse_pat(input: ParseStream) -> Result<SpannedPat<Self::Target>> {
            parse_with_earlier_boundary_rule(input)
        }
    }

    pub(super) fn parse_with_earlier_boundary_rule(input: ParseStream) -> Result<SpannedPat<Expr>> {
        let mut attrs = input.call(expr_attrs)?;
        let mut expr: SpannedPat<Expr> = if input.peek(token::Group) {
            let allow_struct = AllowStruct(true);
            let atom = input.call_spanned(|input| expr_group(input, allow_struct))?;
            if continue_parsing_early(input, &atom) {
                trailer_helper(input, atom)?
            } else {
                atom
            }
        } else if input.peek(Token![if]) {
            input.parse_span_with(Expr::If)?
        } else if input.peek(Token![while]) {
            input.parse_span_with(Expr::While)?
        } else if input.peek(Token![for])
            && !(input.peek2(Token![<]) && (input.peek3(Lifetime) || input.peek3(Token![>])))
        {
            input.parse_span_with(Expr::ForLoop)?
        } else if input.peek(Token![loop]) {
            input.parse_span_with(Expr::Loop)?
        } else if input.peek(Token![match]) {
            input.parse_span_with(Expr::Match)?
        } else if input.peek(Token![try]) && input.peek2(token::Brace) {
            input.parse_span_with(Expr::TryBlock)?
        } else if input.peek(Token![unsafe]) {
            input.parse_span_with(Expr::Unsafe)?
        } else if input.peek(Token![const]) && input.peek2(token::Brace) {
            input.parse_span_with(Expr::Const)?
        } else if input.peek(token::Brace) {
            input.parse_span_with(Expr::Block)?
        } else if input.peek(Lifetime) {
            input.call_spanned(atom_labeled)?
        } else {
            let allow_struct = AllowStruct(true);
            unary_expr(input, allow_struct)?
        };

        if continue_parsing_early(input, &expr) {
            if let Pattern::Real(ref mut expr) = &mut *expr {
                attrs.extend(expr.replace_attrs(Vec::new()));
                expr.replace_attrs(attrs);
            }

            let allow_struct = AllowStruct(true);
            return parse_expr(input, expr, allow_struct, Precedence::MIN);
        }

        if input.peek(Token![.]) && !input.peek(Token![..]) || input.peek(Token![?]) {
            expr = trailer_helper(input, expr)?;

            if let Pattern::Real(ref mut expr) = &mut *expr {
                attrs.extend(expr.replace_attrs(Vec::new()));
                expr.replace_attrs(attrs);
            }

            let allow_struct = AllowStruct(true);
            return parse_expr(input, expr, allow_struct, Precedence::MIN);
        }

        if let Pattern::Real(ref mut expr) = &mut *expr {
            attrs.extend(expr.replace_attrs(Vec::new()));
            expr.replace_attrs(attrs);
        }
        Ok(expr)
    }

    #[cfg(feature = "full")]
    impl Copy for AllowStruct {}

    #[cfg(feature = "full")]
    impl Clone for AllowStruct {
        fn clone(&self) -> Self {
            *self
        }
    }

    fn parse_expr(
        input: ParseStream,
        mut lhs: SpannedPat<Expr>,
        allow_struct: AllowStruct,
        base: Precedence,
    ) -> Result<SpannedPat<Expr>> {
        loop {
            let ahead = input.fork();
            if let Some(Expr::Range(_)) = lhs.real() {
                // A range cannot be the left-hand side of another binary operator.
                break;
            } else if let Ok(op) = ahead.parse::<BinOp>() {
                let precedence = Precedence::of_binop(&op);
                if precedence < base {
                    break;
                }
                if precedence == Precedence::Assign {
                    if let Some(Expr::Range(_)) = lhs.real() {
                        break;
                    }
                }
                if precedence == Precedence::Compare {
                    if let Some(Expr::Binary(lhs)) = lhs.real() {
                        if Precedence::of_binop(&lhs.op) == Precedence::Compare {
                            return Err(input.error("comparison operators cannot be chained"));
                        }
                    }
                }
                input.advance_to(&ahead);
                let right = parse_binop_rhs(input, allow_struct, precedence)?;
                let span = lhs.join(&right);
                lhs = Expr::Binary(ExprBinary {
                    attrs: Vec::new(),
                    left: input.add_pat(lhs),
                    op,
                    right: input.add_pat(right),
                })
                .pattern_with_span(span);
            } else if Precedence::Assign >= base
                && input.peek(Token![=])
                && !input.peek(Token![=>])
                && !matches!(lhs.real(), Some(Expr::Range(_)))
            {
                let eq_token: Token![=] = input.parse()?;
                let right = parse_binop_rhs(input, allow_struct, Precedence::Assign)?;
                let span = lhs.join(&right);
                lhs = Expr::Assign(ExprAssign {
                    attrs: Vec::new(),
                    left: input.add_pat(lhs),
                    eq_token,
                    right: input.add_pat(right),
                })
                .pattern_with_span(span);
            } else if Precedence::Range >= base && input.peek(Token![..]) {
                let limits: RangeLimits = input.parse()?;
                let end = parse_range_end(input, &limits, allow_struct)?;
                let span = end.as_ref().map(|end| lhs.join(end)).unwrap_or(lhs.span());
                lhs = Expr::Range(ExprRange {
                    attrs: Vec::new(),
                    start: Some(input.add_pat(lhs)),
                    limits,
                    end: end.map(|end| input.add_pat(end)),
                })
                .pattern_with_span(span);
            } else if Precedence::Cast >= base && input.peek(Token![as]) {
                let as_token: Token![as] = input.parse()?;
                let ty = input.parse_id::<(Type, NoPlus)>()?;
                check_cast(input)?;
                let span = lhs.span().join(input.ctx().get_span(ty));
                lhs = Expr::Cast(ExprCast {
                    attrs: Vec::new(),
                    expr: input.add_pat(lhs),
                    as_token,
                    ty,
                })
                .pattern_with_span(span);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_binop_rhs(
        input: ParseStream,
        #[cfg(feature = "full")] allow_struct: AllowStruct,
        precedence: Precedence,
    ) -> Result<SpannedPat<Expr>> {
        let mut rhs = unary_expr(
            input,
            #[cfg(feature = "full")]
            allow_struct,
        )?;
        loop {
            let next = peek_precedence(input);
            if next > precedence || next == precedence && precedence == Precedence::Assign {
                let cursor = input.cursor();
                rhs = parse_expr(
                    input,
                    rhs,
                    #[cfg(feature = "full")]
                    allow_struct,
                    next,
                )?;
                if cursor == input.cursor() {
                    // Bespoke grammar restrictions separate from precedence can
                    // cause parsing to not advance, such as `..a` being
                    // disallowed in the left-hand side of binary operators,
                    // even ones that have lower precedence than `..`.
                    break;
                }
            } else {
                break;
            }
        }
        Ok(rhs)
    }

    fn peek_precedence(input: ParseStream) -> Precedence {
        if let Ok(op) = input.fork().parse() {
            Precedence::of_binop(&op)
        } else if input.peek(Token![=]) && !input.peek(Token![=>]) {
            Precedence::Assign
        } else if input.peek(Token![..]) {
            Precedence::Range
        } else if input.peek(Token![as]) {
            Precedence::Cast
        } else {
            Precedence::MIN
        }
    }

    fn ambiguous_expr(input: ParseStream, allow_struct: AllowStruct) -> Result<SpannedPat<Expr>> {
        let lhs = unary_expr(
            input,
            #[cfg(feature = "full")]
            allow_struct,
        )?;
        parse_expr(
            input,
            lhs,
            #[cfg(feature = "full")]
            allow_struct,
            Precedence::MIN,
        )
    }

    #[cfg(feature = "full")]
    fn expr_attrs(input: ParseStream) -> Result<Vec<Attribute>> {
        let mut attrs = Vec::new();
        while !input.peek(token::Group) && input.peek(Token![#]) {
            attrs.push(input.call(attr::parsing::single_parse_outer)?);
        }
        Ok(attrs)
    }

    // <UnOp> <trailer>
    // & <trailer>
    // &mut <trailer>
    // box <trailer>
    #[cfg(feature = "full")]
    fn unary_expr(input: ParseStream, allow_struct: AllowStruct) -> Result<SpannedPat<Expr>> {
        let begin = input.fork();
        let marker = input.marker();
        let attrs = input.call(expr_attrs)?;
        if input.peek(token::Group) {
            return trailer_expr(begin, attrs, input, allow_struct);
        }

        if input.peek(Token![&]) {
            let and_token: Token![&] = input.parse()?;
            let raw: Option<Token![raw]> = if input.peek(Token![raw])
                && (input.peek2(Token![mut]) || input.peek2(Token![const]))
            {
                Some(input.parse()?)
            } else {
                None
            };
            let mutability: Option<Token![mut]> = input.parse()?;
            let const_token: Option<Token![const]> = if raw.is_some() && mutability.is_none() {
                Some(input.parse()?)
            } else {
                None
            };
            let expr = unary_expr(input, allow_struct)?;
            if let Some(raw) = raw {
                Ok(input
                    .from_marker(
                        marker,
                        Expr::RawAddr(ExprRawAddr {
                            attrs,
                            and_token,
                            raw,
                            mutability: match mutability {
                                Some(mut_token) => PointerMutability::Mut(mut_token),
                                None => PointerMutability::Const(const_token.unwrap()),
                            },
                            expr: input.add_pat(expr),
                        }),
                    )
                    .as_pattern())
            } else {
                Ok(input
                    .from_marker(
                        marker,
                        Expr::Reference(ExprReference {
                            attrs,
                            and_token,
                            mutability,
                            expr: input.add_pat(expr),
                        }),
                    )
                    .as_pattern())
            }
        } else if input.peek(Token![*]) || input.peek(Token![!]) || input.peek(Token![-]) {
            Ok(expr_unary(input, attrs, allow_struct)?.map_real(Expr::Unary))
        } else {
            trailer_expr(begin, attrs, input, allow_struct)
        }
    }

    #[cfg(not(feature = "full"))]
    fn unary_expr(input: ParseStream) -> Result<Expr> {
        if input.peek(Token![&]) {
            Ok(Expr::Reference(ExprReference {
                attrs: Vec::new(),
                and_token: input.parse()?,
                mutability: input.parse()?,
                expr: Box::new(unary_expr(input)?),
            }))
        } else if input.peek(Token![*]) || input.peek(Token![!]) || input.peek(Token![-]) {
            Ok(Expr::Unary(ExprUnary {
                attrs: Vec::new(),
                op: input.parse()?,
                expr: Box::new(unary_expr(input)?),
            }))
        } else {
            trailer_expr(input)
        }
    }

    // <atom> (..<args>) ...
    // <atom> . <ident> (..<args>) ...
    // <atom> . <ident> ...
    // <atom> . <lit> ...
    // <atom> [ <expr> ] ...
    // <atom> ? ...
    #[cfg(feature = "full")]
    fn trailer_expr(
        begin: ParseBuffer,
        mut attrs: Vec<Attribute>,
        input: ParseStream,
        allow_struct: AllowStruct,
    ) -> Result<SpannedPat<Expr>> {
        let atom = atom_expr(input, allow_struct)?;
        let e = trailer_helper(input, atom)?;

        if e.is_var() {
            return Ok(e);
        }
        let mut e = e.unwrap_real();

        if let Expr::Verbatim(tokens) = &mut *e {
            *tokens = verbatim::between(&begin, input);
        } else {
            let inner_attrs = e.replace_attrs(Vec::new());
            attrs.extend(inner_attrs);
            e.replace_attrs(attrs);
        }

        Ok(e.as_pattern())
    }

    fn trailer_helper(input: ParseStream, mut e: SpannedPat<Expr>) -> Result<SpannedPat<Expr>> {
        loop {
            let marker = input.marker();
            let orig_span = e.span();
            if input.peek(token::Paren) {
                let content;
                let e2 = Expr::Call(ExprCall {
                    attrs: Vec::new(),
                    func: input.add_pat(e),
                    paren_token: parenthesized!(content in input),
                    args: content.parse_list::<Expr>()?,
                });
                let span = input.span_from_marker(marker).join(orig_span);
                e = e2.pattern_with_span(span);
            } else if input.peek(Token![.])
                && !input.peek(Token![..])
                && !matches!(e.real(), Some(Expr::Range(_)))
            {
                let mut dot_token: Token![.] = input.parse()?;

                let float_token: Option<LitFloat> = input.parse()?;
                if let Some(float_token) = float_token {
                    if multi_index(input, &mut e, &mut dot_token, float_token)? {
                        continue;
                    }
                }

                let await_token: Option<Token![await]> = input.parse()?;
                if let Some(await_token) = await_token {
                    let e2 = Expr::Await(ExprAwait {
                        attrs: Vec::new(),
                        base: input.add_pat(e),
                        dot_token,
                        await_token,
                    });
                    let span = input.span_from_marker(marker).join(orig_span);
                    e = e2.pattern_with_span(span);
                    continue;
                }

                let member: Member = input.parse()?;
                let turbofish = if member.is_named() && input.peek(Token![::]) {
                    Some(AngleBracketedGenericArguments::parse_turbofish(input)?)
                } else {
                    None
                };

                if turbofish.is_some() || input.peek(token::Paren) {
                    if let Member::Named(method) = member {
                        let content;
                        let e2 = Expr::MethodCall(ExprMethodCall {
                            attrs: Vec::new(),
                            receiver: input.add_pat(e),
                            dot_token,
                            method,
                            turbofish,
                            paren_token: parenthesized!(content in input),
                            args: content.parse_list::<Expr>()?,
                        });
                        let span = input.span_from_marker(marker).join(orig_span);
                        e = e2.pattern_with_span(span);
                        continue;
                    }
                }

                let e2 = Expr::Field(ExprField {
                    attrs: Vec::new(),
                    base: input.add_pat(e),
                    dot_token,
                    member,
                });
                let span = input.span_from_marker(marker).join(orig_span);
                e = e2.pattern_with_span(span);
            } else if input.peek(token::Bracket) {
                let content;
                let e2 = Expr::Index(ExprIndex {
                    attrs: Vec::new(),
                    expr: input.add_pat(e),
                    bracket_token: bracketed!(content in input),
                    index: content.parse()?,
                });
                let span = input.span_from_marker(marker).join(orig_span);
                e = e2.pattern_with_span(span);
            } else if input.peek(Token![?]) && !matches!(e.real(), Some(Expr::Range(_))) {
                let e2 = Expr::Try(ExprTry {
                    attrs: Vec::new(),
                    expr: input.add_pat(e),
                    question_token: input.parse()?,
                });
                let span = input.span_from_marker(marker).join(orig_span);
                e = e2.pattern_with_span(span);
            } else {
                break;
            }
        }
        Ok(e)
    }

    // Parse all atomic expressions which don't have to worry about precedence
    // interactions, as they are fully contained.
    fn atom_expr(input: ParseStream, allow_struct: AllowStruct) -> Result<SpannedPat<Expr>> {
        use molt_lib::SpannedPat;

        if let Some(var) = input.parse_var() {
            return var;
        }
        let real: SpannedPat<Expr> = if input.peek(token::Group) {
            input.call_spanned(|input| expr_group(input, allow_struct))?
        } else if input.peek(Lit) {
            input.parse_span_with(Expr::Lit)?
        } else if input.peek(Token![async])
            && (input.peek2(token::Brace) || input.peek2(Token![move]) && input.peek3(token::Brace))
        {
            input.parse_span_with(Expr::Async)?
        } else if input.peek(Token![try]) && input.peek2(token::Brace) {
            input.parse_span_with(Expr::TryBlock)?
        } else if input.peek(Token![|])
            || input.peek(Token![move])
            || input.peek(Token![for])
                && input.peek2(Token![<])
                && (input.peek3(Lifetime) || input.peek3(Token![>]))
            || input.peek(Token![const]) && !input.peek2(token::Brace)
            || input.peek(Token![static])
            || input.peek(Token![async]) && (input.peek2(Token![|]) || input.peek2(Token![move]))
        {
            input.call_spanned(|input| expr_closure(input, allow_struct).map(Expr::Closure))?
        } else if token::parsing::peek_keyword(input.cursor(), "builtin") && input.peek2(Token![#])
        {
            unimplemented!()
        } else if input.peek_pat::<Ident>()
            || input.peek(Token![::])
            || input.peek(Token![<])
            || input.peek(Token![self])
            || input.peek(Token![Self])
            || input.peek(Token![super])
            || input.peek(Token![crate])
            || input.peek(Token![try]) && (input.peek2(Token![!]) || input.peek2(Token![::]))
        {
            input.call_spanned(|input| path_or_macro_or_struct(input, allow_struct))?
        } else if input.peek(token::Paren) {
            input.call_spanned(paren_or_tuple)?
        } else if input.peek(Token![break]) {
            input.call_spanned(|input| expr_break(input, allow_struct).map(Expr::Break))?
        } else if input.peek(Token![continue]) {
            input.parse_span_with(Expr::Continue)?
        } else if input.peek(Token![return]) {
            input.parse_span_with(Expr::Return)?
        } else if input.peek(Token![become]) {
            input.call_spanned(expr_become)?
        } else if input.peek(token::Bracket) {
            input.call_spanned(array_or_repeat)?
        } else if input.peek(Token![let]) {
            input.call_spanned(|input| expr_let(input, allow_struct).map(Expr::Let))?
        } else if input.peek(Token![if]) {
            input.parse_span_with(Expr::If)?
        } else if input.peek(Token![while]) {
            input.parse_span_with(Expr::While)?
        } else if input.peek(Token![for]) {
            input.parse_span_with(Expr::ForLoop)?
        } else if input.peek(Token![loop]) {
            input.parse_span_with(Expr::Loop)?
        } else if input.peek(Token![match]) {
            input.parse_span_with(Expr::Match)?
        } else if input.peek(Token![yield]) {
            input.parse_span_with(Expr::Yield)?
        } else if input.peek(Token![unsafe]) {
            input.parse_span_with(Expr::Unsafe)?
        } else if input.peek(Token![const]) {
            input.parse_span_with(Expr::Const)?
        } else if input.peek(token::Brace) {
            input.parse_span_with(Expr::Block)?
        } else if input.peek(Token![..]) {
            input.call_spanned(|input| expr_range(input, allow_struct).map(Expr::Range))?
        } else if input.peek(Token![_]) {
            input.parse_span_with(Expr::Infer)?
        } else if input.peek(Lifetime) {
            input.call_spanned(atom_labeled)?
        } else {
            Err(input.error("expected an expression"))?
        };
        Ok(real)
    }

    #[cfg(feature = "full")]
    fn atom_labeled(input: ParseStream) -> Result<Expr> {
        let the_label: Label = input.parse()?;
        let mut expr = if input.peek(Token![while]) {
            Expr::While(input.parse()?)
        } else if input.peek(Token![for]) {
            Expr::ForLoop(input.parse()?)
        } else if input.peek(Token![loop]) {
            Expr::Loop(input.parse()?)
        } else if input.peek(token::Brace) {
            Expr::Block(input.parse()?)
        } else {
            return Err(input.error("expected loop or block expression"));
        };
        match &mut expr {
            Expr::While(ExprWhile { label, .. })
            | Expr::ForLoop(ExprForLoop { label, .. })
            | Expr::Loop(ExprLoop { label, .. })
            | Expr::Block(ExprBlock { label, .. }) => *label = Some(the_label),
            _ => unreachable!(),
        }
        Ok(expr)
    }

    #[cfg(not(feature = "full"))]
    fn atom_expr(input: ParseStream) -> Result<Expr> {
        if input.peek(token::Group) {
            expr_group(input)
        } else if input.peek(Lit) {
            input.parse().map(Expr::Lit)
        } else if input.peek(token::Paren) {
            paren_or_tuple(input)
        } else if input.peek_pat::<Ident>()
            || input.peek(Token![::])
            || input.peek(Token![<])
            || input.peek(Token![self])
            || input.peek(Token![Self])
            || input.peek(Token![super])
            || input.peek(Token![crate])
        {
            path_or_macro_or_struct(input)
        } else if input.is_empty() {
            Err(input.error("expected an expression"))
        } else {
            if input.peek(token::Brace) {
                let scan = input.fork();
                let content;
                braced!(content in scan);
                if content.parse::<Expr>().is_ok() && content.is_empty() {
                    let expr_block = verbatim::between(input, &scan);
                    input.advance_to(&scan);
                    return Ok(Expr::Verbatim(expr_block));
                }
            }
            Err(input.error("unsupported expression; enable syn's features=[\"full\"]"))
        }
    }

    fn path_or_macro_or_struct(
        input: ParseStream,
        #[cfg(feature = "full")] allow_struct: AllowStruct,
    ) -> Result<Expr> {
        let expr_style = true;
        let (qself, path) = path::parsing::qpath(input, expr_style)?;
        rest_of_path_or_macro_or_struct(
            qself,
            path,
            input,
            #[cfg(feature = "full")]
            allow_struct,
        )
    }

    fn rest_of_path_or_macro_or_struct(
        qself: Option<QSelf>,
        path: Path,
        input: ParseStream,
        #[cfg(feature = "full")] allow_struct: AllowStruct,
    ) -> Result<Expr> {
        if qself.is_none()
            && input.peek(Token![!])
            && !input.peek(Token![!=])
            && path.is_mod_style()
        {
            let bang_token: Token![!] = input.parse()?;
            let (delimiter, tokens) = mac::parse_delimiter(input)?;
            return Ok(Expr::Macro(ExprMacro {
                attrs: Vec::new(),
                mac: Macro {
                    path,
                    bang_token,
                    delimiter,
                    tokens,
                },
            }));
        }

        #[cfg(not(feature = "full"))]
        let allow_struct = (true,);
        if allow_struct.0 && input.peek(token::Brace) {
            return expr_struct_helper(input, qself, path).map(Expr::Struct);
        }

        Ok(Expr::Path(ExprPath {
            attrs: Vec::new(),
            qself,
            path,
        }))
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprMacro {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprMacro {
                attrs: Vec::new(),
                mac: input.parse()?,
            })
        }
    }

    fn paren_or_tuple(input: ParseStream) -> Result<Expr> {
        let content;
        let paren_token = parenthesized!(content in input);
        Ok(match content.parse_list_or_item::<ExprTuple>()? {
            ListOrItem::Item(expr) => Expr::Paren(ExprParen {
                attrs: Vec::new(),
                paren_token,
                expr: input.add_pat(expr),
            }),
            ListOrItem::List(elems) => Expr::Tuple(ExprTuple {
                attrs: Vec::new(),
                paren_token,
                elems,
            }),
        })
    }

    impl ParseListOrItem for ExprTuple {
        type Target = Expr;

        type Punct = Token![,];

        fn parse_list_or_item(input: ParseStream) -> Result<ListOrItem<Self::Target, Self::Punct>> {
            if input.is_empty() {
                return Ok(ListOrItem::List(NodeList::empty(input.mode())));
            }

            let first = input.parse_pat::<Expr>()?;
            if input.is_empty() {
                return Ok(ListOrItem::Item(first));
            }

            let first = input.add_pat(first);
            let mut elems = Punctuated::<_, Token![,]>::new();
            elems.push_value(first);
            while !input.is_empty() {
                let punct = input.parse()?;
                elems.push_punct(punct);
                if input.is_empty() {
                    break;
                }
                let value = input.parse()?;
                elems.push_value(value);
            }
            Ok(ListOrItem::List(NodeList::Real(
                elems.into_iter().collect(),
            )))
        }
    }

    fn array_or_repeat(input: ParseStream) -> Result<Expr> {
        use crate::parse::ListOrItem;

        let content;
        let bracket_token = bracketed!(content in input);

        match content.parse_list_or_item::<ExprArray>()? {
            ListOrItem::List(elems) => Ok(Expr::Array(ExprArray {
                attrs: Vec::new(),
                bracket_token,
                elems,
            })),
            ListOrItem::Item(expr) => {
                let semi_token: Token![;] = content.parse()?;
                let len: NodeId<Expr> = content.parse()?;
                Ok(Expr::Repeat(ExprRepeat {
                    attrs: Vec::new(),
                    bracket_token,
                    expr: content.add_pat(expr),
                    semi_token,
                    len,
                }))
            }
        }
    }

    impl ParseListOrItem for ExprArray {
        type Target = Expr;

        type Punct = Token![,];

        fn parse_list_or_item(input: ParseStream) -> Result<ListOrItem<Self::Target, Self::Punct>> {
            if input.is_empty() {
                return Ok(ListOrItem::List(NodeList::empty(input.mode())));
            }
            let first = input.parse_pat::<Expr>()?;
            if input.is_empty() || input.peek(Token![,]) {
                let mut elems = Punctuated::<_, Token![,]>::new();
                elems.push_value(input.add_pat(first));
                while !input.is_empty() {
                    let punct = input.parse()?;
                    elems.push_punct(punct);
                    if input.is_empty() {
                        break;
                    }
                    let value: NodeId<Expr> = input.parse()?;
                    elems.push_value(value);
                }
                Ok(ListOrItem::List(NodeList::Real(
                    elems.into_iter().collect(),
                )))
            } else if input.peek(Token![;]) {
                Ok(ListOrItem::Item(first))
            } else {
                Err(input.error("expected `,` or `;`"))
            }
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprRepeat {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(ExprRepeat {
                bracket_token: bracketed!(content in input),
                attrs: Vec::new(),
                expr: content.parse()?,
                semi_token: content.parse()?,
                len: content.parse()?,
            })
        }
    }

    #[cfg(feature = "full")]
    fn continue_parsing_early(input: ParseStream, expr: &SpannedPat<Expr>) -> bool {
        let ctx = input.ctx();
        let mut expr: Option<&Expr> = expr.real();
        while let Some(Expr::Group(group)) = expr {
            expr = if let Some(expr) = ctx.get_real(group.expr) {
                Some(expr)
            } else {
                None
            };
        }
        match expr {
            Some(Expr::If(_))
            | Some(Expr::While(_))
            | Some(Expr::ForLoop(_))
            | Some(Expr::Loop(_))
            | Some(Expr::Match(_))
            | Some(Expr::TryBlock(_))
            | Some(Expr::Unsafe(_))
            | Some(Expr::Const(_))
            | Some(Expr::Block(_)) => false,
            // A variable is an atom-like expression,
            // so we return the same default as for
            // atomic expressions.
            None => true,
            _ => true,
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprLit {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprLit {
                attrs: Vec::new(),
                lit: input.parse()?,
            })
        }
    }

    fn expr_group(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
        let group = crate::group::parse_group(input)?;
        let inner: SpannedPat<Expr> = group.content.parse_pat::<Expr>()?;
        let (span, inner) = inner.decompose();
        let make_group_expr = |inner: Pattern<Expr, Id>| {
            Ok(Expr::Group(ExprGroup {
                attrs: Vec::new(),
                group_token: group.token,
                expr: input.add_pat(inner.with_span(span)),
            }))
        };
        match inner {
            Pattern::Real(Expr::Path(mut expr)) if expr.attrs.is_empty() => {
                let grouped_len = expr.path.segments.len();
                Path::parse_rest(input, &mut expr.path, true)?;
                match rest_of_path_or_macro_or_struct(expr.qself, expr.path, input, allow_struct)? {
                    Expr::Path(expr) if expr.path.segments.len() == grouped_len => {
                        make_group_expr(Pattern::Real(Expr::Path(expr)))
                    }
                    extended => Ok(extended),
                }
            }
            inner => make_group_expr(inner),
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprParen {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(ExprParen {
                attrs: Vec::new(),
                paren_token: parenthesized!(content in input),
                expr: content.parse()?,
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprLet {
        fn parse(input: ParseStream) -> Result<Self> {
            let allow_struct = AllowStruct(true);
            expr_let(input, allow_struct)
        }
    }

    #[cfg(feature = "full")]
    fn expr_let(input: ParseStream, allow_struct: AllowStruct) -> Result<ExprLet> {
        Ok(ExprLet {
            attrs: Vec::new(),
            let_token: input.parse()?,
            pat: Box::new(Pat::parse_multi_with_leading_vert(input)?),
            eq_token: input.parse()?,
            expr: {
                let lhs = unary_expr(input, allow_struct)?;
                input.add_pat(parse_expr(input, lhs, allow_struct, Precedence::Compare)?)
            },
        })
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprIf {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut markers = vec![];
            let attrs = input.call(Attribute::parse_outer)?;
            let mut clauses = Vec::new();
            let mut expr;
            loop {
                markers.push(input.marker());
                let if_token: Token![if] = input.parse()?;
                let cond = input.parse_id::<ExprNoEagerBrace>()?;
                let then_branch: Block = input.parse()?;

                expr = ExprIf {
                    attrs: Vec::new(),
                    if_token,
                    cond,
                    then_branch,
                    else_branch: None,
                };

                if !input.peek(Token![else]) {
                    break;
                }

                let else_token: Token![else] = input.parse()?;
                let lookahead = input.lookahead1();
                if lookahead.peek(Token![if]) {
                    expr.else_branch = Some((else_token, NodeId::placeholder()));
                    clauses.push(expr);
                } else if lookahead.peek(token::Brace) {
                    let block: Spanned<Block> = input.parse_spanned()?;
                    expr.else_branch = Some((
                        else_token,
                        input.add(block.map(|block| {
                            Expr::Block(ExprBlock {
                                attrs: Vec::new(),
                                label: None,
                                block,
                            })
                        })),
                    ));
                    break;
                } else {
                    return Err(lookahead.error());
                }
            }

            while let Some(mut prev) = clauses.pop() {
                let marker = markers.pop().unwrap();
                prev.else_branch.as_mut().unwrap().1 =
                    input.add(Expr::If(expr).with_span(input.span_from_marker(marker)));
                expr = prev;
            }
            expr.attrs = attrs;
            Ok(expr)
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprInfer {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprInfer {
                attrs: input.call(Attribute::parse_outer)?,
                underscore_token: input.parse()?,
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprForLoop {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut attrs = input.call(Attribute::parse_outer)?;
            let label: Option<Label> = input.parse()?;
            let for_token: Token![for] = input.parse()?;

            let pat = Pat::parse_multi_with_leading_vert(input)?;

            let in_token: Token![in] = input.parse()?;
            let expr = input.parse_id::<ExprNoEagerBrace>()?;

            let content;
            let brace_token = braced!(content in input);
            attr::parsing::parse_inner(&content, &mut attrs)?;
            let stmts = content.parse_list::<Stmt>()?;

            Ok(ExprForLoop {
                attrs,
                label,
                for_token,
                pat: Box::new(pat),
                in_token,
                expr,
                body: Block { brace_token, stmts },
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprLoop {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut attrs = input.call(Attribute::parse_outer)?;
            let label: Option<Label> = input.parse()?;
            let loop_token: Token![loop] = input.parse()?;

            let content;
            let brace_token = braced!(content in input);
            attr::parsing::parse_inner(&content, &mut attrs)?;
            let stmts = content.parse_list::<Stmt>()?;

            Ok(ExprLoop {
                attrs,
                label,
                loop_token,
                body: Block { brace_token, stmts },
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprMatch {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut attrs = input.call(Attribute::parse_outer)?;
            let match_token: Token![match] = input.parse()?;
            let expr = input.parse_id::<ExprNoEagerBrace>()?;

            let content;
            let brace_token = braced!(content in input);
            attr::parsing::parse_inner(&content, &mut attrs)?;

            let arms = content.parse_list::<Arm>()?;

            Ok(ExprMatch {
                attrs,
                match_token,
                expr,
                brace_token,
                arms,
            })
        }
    }

    macro_rules! impl_by_parsing_expr {
        (
            $(
                $expr_type:ty, $variant:ident, $msg:expr,
            )*
        ) => {
            $(
                #[cfg(all(feature = "full", feature = "printing"))]
                #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
                impl Parse for $expr_type {
                    fn parse(input: ParseStream) -> Result<Self> {
                        let mut expr: Expr = input.parse()?;
                        loop {
                            match expr {
                                Expr::$variant(inner) => return Ok(inner),
                                Expr::Group(next) => expr = *next.expr,
                                _ => return Err(Error::new_spanned(expr, $msg)),
                            }
                        }
                    }
                }
            )*
        };
    }

    impl_by_parsing_expr! {
        ExprAssign, Assign, "expected assignment expression",
        ExprAwait, Await, "expected await expression",
        ExprBinary, Binary, "expected binary operation",
        ExprCall, Call, "expected function call expression",
        ExprCast, Cast, "expected cast expression",
        ExprField, Field, "expected struct field access",
        ExprIndex, Index, "expected indexing expression",
        ExprMethodCall, MethodCall, "expected method call expression",
        ExprRange, Range, "expected range expression",
        ExprTry, Try, "expected try expression",
        ExprTuple, Tuple, "expected tuple expression",
    }

    #[cfg(feature = "full")]
    fn expr_unary(
        input: ParseStream,
        attrs: Vec<Attribute>,
        allow_struct: AllowStruct,
    ) -> Result<SpannedPat<ExprUnary>> {
        let marker = input.marker();
        Ok(input
            .from_marker(
                marker,
                ExprUnary {
                    attrs,
                    op: input.parse()?,
                    expr: input.add_pat(unary_expr(input, allow_struct)?),
                },
            )
            .as_pattern())
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprRawAddr {
        fn parse(input: ParseStream) -> Result<Self> {
            let allow_struct = AllowStruct(true);
            Ok(ExprRawAddr {
                attrs: Vec::new(),
                and_token: input.parse()?,
                raw: input.parse()?,
                mutability: input.parse()?,
                expr: input.add_pat(unary_expr(input, allow_struct)?),
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprReference {
        fn parse(input: ParseStream) -> Result<Self> {
            let allow_struct = AllowStruct(true);
            Ok(ExprReference {
                attrs: Vec::new(),
                and_token: input.parse()?,
                mutability: input.parse()?,
                expr: input.add_pat(unary_expr(input, allow_struct)?),
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprBreak {
        fn parse(input: ParseStream) -> Result<Self> {
            let allow_struct = AllowStruct(true);
            expr_break(input, allow_struct)
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprReturn {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprReturn {
                attrs: Vec::new(),
                return_token: input.parse()?,
                expr: {
                    if Expr::peek(input) {
                        Some(input.parse()?)
                    } else {
                        None
                    }
                },
            })
        }
    }

    #[cfg(feature = "full")]
    fn expr_become(input: ParseStream) -> Result<Expr> {
        let begin = input.fork();
        input.parse::<Token![become]>()?;
        input.parse::<NodeId<Expr>>()?;
        Ok(Expr::Verbatim(verbatim::between(&begin, input)))
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprTryBlock {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprTryBlock {
                attrs: Vec::new(),
                try_token: input.parse()?,
                block: input.parse()?,
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprYield {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprYield {
                attrs: Vec::new(),
                yield_token: input.parse()?,
                expr: {
                    if Expr::peek(input) {
                        Some(input.parse()?)
                    } else {
                        None
                    }
                },
            })
        }
    }

    #[cfg(feature = "full")]
    fn expr_closure(input: ParseStream, allow_struct: AllowStruct) -> Result<ExprClosure> {
        let lifetimes: Option<BoundLifetimes> = input.parse()?;
        let constness: Option<Token![const]> = input.parse()?;
        let movability: Option<Token![static]> = input.parse()?;
        let asyncness: Option<Token![async]> = input.parse()?;
        let capture: Option<Token![move]> = input.parse()?;
        let or1_token: Token![|] = input.parse()?;

        let mut inputs = Punctuated::new();
        loop {
            if input.peek(Token![|]) {
                break;
            }
            let value = closure_arg(input)?;
            inputs.push_value(value);
            if input.peek(Token![|]) {
                break;
            }
            let punct: Token![,] = input.parse()?;
            inputs.push_punct(punct);
        }

        let or2_token: Token![|] = input.parse()?;

        let (output, body) = if input.peek(Token![->]) {
            let arrow_token: Token![->] = input.parse()?;
            let ty = input.parse()?;
            let body: Spanned<Block> = input.parse_spanned()?;
            let output = ReturnType::Type(arrow_token, ty);
            let block = input.add(body.map(|body| {
                Expr::Block(ExprBlock {
                    attrs: Vec::new(),
                    label: None,
                    block: body,
                })
            }));
            (output, block)
        } else {
            let body = if allow_struct.0 {
                input.parse_id::<Expr>()?
            } else {
                input.parse_id::<ExprNoEagerBrace>()?
            };
            (ReturnType::Default, body)
        };

        Ok(ExprClosure {
            attrs: Vec::new(),
            lifetimes,
            constness,
            movability,
            asyncness,
            capture,
            or1_token,
            inputs,
            or2_token,
            output,
            body,
        })
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprAsync {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprAsync {
                attrs: Vec::new(),
                async_token: input.parse()?,
                capture: input.parse()?,
                block: input.parse()?,
            })
        }
    }

    #[cfg(feature = "full")]
    fn closure_arg(input: ParseStream) -> Result<Pat> {
        let attrs = input.call(Attribute::parse_outer)?;
        let mut pat = Pat::parse_single(input)?;

        if input.peek(Token![:]) {
            Ok(Pat::Type(PatType {
                attrs,
                pat: Box::new(pat),
                colon_token: input.parse()?,
                ty: input.parse()?,
            }))
        } else {
            match &mut pat {
                Pat::Const(pat) => pat.attrs = attrs,
                Pat::Ident(pat) => pat.attrs = attrs,
                Pat::Lit(pat) => pat.attrs = attrs,
                Pat::Macro(pat) => pat.attrs = attrs,
                Pat::Or(pat) => pat.attrs = attrs,
                Pat::Paren(pat) => pat.attrs = attrs,
                Pat::Path(pat) => pat.attrs = attrs,
                Pat::Range(pat) => pat.attrs = attrs,
                Pat::Reference(pat) => pat.attrs = attrs,
                Pat::Rest(pat) => pat.attrs = attrs,
                Pat::Slice(pat) => pat.attrs = attrs,
                Pat::Struct(pat) => pat.attrs = attrs,
                Pat::Tuple(pat) => pat.attrs = attrs,
                Pat::TupleStruct(pat) => pat.attrs = attrs,
                Pat::Type(_) => unreachable!(),
                Pat::Verbatim(_) => {}
                Pat::Wild(pat) => pat.attrs = attrs,
            }
            Ok(pat)
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprWhile {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut attrs = input.call(Attribute::parse_outer)?;
            let label: Option<Label> = input.parse()?;
            let while_token: Token![while] = input.parse()?;
            let cond = input.parse_id::<ExprNoEagerBrace>()?;

            let content;
            let brace_token = braced!(content in input);
            attr::parsing::parse_inner(&content, &mut attrs)?;
            let stmts = content.parse_list::<Stmt>()?;

            Ok(ExprWhile {
                attrs,
                label,
                while_token,
                cond,
                body: Block { brace_token, stmts },
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprConst {
        fn parse(input: ParseStream) -> Result<Self> {
            let const_token: Token![const] = input.parse()?;

            let content;
            let brace_token = braced!(content in input);
            let inner_attrs = content.call(Attribute::parse_inner)?;
            let stmts = content.parse_list::<Stmt>()?;

            Ok(ExprConst {
                attrs: inner_attrs,
                const_token,
                block: Block { brace_token, stmts },
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Label {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(Label {
                name: input.parse()?,
                colon_token: input.parse()?,
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Option<Label> {
        fn parse(input: ParseStream) -> Result<Self> {
            if input.peek(Lifetime) {
                input.parse().map(Some)
            } else {
                Ok(None)
            }
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprContinue {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(ExprContinue {
                attrs: Vec::new(),
                continue_token: input.parse()?,
                label: input.parse()?,
            })
        }
    }

    #[cfg(feature = "full")]
    fn expr_break(input: ParseStream, allow_struct: AllowStruct) -> Result<ExprBreak> {
        let break_token: Token![break] = input.parse()?;

        let ahead = input.fork();
        let label: Option<Lifetime> = ahead.parse()?;
        if label.is_some() && ahead.peek(Token![:]) {
            // Not allowed: `break 'label: loop {...}`
            // Parentheses are required. `break ('label: loop {...})`
            let _: NodeId<Expr> = input.parse()?;
            let start_span = label.unwrap().apostrophe;
            let end_span = input.cursor().prev_span();
            return Err(crate::error::new2(
                start_span,
                end_span,
                "parentheses required",
            ));
        }

        input.advance_to(&ahead);
        let expr = if Expr::peek(input) && (allow_struct.0 || !input.peek(token::Brace)) {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(ExprBreak {
            attrs: Vec::new(),
            break_token,
            label,
            expr,
        })
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for FieldValue {
        fn parse(input: ParseStream) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let member: Spanned<Member> = input.parse_spanned()?;
            let (colon_token, value) = if input.peek(Token![:]) || !member.is_named() {
                let colon_token: Token![:] = input.parse()?;
                let value: NodeId<Expr> = input.parse()?;
                (Some(colon_token), value)
            } else if let Member::Named(ident) = &*member {
                let span = member.span();
                let value = input.add(
                    Expr::Path(ExprPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: Path::from(*ident),
                    })
                    .with_span(span),
                );
                (None, value)
            } else {
                unreachable!()
            };

            Ok(FieldValue {
                attrs,
                member: member.take(),
                colon_token,
                expr: value,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprStruct {
        fn parse(input: ParseStream) -> Result<Self> {
            let expr_style = true;
            let (qself, path) = path::parsing::qpath(input, expr_style)?;
            expr_struct_helper(input, qself, path)
        }
    }

    fn expr_struct_helper(
        input: ParseStream,
        qself: Option<QSelf>,
        path: Path,
    ) -> Result<ExprStruct> {
        let content;
        let brace_token = braced!(content in input);

        let mut fields = Punctuated::new();
        while !content.is_empty() {
            if content.peek(Token![..]) {
                return Ok(ExprStruct {
                    attrs: Vec::new(),
                    qself,
                    path,
                    brace_token,
                    fields,
                    dot2_token: Some(content.parse()?),
                    rest: if content.is_empty() {
                        None
                    } else {
                        Some(content.parse()?)
                    },
                });
            }

            fields.push(content.parse()?);
            if content.is_empty() {
                break;
            }
            let punct: Token![,] = content.parse()?;
            fields.push_punct(punct);
        }

        Ok(ExprStruct {
            attrs: Vec::new(),
            qself,
            path,
            brace_token,
            fields,
            dot2_token: None,
            rest: None,
        })
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprUnsafe {
        fn parse(input: ParseStream) -> Result<Self> {
            let unsafe_token: Token![unsafe] = input.parse()?;

            let content;
            let brace_token = braced!(content in input);
            let inner_attrs = content.call(Attribute::parse_inner)?;
            let stmts = content.parse_list::<Stmt>()?;

            Ok(ExprUnsafe {
                attrs: inner_attrs,
                unsafe_token,
                block: Block { brace_token, stmts },
            })
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprBlock {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut attrs = input.call(Attribute::parse_outer)?;
            let label: Option<Label> = input.parse()?;

            let content;
            let brace_token = braced!(content in input);
            attr::parsing::parse_inner(&content, &mut attrs)?;
            let stmts = content.parse_list::<Stmt>()?;

            Ok(ExprBlock {
                attrs,
                label,
                block: Block { brace_token, stmts },
            })
        }
    }

    #[cfg(feature = "full")]
    fn expr_range(input: ParseStream, allow_struct: AllowStruct) -> Result<ExprRange> {
        let limits: RangeLimits = input.parse()?;
        let end = parse_range_end(input, &limits, allow_struct)?;
        Ok(ExprRange {
            attrs: Vec::new(),
            start: None,
            limits,
            end: end.map(|end| input.add_pat(end)),
        })
    }

    #[cfg(feature = "full")]
    fn parse_range_end(
        input: ParseStream,
        limits: &RangeLimits,
        allow_struct: AllowStruct,
    ) -> Result<Option<SpannedPat<Expr>>> {
        if matches!(limits, RangeLimits::HalfOpen(_))
            && (input.is_empty()
                || input.peek(Token![,])
                || input.peek(Token![;])
                || input.peek(Token![.]) && !input.peek(Token![..])
                || input.peek(Token![?])
                || input.peek(Token![=>])
                || !allow_struct.0 && input.peek(token::Brace)
                || input.peek(Token![=])
                || input.peek(Token![+])
                || input.peek(Token![/])
                || input.peek(Token![%])
                || input.peek(Token![^])
                || input.peek(Token![>])
                || input.peek(Token![<=])
                || input.peek(Token![!=])
                || input.peek(Token![-=])
                || input.peek(Token![*=])
                || input.peek(Token![&=])
                || input.peek(Token![|=])
                || input.peek(Token![<<=])
                || input.peek(Token![as]))
        {
            Ok(None)
        } else {
            let end = parse_binop_rhs(input, allow_struct, Precedence::Range)?;
            Ok(Some(end))
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for RangeLimits {
        fn parse(input: ParseStream) -> Result<Self> {
            let lookahead = input.lookahead1();
            let dot_dot = lookahead.peek(Token![..]);
            let dot_dot_eq = dot_dot && lookahead.peek(Token![..=]);
            let dot_dot_dot = dot_dot && input.peek(Token![...]);
            if dot_dot_eq {
                input.parse().map(RangeLimits::Closed)
            } else if dot_dot && !dot_dot_dot {
                input.parse().map(RangeLimits::HalfOpen)
            } else {
                Err(lookahead.error())
            }
        }
    }

    #[cfg(feature = "full")]
    impl RangeLimits {
        pub(crate) fn parse_obsolete(input: ParseStream) -> Result<Self> {
            let lookahead = input.lookahead1();
            let dot_dot = lookahead.peek(Token![..]);
            let dot_dot_eq = dot_dot && lookahead.peek(Token![..=]);
            let dot_dot_dot = dot_dot && input.peek(Token![...]);
            if dot_dot_eq {
                input.parse().map(RangeLimits::Closed)
            // deprecated in rust 1.26, removed in  1.53
            } else if dot_dot_dot {
                let dot3: Token![...] = input.parse()?;
                Ok(RangeLimits::Closed(Token![..=](dot3.spans)))
            } else if dot_dot {
                input.parse().map(RangeLimits::HalfOpen)
            } else {
                Err(lookahead.error())
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for ExprPath {
        fn parse(input: ParseStream) -> Result<Self> {
            #[cfg(not(feature = "full"))]
            let attrs = Vec::new();
            #[cfg(feature = "full")]
            let attrs = input.call(Attribute::parse_outer)?;

            let expr_style = true;
            let (qself, path) = path::parsing::qpath(input, expr_style)?;

            Ok(ExprPath { attrs, qself, path })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Member {
        fn parse(input: ParseStream) -> Result<Self> {
            if input.peek_pat::<Ident>() {
                input.parse().map(Member::Named)
            } else if input.peek(LitInt) {
                input.parse().map(Member::Unnamed)
            } else {
                Err(input.error("expected identifier or integer"))
            }
        }
    }

    impl ParseList for Arm {
        type Punct = Token![,];

        fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Arm>>> {
            let mut arms = Vec::new();
            while !input.is_empty() {
                arms.push(input.parse_id::<Arm>()?);
            }
            Ok(arms)
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl ParsePat for Arm {
        type Target = Arm;

        fn parse_pat(input: ParseStream) -> Result<SpannedPat<Arm>> {
            input.call_spanned(|input| {
                let requires_comma;
                Ok(Arm {
                    attrs: input.call(Attribute::parse_outer)?,
                    pat: Pat::parse_multi_with_leading_vert(input)?,
                    guard: {
                        if input.peek(Token![if]) {
                            let if_token: Token![if] = input.parse()?;
                            let guard: NodeId<Expr> = input.parse()?;
                            Some((if_token, guard))
                        } else {
                            None
                        }
                    },
                    fat_arrow_token: input.parse()?,
                    body: {
                        let body = input.parse_id::<ExprEarlierBoundaryRule>()?;
                        requires_comma = classify::requires_comma_to_be_match_arm(input, body);
                        body
                    },
                    comma: {
                        if requires_comma && !input.is_empty() {
                            Some(input.parse()?)
                        } else {
                            input.parse()?
                        }
                    },
                })
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Index {
        fn parse(input: ParseStream) -> Result<Self> {
            let lit: LitInt = input.parse()?;
            if lit.suffix().is_empty() {
                Ok(Index {
                    index: lit
                        .base10_digits()
                        .parse()
                        .map_err(|err| Error::new(lit.span(), err))?,
                    span: lit.span(),
                })
            } else {
                Err(Error::new(lit.span(), "expected unsuffixed integer"))
            }
        }
    }

    fn multi_index(
        input: ParseStream,
        e: &mut SpannedPat<Expr>,
        dot_token: &mut Token![.],
        float: LitFloat,
    ) -> Result<bool> {
        let float_token = float.token();
        let float_span = float_token.span();
        let mut float_repr = float_token.to_string();
        let trailing_dot = float_repr.ends_with('.');
        if trailing_dot {
            float_repr.truncate(float_repr.len() - 1);
        }

        let mut offset = 0;
        for part in float_repr.split('.') {
            let mut index: Index = crate::parse::parse_str(part, input.mode())
                .map_err(|err| Error::new(float_span, err))?;
            let part_end = offset + part.len();
            index.span = float_token.subspan(offset..part_end).unwrap_or(float_span);

            let base = mem::replace(
                e,
                Expr::PLACEHOLDER.pattern_with_span(molt_lib::Span::fake()),
            );
            let span = base.span().join(index.span.byte_range());
            *e = Expr::Field(ExprField {
                attrs: Vec::new(),
                base: input.add_pat(base),
                dot_token: Token![.](dot_token.span),
                member: Member::Unnamed(index),
            })
            .pattern_with_span(span);

            let dot_span = float_token
                .subspan(part_end..part_end + 1)
                .unwrap_or(float_span);
            *dot_token = Token![.](dot_span);
            offset = part_end + 1;
        }

        Ok(!trailing_dot)
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for PointerMutability {
        fn parse(input: ParseStream) -> Result<Self> {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![const]) {
                Ok(PointerMutability::Const(input.parse()?))
            } else if lookahead.peek(Token![mut]) {
                Ok(PointerMutability::Mut(input.parse()?))
            } else {
                Err(lookahead.error())
            }
        }
    }

    fn check_cast(input: ParseStream) -> Result<()> {
        let kind = if input.peek(Token![.]) && !input.peek(Token![..]) {
            if input.peek2(Token![await]) {
                "`.await`"
            } else if input.peek2(Ident) && (input.peek3(token::Paren) || input.peek3(Token![::])) {
                "a method call"
            } else {
                "a field access"
            }
        } else if input.peek(Token![?]) {
            "`?`"
        } else if input.peek(token::Bracket) {
            "indexing"
        } else if input.peek(token::Paren) {
            "a function call"
        } else {
            return Ok(());
        };
        let msg = format!("casts cannot be followed by {}", kind);
        Err(input.error(msg))
    }
}

#[cfg(feature = "printing")]
pub(crate) mod printing {
    use crate::attr::Attribute;
    #[cfg(feature = "full")]
    use crate::attr::FilterAttrs;
    #[cfg(feature = "full")]
    use crate::classify;
    #[cfg(feature = "full")]
    use crate::expr::{
        Arm, ExprArray, ExprAssign, ExprAsync, ExprAwait, ExprBlock, ExprBreak, ExprClosure,
        ExprConst, ExprContinue, ExprForLoop, ExprIf, ExprInfer, ExprLet, ExprLoop, ExprMatch,
        ExprRange, ExprRawAddr, ExprRepeat, ExprReturn, ExprTry, ExprTryBlock, ExprUnsafe,
        ExprWhile, ExprYield, Label, PointerMutability, RangeLimits,
    };
    use crate::expr::{
        Expr, ExprBinary, ExprCall, ExprCast, ExprField, ExprGroup, ExprIndex, ExprLit, ExprMacro,
        ExprMethodCall, ExprParen, ExprPath, ExprReference, ExprStruct, ExprTuple, ExprUnary,
        FieldValue, Index, Member,
    };
    use crate::fixup::FixupContext;
    use crate::op::BinOp;
    use crate::path;
    use crate::path::printing::PathStyle;
    use crate::precedence::Precedence;
    use crate::token;
    #[cfg(feature = "full")]
    use crate::ty::ReturnType;
    use proc_macro2::{Literal, Span, TokenStream};
    use quote::{ToTokens, TokenStreamExt};

    #[cfg(feature = "full")]
    pub(crate) fn outer_attrs_to_tokens(attrs: &[Attribute], tokens: &mut TokenStream) {
        tokens.append_all(attrs.outer());
    }

    #[cfg(feature = "full")]
    fn inner_attrs_to_tokens(attrs: &[Attribute], tokens: &mut TokenStream) {
        tokens.append_all(attrs.inner());
    }

    #[cfg(not(feature = "full"))]
    pub(crate) fn outer_attrs_to_tokens(_attrs: &[Attribute], _tokens: &mut TokenStream) {}

    pub(crate) fn print_subexpression(
        expr: &Expr,
        needs_group: bool,
        tokens: &mut TokenStream,
        mut fixup: FixupContext,
    ) {
        if needs_group {
            // If we are surrounding the whole cond in parentheses, such as:
            //
            //     if (return Struct {}) {}
            //
            // then there is no need for parenthesizing the individual struct
            // expressions within. On the other hand if the whole cond is not
            // parenthesized, then print_expr must parenthesize exterior struct
            // literals.
            //
            //     if x == (Struct {}) {}
            //
            fixup = FixupContext::NONE;
        }

        let do_print_expr = |tokens: &mut TokenStream| print_expr(expr, tokens, fixup);

        if needs_group {
            token::Paren::default().surround(tokens, do_print_expr);
        } else {
            do_print_expr(tokens);
        }
    }

    pub(crate) fn print_expr(expr: &Expr, tokens: &mut TokenStream, mut fixup: FixupContext) {
        #[cfg(feature = "full")]
        let needs_group = fixup.parenthesize(expr);
        #[cfg(not(feature = "full"))]
        let needs_group = false;

        if needs_group {
            fixup = FixupContext::NONE;
        }

        let do_print_expr = |tokens: &mut TokenStream| match expr {
            #[cfg(feature = "full")]
            Expr::Array(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Assign(e) => print_expr_assign(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::Async(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Await(e) => print_expr_await(e, tokens, fixup),
            Expr::Binary(e) => print_expr_binary(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::Block(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Break(e) => print_expr_break(e, tokens, fixup),
            Expr::Call(e) => print_expr_call(e, tokens, fixup),
            Expr::Cast(e) => print_expr_cast(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::Closure(e) => print_expr_closure(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::Const(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Continue(e) => e.to_tokens(tokens),
            Expr::Field(e) => print_expr_field(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::ForLoop(e) => e.to_tokens(tokens),
            Expr::Group(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::If(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Index(e) => print_expr_index(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::Infer(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Let(e) => print_expr_let(e, tokens, fixup),
            Expr::Lit(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Loop(e) => e.to_tokens(tokens),
            Expr::Macro(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Match(e) => e.to_tokens(tokens),
            Expr::MethodCall(e) => print_expr_method_call(e, tokens, fixup),
            Expr::Paren(e) => e.to_tokens(tokens),
            Expr::Path(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Range(e) => print_expr_range(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::RawAddr(e) => print_expr_raw_addr(e, tokens, fixup),
            Expr::Reference(e) => print_expr_reference(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::Repeat(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Return(e) => print_expr_return(e, tokens, fixup),
            Expr::Struct(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Try(e) => print_expr_try(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::TryBlock(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Tuple(e) => e.to_tokens(tokens),
            Expr::Unary(e) => print_expr_unary(e, tokens, fixup),
            #[cfg(feature = "full")]
            Expr::Unsafe(e) => e.to_tokens(tokens),
            Expr::Verbatim(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::While(e) => e.to_tokens(tokens),
            #[cfg(feature = "full")]
            Expr::Yield(e) => print_expr_yield(e, tokens, fixup),

            #[cfg(not(feature = "full"))]
            _ => unreachable!(),
        };

        if needs_group {
            token::Paren::default().surround(tokens, do_print_expr);
        } else {
            do_print_expr(tokens);
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprArray {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.bracket_token.surround(tokens, |tokens| {
                self.elems.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprAssign {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_assign(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_assign(e: &ExprAssign, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        let (left_prec, left_fixup) =
            fixup.leftmost_subexpression_with_operator(&e.left, false, false, Precedence::Assign);
        print_subexpression(&e.left, left_prec <= Precedence::Range, tokens, left_fixup);
        e.eq_token.to_tokens(tokens);
        print_expr(
            &e.right,
            tokens,
            fixup.rightmost_subexpression_fixup(false, false, Precedence::Assign),
        );
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprAsync {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.async_token.to_tokens(tokens);
            self.capture.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprAwait {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_await(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_await(e: &ExprAwait, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        let (left_prec, left_fixup) = fixup.leftmost_subexpression_with_dot(&e.base);
        print_subexpression(
            &e.base,
            left_prec < Precedence::Unambiguous,
            tokens,
            left_fixup,
        );
        e.dot_token.to_tokens(tokens);
        e.await_token.to_tokens(tokens);
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprBinary {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_binary(self, tokens, FixupContext::NONE);
        }
    }

    fn print_expr_binary(e: &ExprBinary, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);

        let binop_prec = Precedence::of_binop(&e.op);
        let (left_prec, left_fixup) = fixup.leftmost_subexpression_with_operator(
            &e.left,
            #[cfg(feature = "full")]
            match &e.op {
                BinOp::Sub(_)
                | BinOp::Mul(_)
                | BinOp::And(_)
                | BinOp::Or(_)
                | BinOp::BitAnd(_)
                | BinOp::BitOr(_)
                | BinOp::Shl(_)
                | BinOp::Lt(_) => true,
                _ => false,
            },
            match &e.op {
                BinOp::Shl(_) | BinOp::Lt(_) => true,
                _ => false,
            },
            #[cfg(feature = "full")]
            binop_prec,
        );
        let left_needs_group = match binop_prec {
            Precedence::Assign => left_prec <= Precedence::Range,
            Precedence::Compare => left_prec <= binop_prec,
            _ => left_prec < binop_prec,
        };

        let right_fixup = fixup.rightmost_subexpression_fixup(
            #[cfg(feature = "full")]
            false,
            #[cfg(feature = "full")]
            false,
            #[cfg(feature = "full")]
            binop_prec,
        );
        let right_needs_group = binop_prec != Precedence::Assign
            && right_fixup.rightmost_subexpression_precedence(&e.right) <= binop_prec;

        print_subexpression(&e.left, left_needs_group, tokens, left_fixup);
        e.op.to_tokens(tokens);
        print_subexpression(&e.right, right_needs_group, tokens, right_fixup);
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprBlock {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.label.to_tokens(tokens);
            self.block.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.block.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprBreak {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_break(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_break(e: &ExprBreak, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        e.break_token.to_tokens(tokens);
        e.label.to_tokens(tokens);
        if let Some(value) = &e.expr {
            print_subexpression(
                value,
                // Parenthesize `break 'inner: loop { break 'inner 1 } + 1`
                //                     ^---------------------------------^
                e.label.is_none() && classify::expr_leading_label(value),
                tokens,
                fixup.rightmost_subexpression_fixup(true, true, Precedence::Jump),
            );
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprCall {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_call(self, tokens, FixupContext::NONE);
        }
    }

    fn print_expr_call(e: &ExprCall, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);

        let (left_prec, left_fixup) = fixup.leftmost_subexpression_with_operator(
            &e.func,
            #[cfg(feature = "full")]
            true,
            false,
            #[cfg(feature = "full")]
            Precedence::Unambiguous,
        );
        let needs_group = if let Expr::Field(func) = &*e.func {
            func.member.is_named()
        } else {
            left_prec < Precedence::Unambiguous
        };
        print_subexpression(&e.func, needs_group, tokens, left_fixup);

        e.paren_token.surround(tokens, |tokens| {
            e.args.to_tokens(tokens);
        });
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprCast {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_cast(self, tokens, FixupContext::NONE);
        }
    }

    fn print_expr_cast(e: &ExprCast, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        let (left_prec, left_fixup) = fixup.leftmost_subexpression_with_operator(
            &e.expr,
            #[cfg(feature = "full")]
            false,
            false,
            #[cfg(feature = "full")]
            Precedence::Cast,
        );
        print_subexpression(&e.expr, left_prec < Precedence::Cast, tokens, left_fixup);
        e.as_token.to_tokens(tokens);
        e.ty.to_tokens(tokens);
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprClosure {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_closure(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_closure(e: &ExprClosure, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        e.lifetimes.to_tokens(tokens);
        e.constness.to_tokens(tokens);
        e.movability.to_tokens(tokens);
        e.asyncness.to_tokens(tokens);
        e.capture.to_tokens(tokens);
        e.or1_token.to_tokens(tokens);
        e.inputs.to_tokens(tokens);
        e.or2_token.to_tokens(tokens);
        e.output.to_tokens(tokens);
        if matches!(e.output, ReturnType::Default)
            || matches!(&*e.body, Expr::Block(body) if body.attrs.is_empty() && body.label.is_none())
        {
            print_expr(
                &e.body,
                tokens,
                fixup.rightmost_subexpression_fixup(false, false, Precedence::Jump),
            );
        } else {
            token::Brace::default().surround(tokens, |tokens| {
                print_expr(&e.body, tokens, FixupContext::new_stmt());
            });
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprConst {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.const_token.to_tokens(tokens);
            self.block.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.block.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprContinue {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.continue_token.to_tokens(tokens);
            self.label.to_tokens(tokens);
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprField {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_field(self, tokens, FixupContext::NONE);
        }
    }

    fn print_expr_field(e: &ExprField, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        let (left_prec, left_fixup) = fixup.leftmost_subexpression_with_dot(&e.base);
        print_subexpression(
            &e.base,
            left_prec < Precedence::Unambiguous,
            tokens,
            left_fixup,
        );
        e.dot_token.to_tokens(tokens);
        e.member.to_tokens(tokens);
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprForLoop {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.label.to_tokens(tokens);
            self.for_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
            self.in_token.to_tokens(tokens);
            print_expr(&self.expr, tokens, FixupContext::new_condition());
            self.body.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.body.stmts);
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprGroup {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.group_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprIf {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);

            let mut expr = self;
            loop {
                expr.if_token.to_tokens(tokens);
                print_expr(&expr.cond, tokens, FixupContext::new_condition());
                expr.then_branch.to_tokens(tokens);

                let (else_token, else_) = match &expr.else_branch {
                    Some(else_branch) => else_branch,
                    None => break,
                };

                else_token.to_tokens(tokens);
                match &**else_ {
                    Expr::If(next) => {
                        expr = next;
                    }
                    Expr::Block(last) => {
                        last.to_tokens(tokens);
                        break;
                    }
                    // If this is not one of the valid expressions to exist in
                    // an else clause, wrap it in a block.
                    other => {
                        token::Brace::default().surround(tokens, |tokens| {
                            print_expr(other, tokens, FixupContext::new_stmt());
                        });
                        break;
                    }
                }
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprIndex {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_index(self, tokens, FixupContext::NONE);
        }
    }

    fn print_expr_index(e: &ExprIndex, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        let (left_prec, left_fixup) = fixup.leftmost_subexpression_with_operator(
            &e.expr,
            #[cfg(feature = "full")]
            true,
            false,
            #[cfg(feature = "full")]
            Precedence::Unambiguous,
        );
        print_subexpression(
            &e.expr,
            left_prec < Precedence::Unambiguous,
            tokens,
            left_fixup,
        );
        e.bracket_token.surround(tokens, |tokens| {
            e.index.to_tokens(tokens);
        });
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprInfer {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.underscore_token.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprLet {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_let(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_let(e: &ExprLet, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        e.let_token.to_tokens(tokens);
        e.pat.to_tokens(tokens);
        e.eq_token.to_tokens(tokens);
        let (right_prec, right_fixup) = fixup.rightmost_subexpression(&e.expr, Precedence::Let);
        print_subexpression(&e.expr, right_prec < Precedence::Let, tokens, right_fixup);
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprLit {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.lit.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprLoop {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.label.to_tokens(tokens);
            self.loop_token.to_tokens(tokens);
            self.body.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.body.stmts);
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprMacro {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.mac.to_tokens(tokens);
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprMatch {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.match_token.to_tokens(tokens);
            print_expr(&self.expr, tokens, FixupContext::new_condition());
            self.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                for (i, arm) in self.arms.iter().enumerate() {
                    arm.to_tokens(tokens);
                    // Ensure that we have a comma after a non-block arm, except
                    // for the last one.
                    let is_last = i == self.arms.len() - 1;
                    if !is_last
                        && classify::requires_comma_to_be_match_arm(&arm.body)
                        && arm.comma.is_none()
                    {
                        <Token![,]>::default().to_tokens(tokens);
                    }
                }
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprMethodCall {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_method_call(self, tokens, FixupContext::NONE);
        }
    }

    fn print_expr_method_call(e: &ExprMethodCall, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        let (left_prec, left_fixup) = fixup.leftmost_subexpression_with_dot(&e.receiver);
        print_subexpression(
            &e.receiver,
            left_prec < Precedence::Unambiguous,
            tokens,
            left_fixup,
        );
        e.dot_token.to_tokens(tokens);
        e.method.to_tokens(tokens);
        if let Some(turbofish) = &e.turbofish {
            path::printing::print_angle_bracketed_generic_arguments(
                tokens,
                turbofish,
                PathStyle::Expr,
            );
        }
        e.paren_token.surround(tokens, |tokens| {
            e.args.to_tokens(tokens);
        });
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprParen {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprPath {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            path::printing::print_qpath(tokens, &self.qself, &self.path, PathStyle::Expr);
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprRange {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_range(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_range(e: &ExprRange, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        if let Some(start) = &e.start {
            let (left_prec, left_fixup) =
                fixup.leftmost_subexpression_with_operator(start, true, false, Precedence::Range);
            print_subexpression(start, left_prec <= Precedence::Range, tokens, left_fixup);
        }
        e.limits.to_tokens(tokens);
        if let Some(end) = &e.end {
            let right_fixup = fixup.rightmost_subexpression_fixup(false, true, Precedence::Range);
            let right_prec = right_fixup.rightmost_subexpression_precedence(end);
            print_subexpression(end, right_prec <= Precedence::Range, tokens, right_fixup);
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprRawAddr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_raw_addr(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_raw_addr(e: &ExprRawAddr, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        e.and_token.to_tokens(tokens);
        e.raw.to_tokens(tokens);
        e.mutability.to_tokens(tokens);
        let (right_prec, right_fixup) = fixup.rightmost_subexpression(&e.expr, Precedence::Prefix);
        print_subexpression(
            &e.expr,
            right_prec < Precedence::Prefix,
            tokens,
            right_fixup,
        );
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprReference {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_reference(self, tokens, FixupContext::NONE);
        }
    }

    fn print_expr_reference(e: &ExprReference, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        e.and_token.to_tokens(tokens);
        e.mutability.to_tokens(tokens);
        let (right_prec, right_fixup) = fixup.rightmost_subexpression(
            &e.expr,
            #[cfg(feature = "full")]
            Precedence::Prefix,
        );
        print_subexpression(
            &e.expr,
            right_prec < Precedence::Prefix,
            tokens,
            right_fixup,
        );
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprRepeat {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.bracket_token.surround(tokens, |tokens| {
                self.expr.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
                self.len.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprReturn {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_return(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_return(e: &ExprReturn, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        e.return_token.to_tokens(tokens);
        if let Some(expr) = &e.expr {
            print_expr(
                expr,
                tokens,
                fixup.rightmost_subexpression_fixup(true, false, Precedence::Jump),
            );
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprStruct {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            path::printing::print_qpath(tokens, &self.qself, &self.path, PathStyle::Expr);
            self.brace_token.surround(tokens, |tokens| {
                self.fields.to_tokens(tokens);
                if let Some(dot2_token) = &self.dot2_token {
                    dot2_token.to_tokens(tokens);
                } else if self.rest.is_some() {
                    Token![..](Span::call_site()).to_tokens(tokens);
                }
                self.rest.to_tokens(tokens);
            });
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprTry {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_try(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_try(e: &ExprTry, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        let (left_prec, left_fixup) = fixup.leftmost_subexpression_with_dot(&e.expr);
        print_subexpression(
            &e.expr,
            left_prec < Precedence::Unambiguous,
            tokens,
            left_fixup,
        );
        e.question_token.to_tokens(tokens);
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprTryBlock {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.try_token.to_tokens(tokens);
            self.block.to_tokens(tokens);
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprTuple {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.elems.to_tokens(tokens);
                // If we only have one argument, we need a trailing comma to
                // distinguish ExprTuple from ExprParen.
                if self.elems.len() == 1 && !self.elems.trailing_punct() {
                    <Token![,]>::default().to_tokens(tokens);
                }
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprUnary {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_unary(self, tokens, FixupContext::NONE);
        }
    }

    fn print_expr_unary(e: &ExprUnary, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        e.op.to_tokens(tokens);
        let (right_prec, right_fixup) = fixup.rightmost_subexpression(
            &e.expr,
            #[cfg(feature = "full")]
            Precedence::Prefix,
        );
        print_subexpression(
            &e.expr,
            right_prec < Precedence::Prefix,
            tokens,
            right_fixup,
        );
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprUnsafe {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.unsafe_token.to_tokens(tokens);
            self.block.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.block.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprWhile {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.label.to_tokens(tokens);
            self.while_token.to_tokens(tokens);
            print_expr(&self.cond, tokens, FixupContext::new_condition());
            self.body.brace_token.surround(tokens, |tokens| {
                inner_attrs_to_tokens(&self.attrs, tokens);
                tokens.append_all(&self.body.stmts);
            });
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for ExprYield {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            print_expr_yield(self, tokens, FixupContext::NONE);
        }
    }

    #[cfg(feature = "full")]
    fn print_expr_yield(e: &ExprYield, tokens: &mut TokenStream, fixup: FixupContext) {
        outer_attrs_to_tokens(&e.attrs, tokens);
        e.yield_token.to_tokens(tokens);
        if let Some(expr) = &e.expr {
            print_expr(
                expr,
                tokens,
                fixup.rightmost_subexpression_fixup(true, false, Precedence::Jump),
            );
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for Arm {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.pat.to_tokens(tokens);
            if let Some((if_token, guard)) = &self.guard {
                if_token.to_tokens(tokens);
                guard.to_tokens(tokens);
            }
            self.fat_arrow_token.to_tokens(tokens);
            print_expr(&self.body, tokens, FixupContext::new_match_arm());
            self.comma.to_tokens(tokens);
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for FieldValue {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            outer_attrs_to_tokens(&self.attrs, tokens);
            self.member.to_tokens(tokens);
            if let Some(colon_token) = &self.colon_token {
                colon_token.to_tokens(tokens);
                self.expr.to_tokens(tokens);
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for Index {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let mut lit = Literal::i64_unsuffixed(i64::from(self.index));
            lit.set_span(self.span);
            tokens.append(lit);
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for Label {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.name.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for Member {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                Member::Named(ident) => ident.to_tokens(tokens),
                Member::Unnamed(index) => index.to_tokens(tokens),
            }
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for RangeLimits {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                RangeLimits::HalfOpen(t) => t.to_tokens(tokens),
                RangeLimits::Closed(t) => t.to_tokens(tokens),
            }
        }
    }

    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for PointerMutability {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self {
                PointerMutability::Const(const_token) => const_token.to_tokens(tokens),
                PointerMutability::Mut(mut_token) => mut_token.to_tokens(tokens),
            }
        }
    }
}
