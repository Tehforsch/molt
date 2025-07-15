use std::hash::{Hash, Hasher};
use std::mem;

use derive_macro::CmpSyn;
use molt_lib::{Id, NodeId, NodeList, Pattern, Spanned, SpannedPat, WithSpan};
use proc_macro2::{Span, TokenStream};

use crate::attr::Attribute;
use crate::error::{Error, Result};
use crate::generics::BoundLifetimes;
use crate::ident::{AnyIdent, Ident};
use crate::item::{Asyncness, Constness};
use crate::lifetime::Lifetime;
use crate::lit::{Lit, LitFloat, LitInt};
use crate::mac::{
    Macro, {self},
};
use crate::op::{BinOp, UnOp};
use crate::parse::discouraged::Speculative as _;
use crate::parse::{
    ListOrItem, Parse, ParseBuffer, ParseList, ParseListOrItem, ParseNode, ParseStream,
    parse_punctuated_list_real,
};
use crate::pat::{Pat, PatMultiLeadingVert, PatType};
use crate::path::{
    AngleBracketedGenericArguments, Path, QSelf, {self},
};
use crate::precedence::Precedence;
use crate::punctuated::Punctuated;
use crate::stmt::Block;
use crate::ty::{NoPlus, ReturnType, Type};
use crate::{attr, classify, token, verbatim};

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
struct ExprNoEagerBrace;

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
}

#[derive(Debug, CmpSyn)]
/// A slice literal expression: `[a, b, c, d]`.
pub struct ExprArray {
    pub attrs: Vec<Attribute>,
    pub bracket_token: token::Bracket,
    pub elems: NodeList<Expr, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// An assignment expression: `a = compute()`.
pub struct ExprAssign {
    pub attrs: Vec<Attribute>,
    pub left: NodeId<Expr>,
    pub eq_token: Token![=],
    pub right: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// An async block: `async { ... }`.
pub struct ExprAsync {
    pub attrs: Vec<Attribute>,
    pub async_token: Token![async],
    pub capture: Option<Token![move]>,
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// An await expression: `fut.await`.
pub struct ExprAwait {
    pub attrs: Vec<Attribute>,
    pub base: NodeId<Expr>,
    pub dot_token: Token![.],
    pub await_token: Token![await],
}

#[derive(Debug, CmpSyn)]
/// A binary operation: `a + b`, `a += b`.
pub struct ExprBinary {
    pub attrs: Vec<Attribute>,
    pub left: NodeId<Expr>,
    pub op: BinOp,
    pub right: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A blocked scope: `{ ... }`.
pub struct ExprBlock {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// A `break`, with an optional label to break and an optional
/// expression.
pub struct ExprBreak {
    pub attrs: Vec<Attribute>,
    pub break_token: Token![break],
    pub label: Option<Lifetime>,
    pub expr: Option<NodeId<Expr>>,
}

#[derive(Debug, CmpSyn)]
/// A function call expression: `invoke(a, b)`.
pub struct ExprCall {
    pub attrs: Vec<Attribute>,
    pub func: NodeId<Expr>,
    pub paren_token: token::Paren,
    pub args: NodeList<Expr, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A cast expression: `foo as f64`.
pub struct ExprCast {
    pub attrs: Vec<Attribute>,
    pub expr: NodeId<Expr>,
    pub as_token: Token![as],
    pub ty: NodeId<Type>,
}

#[derive(Debug, CmpSyn)]
/// A closure expression: `|a, b| a + b`.
pub struct ExprClosure {
    pub attrs: Vec<Attribute>,
    pub lifetimes: Option<BoundLifetimes>,
    #[rule(Const, Closure)]
    pub constness: Constness,
    pub movability: Option<Token![static]>,
    #[rule(Async, Closure)]
    pub asyncness: Asyncness,
    pub capture: Option<Token![move]>,
    pub or1_token: Token![|],
    pub inputs: NodeList<Pat, Token![,]>,
    pub or2_token: Token![|],
    pub output: ReturnType,
    pub body: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A const block: `const { ... }`.
pub struct ExprConst {
    pub attrs: Vec<Attribute>,
    pub const_token: Token![const],
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// A `continue`, with an optional label.
pub struct ExprContinue {
    pub attrs: Vec<Attribute>,
    pub continue_token: Token![continue],
    pub label: Option<Lifetime>,
}

#[derive(Debug, CmpSyn)]
/// Access of a named struct field (`obj.k`) or unnamed tuple struct
/// field (`obj.0`).
pub struct ExprField {
    pub attrs: Vec<Attribute>,
    pub base: NodeId<Expr>,
    pub dot_token: Token![.],
    pub member: Member,
}

#[derive(Debug, CmpSyn)]
/// A for loop: `for pat in expr { ... }`.
pub struct ExprForLoop {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub for_token: Token![for],
    pub pat: NodeId<Pat>,
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
pub struct ExprIf {
    pub attrs: Vec<Attribute>,
    pub if_token: Token![if],
    pub cond: NodeId<Expr>,
    pub then_branch: Block,
    pub else_branch: Option<(Token![else], NodeId<Expr>)>,
}

#[derive(Debug, CmpSyn)]
/// A square bracketed indexing expression: `vector[2]`.
pub struct ExprIndex {
    pub attrs: Vec<Attribute>,
    pub expr: NodeId<Expr>,
    pub bracket_token: token::Bracket,
    pub index: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// The inferred value of a const generic argument, denoted `_`.
pub struct ExprInfer {
    pub attrs: Vec<Attribute>,
    pub underscore_token: Token![_],
}

#[derive(Debug, CmpSyn)]
/// A `let` guard: `let Some(x) = opt`.
pub struct ExprLet {
    pub attrs: Vec<Attribute>,
    pub let_token: Token![let],
    pub pat: NodeId<Pat>,
    pub eq_token: Token![=],
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A literal in place of an expression: `1`, `"foo"`.
pub struct ExprLit {
    pub attrs: Vec<Attribute>,
    pub lit: NodeId<Lit>,
}

#[derive(Debug, CmpSyn)]
/// Conditionless loop: `loop { ... }`.
pub struct ExprLoop {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub loop_token: Token![loop],
    pub body: Block,
}

#[derive(Debug, CmpSyn)]
/// A macro invocation expression: `format!("{}", q)`.
pub struct ExprMacro {
    pub attrs: Vec<Attribute>,
    pub mac: Macro,
}

#[derive(Debug, CmpSyn)]
/// A `match` expression: `match n { Some(n) => {}, None => {} }`.
pub struct ExprMatch {
    pub attrs: Vec<Attribute>,
    pub match_token: Token![match],
    pub expr: NodeId<Expr>,
    pub brace_token: token::Brace,
    pub arms: NodeList<Arm, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A method call expression: `x.foo::<T>(a, b)`.
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
pub struct ExprPath {
    pub attrs: Vec<Attribute>,
    pub qself: Option<QSelf>,
    pub path: Path,
}

#[derive(Debug, CmpSyn)]
/// A range expression: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
pub struct ExprRange {
    pub attrs: Vec<Attribute>,
    pub start: Option<NodeId<Expr>>,
    pub limits: RangeLimits,
    pub end: Option<NodeId<Expr>>,
}

#[derive(Debug, CmpSyn)]
/// Address-of operation: `&raw const place` or `&raw mut place`.
pub struct ExprRawAddr {
    pub attrs: Vec<Attribute>,
    pub and_token: Token![&],
    pub raw: Token![raw],
    pub mutability: PointerMutability,
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A referencing operation: `&a` or `&mut a`.
pub struct ExprReference {
    pub attrs: Vec<Attribute>,
    pub and_token: Token![&],
    pub mutability: Option<Token![mut]>,
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// An array literal constructed from one repeated element: `[0u8; N]`.
pub struct ExprRepeat {
    pub attrs: Vec<Attribute>,
    pub bracket_token: token::Bracket,
    pub expr: NodeId<Expr>,
    pub semi_token: Token![;],
    pub len: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// A `return`, with an optional value to be returned.
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
pub struct ExprTry {
    pub attrs: Vec<Attribute>,
    pub expr: NodeId<Expr>,
    pub question_token: Token![?],
}

#[derive(Debug, CmpSyn)]
/// A try block: `try { ... }`.
pub struct ExprTryBlock {
    pub attrs: Vec<Attribute>,
    pub try_token: Token![try],
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// A tuple expression: `(a, b, c, d)`.
pub struct ExprTuple {
    pub attrs: Vec<Attribute>,
    pub paren_token: token::Paren,
    pub elems: NodeList<Expr, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A unary operation: `!x`, `*x`.
pub struct ExprUnary {
    pub attrs: Vec<Attribute>,
    pub op: UnOp,
    pub expr: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// An unsafe block: `unsafe { ... }`.
pub struct ExprUnsafe {
    pub attrs: Vec<Attribute>,
    pub unsafe_token: Token![unsafe],
    pub block: Block,
}

#[derive(Debug, CmpSyn)]
/// A while loop: `while expr { ... }`.
pub struct ExprWhile {
    pub attrs: Vec<Attribute>,
    pub label: Option<Label>,
    pub while_token: Token![while],
    pub cond: NodeId<Expr>,
    pub body: Block,
}

#[derive(Debug, CmpSyn)]
/// A yield expression: `yield expr`.
pub struct ExprYield {
    pub attrs: Vec<Attribute>,
    pub yield_token: Token![yield],
    pub expr: Option<NodeId<Expr>>,
}

impl Expr {
    const PLACEHOLDER: Self = Expr::Path(ExprPath {
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
    fn peek(input: ParseStream) -> bool {
        input.peek_var::<Expr> ()
            || input.peek_pat::<AnyIdent>() && !input.peek(Token![as]) // value name or keyword
            || input.peek(token::Paren) // tuple
            || input.peek(token::Bracket) // array
            || input.peek(token::Brace) // block
            || input.peek_pat::<Lit>() // literal
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

#[derive(Debug, CmpSyn)]
/// A field-value pair in a struct literal.
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
pub struct Arm {
    pub attrs: Vec<Attribute>,
    pub pat: NodeId<Pat>,
    pub guard: Option<(Token![if], NodeId<Expr>)>,
    pub fat_arrow_token: Token![=>],
    pub body: NodeId<Expr>,
}

#[derive(Debug, CmpSyn)]
/// Limit types of a range, inclusive or exclusive.
pub enum RangeLimits {
    /// Inclusive at the beginning, exclusive at the end.
    HalfOpen(Token![..]),
    /// Inclusive at the beginning and end.
    Closed(Token![..=]),
}

#[derive(Debug, CmpSyn)]
/// Mutability of a raw pointer (`*const T`, `*mut T`), in which non-mutable
/// isn't the implicit default.
pub enum PointerMutability {
    Const(Token![const]),
    Mut(Token![mut]),
}

// When we're parsing expressions which occur before blocks, like in an if
// statement's condition, we cannot parse a struct literal.
//
// Struct literals are ambiguous in certain positions
// https://github.com/rust-lang/rfcs/pull/92
pub(super) struct AllowStruct(bool);

// By default, the `parse_pat` function on `ParseStream` will
// automatically check if the input contains a variable and parse
// it appropriately. However, for expressions, this is not the
// right behavior.  This is because while parsing an expression
// like `$e + 5`, we cannot eagerly consume the variable, but
// need to go down to the lowest level first. We only check for
// variables at the level of atomic expressions (see
// `atom_expr`).
// This is why the following trait impls do not impl parse_spanned but
// overwrites parse_pat instead.

impl ParseNode for Expr {
    type Target = Expr;

    fn parse_node(_: ParseStream) -> Result<Self::Target> {
        unreachable!()
    }

    fn parse_pat(input: ParseStream) -> Result<Pattern<Self::Target, Id>> {
        Ok(ambiguous_expr(input, AllowStruct(true))?.item())
    }
}

impl ParseNode for ExprNoEagerBrace {
    type Target = Expr;

    fn parse_node(_: ParseStream) -> Result<Self::Target> {
        unreachable!()
    }

    fn parse_pat(input: ParseStream) -> Result<Pattern<Self::Target, Id>> {
        Ok(ambiguous_expr(input, AllowStruct(false))?.item())
    }
}

impl ParseNode for ExprEarlierBoundaryRule {
    type Target = Expr;

    fn parse_node(_: ParseStream) -> Result<Self::Target> {
        unreachable!()
    }

    fn parse_pat(input: ParseStream) -> Result<Pattern<Self::Target, Id>> {
        Ok(parse_with_earlier_boundary_rule(input)?.item())
    }
}

struct FnArgs;

impl ParseList for FnArgs {
    type Item = Expr;
    type ParseItem = Expr;
    type Punct = Token![,];

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Self::Item>>> {
        parse_punctuated_list_real::<Expr, Self::Punct>(input)
    }
}

pub(super) fn parse_with_earlier_boundary_rule(input: ParseStream) -> Result<SpannedPat<Expr>> {
    let mut attrs = input.call(expr_attrs)?;
    let mut expr: SpannedPat<Expr> = if input.peek(token::Group) {
        let allow_struct = AllowStruct(true);
        let atom = input
            .call_spanned(|input| expr_group(input, allow_struct))?
            .map(Pattern::Real);
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
        input.call_spanned(atom_labeled)?.map(Pattern::Real)
    } else {
        let allow_struct = AllowStruct(true);
        unary_expr(input, allow_struct)?
    };

    if continue_parsing_early(input, &expr) {
        if let Pattern::Real(expr) = &mut *expr {
            attrs.extend(expr.replace_attrs(Vec::new()));
            expr.replace_attrs(attrs);
        }

        let allow_struct = AllowStruct(true);
        return parse_expr(input, expr, allow_struct, Precedence::MIN);
    }

    if input.peek(Token![.]) && !input.peek(Token![..]) || input.peek(Token![?]) {
        expr = trailer_helper(input, expr)?;

        if let Pattern::Real(expr) = &mut *expr {
            attrs.extend(expr.replace_attrs(Vec::new()));
            expr.replace_attrs(attrs);
        }

        let allow_struct = AllowStruct(true);
        return parse_expr(input, expr, allow_struct, Precedence::MIN);
    }

    if let Pattern::Real(expr) = &mut *expr {
        attrs.extend(expr.replace_attrs(Vec::new()));
        expr.replace_attrs(attrs);
    }
    Ok(expr)
}

impl Copy for AllowStruct {}

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
    allow_struct: AllowStruct,
    precedence: Precedence,
) -> Result<SpannedPat<Expr>> {
    let mut rhs = unary_expr(input, allow_struct)?;
    loop {
        let next = peek_precedence(input);
        if next > precedence || next == precedence && precedence == Precedence::Assign {
            let cursor = input.cursor();
            rhs = parse_expr(input, rhs, allow_struct, next)?;
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
    let lhs = unary_expr(input, allow_struct)?;
    parse_expr(input, lhs, allow_struct, Precedence::MIN)
}

fn expr_attrs(input: ParseStream) -> Result<Vec<Attribute>> {
    let mut attrs = Vec::new();
    while !input.peek(token::Group) && input.peek(Token![#]) {
        attrs.push(input.call(attr::single_parse_outer)?);
    }
    Ok(attrs)
}

// <UnOp> <trailer>
// & <trailer>
// &mut <trailer>
// box <trailer>

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
                .make_spanned(
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
                .make_spanned(
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

// <atom> (..<args>) ...
// <atom> . <ident> (..<args>) ...
// <atom> . <ident> ...
// <atom> . <lit> ...
// <atom> [ <expr> ] ...
// <atom> ? ...

fn trailer_expr(
    begin: ParseBuffer,
    mut attrs: Vec<Attribute>,
    input: ParseStream,
    allow_struct: AllowStruct,
) -> Result<SpannedPat<Expr>> {
    let atom = input.call_spanned(|input| atom_expr(input, allow_struct))?;
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
                args: content.parse_list::<FnArgs>()?,
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
                        args: content.parse_list::<FnArgs>()?,
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
fn atom_expr(input: ParseStream, allow_struct: AllowStruct) -> Result<Pattern<Expr, Id>> {
    if let Some(var) = input.parse_var() {
        return var;
    }
    atom_expr_inner(input, allow_struct).map(Pattern::Real)
}

fn atom_expr_inner(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
    let real: Expr = if input.peek(token::Group) {
        expr_group(input, allow_struct)?
    } else if input.peek_pat::<Lit>() {
        input.parse::<ExprLit>().map(Expr::Lit)?
    } else if input.peek(Token![async])
        && (input.peek2(token::Brace) || input.peek2(Token![move]) && input.peek3(token::Brace))
    {
        input.parse::<ExprAsync>().map(Expr::Async)?
    } else if input.peek(Token![try]) && input.peek2(token::Brace) {
        input.parse::<ExprTryBlock>().map(Expr::TryBlock)?
    } else if input.peek(Token![|])
        || input.peek(Token![move])
        || input.peek(Token![for])
            && input.peek2(Token![<])
            && (input.peek3(Lifetime) || input.peek3(Token![>]))
        || input.peek(Token![const]) && !input.peek2(token::Brace)
        || input.peek(Token![static])
        || input.peek(Token![async]) && (input.peek2(Token![|]) || input.peek2(Token![move]))
    {
        expr_closure(input, allow_struct).map(Expr::Closure)?
    } else if token::peek_keyword(input.cursor(), "builtin") && input.peek2(Token![#]) {
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
        path_or_macro_or_struct(input, allow_struct)?
    } else if input.peek(token::Paren) {
        paren_or_tuple(input)?
    } else if input.peek(Token![break]) {
        expr_break(input, allow_struct).map(Expr::Break)?
    } else if input.peek(Token![continue]) {
        input.parse::<ExprContinue>().map(Expr::Continue)?
    } else if input.peek(Token![return]) {
        input.parse::<ExprReturn>().map(Expr::Return)?
    } else if input.peek(Token![become]) {
        expr_become(input)?
    } else if input.peek(token::Bracket) {
        array_or_repeat(input)?
    } else if input.peek(Token![let]) {
        expr_let(input, allow_struct).map(Expr::Let)?
    } else if input.peek(Token![if]) {
        input.parse::<ExprIf>().map(Expr::If)?
    } else if input.peek(Token![while]) {
        input.parse::<ExprWhile>().map(Expr::While)?
    } else if input.peek(Token![for]) {
        input.parse::<ExprForLoop>().map(Expr::ForLoop)?
    } else if input.peek(Token![loop]) {
        input.parse::<ExprLoop>().map(Expr::Loop)?
    } else if input.peek(Token![match]) {
        input.parse::<ExprMatch>().map(Expr::Match)?
    } else if input.peek(Token![yield]) {
        input.parse::<ExprYield>().map(Expr::Yield)?
    } else if input.peek(Token![unsafe]) {
        input.parse::<ExprUnsafe>().map(Expr::Unsafe)?
    } else if input.peek(Token![const]) {
        input.parse::<ExprConst>().map(Expr::Const)?
    } else if input.peek(token::Brace) {
        input.parse::<ExprBlock>().map(Expr::Block)?
    } else if input.peek(Token![..]) {
        expr_range(input, allow_struct).map(Expr::Range)?
    } else if input.peek(Token![_]) {
        input.parse::<ExprInfer>().map(Expr::Infer)?
    } else if input.peek(Lifetime) {
        atom_labeled(input)?
    } else {
        Err(input.error("expected an expression"))?
    };
    Ok(real)
}

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

fn path_or_macro_or_struct(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
    let expr_style = true;
    let (qself, path) = path::qpath(input, expr_style)?;
    rest_of_path_or_macro_or_struct(qself, path, input, allow_struct)
}

fn rest_of_path_or_macro_or_struct(
    qself: Option<QSelf>,
    path: Path,
    input: ParseStream,
    allow_struct: AllowStruct,
) -> Result<Expr> {
    if qself.is_none() && input.peek(Token![!]) && !input.peek(Token![!=]) && path.is_mod_style() {
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

    if allow_struct.0 && input.peek(token::Brace) {
        return expr_struct_helper(input, qself, path).map(Expr::Struct);
    }

    Ok(Expr::Path(ExprPath {
        attrs: Vec::new(),
        qself,
        path,
    }))
}

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

        let first = input.parse_spanned_pat::<Expr>()?;
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
        let first = input.parse_spanned_pat::<Expr>()?;
        // TODO make this prettier. This might be a pattern that
        // I just havent understood yet. If the input is empty,
        // and we continue the function without this early return,
        // we'll think that the result is a list, even if all we find
        // is a single var.
        if first.is_var() {
            return Ok(ListOrItem::Item(first));
        }
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

impl Parse for ExprLit {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ExprLit {
            attrs: Vec::new(),
            lit: input.parse_id::<Lit>()?,
        })
    }
}

fn expr_group(input: ParseStream, allow_struct: AllowStruct) -> Result<Expr> {
    let group = crate::group::parse_group(input)?;
    let inner: SpannedPat<Expr> = group.content.parse_spanned_pat::<Expr>()?;
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

impl Parse for ExprLet {
    fn parse(input: ParseStream) -> Result<Self> {
        let allow_struct = AllowStruct(true);
        expr_let(input, allow_struct)
    }
}

fn expr_let(input: ParseStream, allow_struct: AllowStruct) -> Result<ExprLet> {
    use crate::pat::PatMultiLeadingVert;

    Ok(ExprLet {
        attrs: Vec::new(),
        let_token: input.parse()?,
        pat: input.parse_id::<PatMultiLeadingVert>()?,
        eq_token: input.parse()?,
        expr: {
            let lhs = unary_expr(input, allow_struct)?;
            input.add_pat(parse_expr(input, lhs, allow_struct, Precedence::Compare)?)
        },
    })
}

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
                input.add(input.make_spanned(marker, Expr::If(expr)));
            expr = prev;
        }
        expr.attrs = attrs;
        Ok(expr)
    }
}

impl Parse for ExprInfer {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ExprInfer {
            attrs: input.call(Attribute::parse_outer)?,
            underscore_token: input.parse()?,
        })
    }
}

impl Parse for ExprForLoop {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let label: Option<Label> = input.parse()?;
        let for_token: Token![for] = input.parse()?;

        let pat = input.parse_id::<PatMultiLeadingVert>()?;

        let in_token: Token![in] = input.parse()?;
        let expr = input.parse_id::<ExprNoEagerBrace>()?;

        let content;
        let brace_token = braced!(content in input);
        attr::parse_inner(&content, &mut attrs)?;
        let stmts = content.parse_list::<Block>()?;

        Ok(ExprForLoop {
            attrs,
            label,
            for_token,
            pat,
            in_token,
            expr,
            body: Block { brace_token, stmts },
        })
    }
}

impl Parse for ExprLoop {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let label: Option<Label> = input.parse()?;
        let loop_token: Token![loop] = input.parse()?;

        let content;
        let brace_token = braced!(content in input);
        attr::parse_inner(&content, &mut attrs)?;
        let stmts = content.parse_list::<Block>()?;

        Ok(ExprLoop {
            attrs,
            label,
            loop_token,
            body: Block { brace_token, stmts },
        })
    }
}

impl Parse for ExprMatch {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let match_token: Token![match] = input.parse()?;
        let expr = input.parse_id::<ExprNoEagerBrace>()?;

        let content;
        let brace_token = braced!(content in input);
        attr::parse_inner(&content, &mut attrs)?;

        let arms = content.parse_list::<Arms>()?;

        Ok(ExprMatch {
            attrs,
            match_token,
            expr,
            brace_token,
            arms,
        })
    }
}

fn expr_unary(
    input: ParseStream,
    attrs: Vec<Attribute>,
    allow_struct: AllowStruct,
) -> Result<SpannedPat<ExprUnary>> {
    let marker = input.marker();
    Ok(input
        .make_spanned(
            marker,
            ExprUnary {
                attrs,
                op: input.parse()?,
                expr: input.add_pat(unary_expr(input, allow_struct)?),
            },
        )
        .as_pattern())
}

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

impl Parse for ExprBreak {
    fn parse(input: ParseStream) -> Result<Self> {
        let allow_struct = AllowStruct(true);
        expr_break(input, allow_struct)
    }
}

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

fn expr_become(input: ParseStream) -> Result<Expr> {
    let begin = input.fork();
    input.parse::<Token![become]>()?;
    input.parse::<NodeId<Expr>>()?;
    Ok(Expr::Verbatim(verbatim::between(&begin, input)))
}

impl Parse for ExprTryBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ExprTryBlock {
            attrs: Vec::new(),
            try_token: input.parse()?,
            block: input.parse()?,
        })
    }
}

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

struct ClosureInput;

impl ParseNode for ClosureInput {
    type Target = Pat;

    fn parse_node(_: ParseStream) -> Result<Self::Target> {
        unreachable!()
    }

    fn parse_pat(input: ParseStream) -> Result<Pattern<Self::Target, Id>> {
        use crate::pat::PatSingle;

        let attrs = input.call(Attribute::parse_outer)?;
        let mut pat = input.parse_spanned_pat::<PatSingle>()?;

        if input.peek(Token![:]) {
            Ok(Pattern::Real(Pat::Type(PatType {
                attrs,
                pat: input.add_pat(pat),
                colon_token: input.parse()?,
                ty: input.parse()?,
            })))
        } else {
            match &mut *pat {
                Pattern::Pat(_) => {}
                Pattern::Real(Pat::Const(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Ident(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Lit(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Macro(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Or(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Paren(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Path(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Range(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Reference(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Rest(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Slice(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Struct(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Tuple(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::TupleStruct(pat)) => pat.attrs = attrs,
                Pattern::Real(Pat::Type(_)) => unreachable!(),
                Pattern::Real(Pat::Verbatim(_)) => {}
                Pattern::Real(Pat::Wild(pat)) => pat.attrs = attrs,
            }
            Ok(pat.item())
        }
    }
}

impl ParseList for ClosureInput {
    type Punct = Token![,];
    type ParseItem = ClosureInput;
    type Item = Pat;

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<<Self as ParseNode>::Target>>> {
        let mut inputs = Punctuated::new();
        loop {
            if input.peek(Token![|]) {
                break;
            }
            let value = input.parse_id::<ClosureInput>()?;
            inputs.push_value(value);
            if input.peek(Token![|]) {
                break;
            }
            let punct: Token![,] = input.parse()?;
            inputs.push_punct(punct);
        }
        Ok(inputs.into_iter().collect())
    }
}

fn expr_closure(input: ParseStream, allow_struct: AllowStruct) -> Result<ExprClosure> {
    let lifetimes: Option<BoundLifetimes> = input.parse()?;
    let constness: Constness = input.parse()?;
    let movability: Option<Token![static]> = input.parse()?;
    let asyncness: Asyncness = input.parse()?;
    let capture: Option<Token![move]> = input.parse()?;
    let or1_token: Token![|] = input.parse()?;

    let inputs = input.parse_list::<ClosureInput>()?;

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

impl Parse for ExprWhile {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let label: Option<Label> = input.parse()?;
        let while_token: Token![while] = input.parse()?;
        let cond = input.parse_id::<ExprNoEagerBrace>()?;

        let content;
        let brace_token = braced!(content in input);
        attr::parse_inner(&content, &mut attrs)?;
        let stmts = content.parse_list::<Block>()?;

        Ok(ExprWhile {
            attrs,
            label,
            while_token,
            cond,
            body: Block { brace_token, stmts },
        })
    }
}

impl Parse for ExprConst {
    fn parse(input: ParseStream) -> Result<Self> {
        let const_token: Token![const] = input.parse()?;

        let content;
        let brace_token = braced!(content in input);
        let inner_attrs = content.call(Attribute::parse_inner)?;
        let stmts = content.parse_list::<Block>()?;

        Ok(ExprConst {
            attrs: inner_attrs,
            const_token,
            block: Block { brace_token, stmts },
        })
    }
}

impl Parse for Label {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Label {
            name: input.parse()?,
            colon_token: input.parse()?,
        })
    }
}

impl Parse for Option<Label> {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Lifetime) {
            input.parse().map(Some)
        } else {
            Ok(None)
        }
    }
}

impl Parse for ExprContinue {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ExprContinue {
            attrs: Vec::new(),
            continue_token: input.parse()?,
            label: input.parse()?,
        })
    }
}

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
            member: member.item(),
            colon_token,
            expr: value,
        })
    }
}

impl Parse for ExprStruct {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr_style = true;
        let (qself, path) = path::qpath(input, expr_style)?;
        expr_struct_helper(input, qself, path)
    }
}

fn expr_struct_helper(input: ParseStream, qself: Option<QSelf>, path: Path) -> Result<ExprStruct> {
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

impl Parse for ExprUnsafe {
    fn parse(input: ParseStream) -> Result<Self> {
        let unsafe_token: Token![unsafe] = input.parse()?;

        let content;
        let brace_token = braced!(content in input);
        let inner_attrs = content.call(Attribute::parse_inner)?;
        let stmts = content.parse_list::<Block>()?;

        Ok(ExprUnsafe {
            attrs: inner_attrs,
            unsafe_token,
            block: Block { brace_token, stmts },
        })
    }
}

impl Parse for ExprBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let label: Option<Label> = input.parse()?;

        let content;
        let brace_token = braced!(content in input);
        attr::parse_inner(&content, &mut attrs)?;
        let stmts = content.parse_list::<Block>()?;

        Ok(ExprBlock {
            attrs,
            label,
            block: Block { brace_token, stmts },
        })
    }
}

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

impl Parse for ExprPath {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;

        let expr_style = true;
        let (qself, path) = path::qpath(input, expr_style)?;

        Ok(ExprPath { attrs, qself, path })
    }
}

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

struct Arms;

impl ParseList for Arms {
    type Punct = Token![,];
    type ParseItem = Arm;
    type Item = Arm;

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Arm>>> {
        let mut arms = Vec::new();
        while !input.is_empty() {
            arms.push(input.parse_id::<Arm>()?);
        }
        Ok(arms)
    }
}

impl ParseNode for Arm {
    type Target = Arm;

    fn parse_node(input: ParseStream) -> Result<Arm> {
        let requires_comma;
        let res = Ok(Arm {
            attrs: input.call(Attribute::parse_outer)?,
            pat: input.parse_id::<PatMultiLeadingVert>()?,
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
        });
        let _: Option<Token![,]> = {
            if requires_comma && !input.is_empty() {
                Some(input.parse()?)
            } else {
                input.parse()?
            }
        };
        res
    }
}

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
    let msg = format!("casts cannot be followed by {kind}");
    Err(input.error(msg))
}
