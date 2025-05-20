use syn::Lit;

use crate::spec::SynVar;

#[derive(Clone)]
pub(crate) enum Pattern<E> {
    Exact(Box<E>),
    Pattern(SynVar),
}

macro_rules! define_node_and_kind {
    ($(($variant_name: ident, $ty: ty, $syn_type: ty)),*$(,)?) => {
        #[derive(Copy, Clone, Debug)]
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


        #[cfg(test)]
        pub(crate) fn unmangles_as_pattern(input: proc_macro2::TokenStream, kind: Kind) -> bool {
            use crate::mangle::FromPlaceholder;
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
}

pub type Ident = syn::Ident;

#[derive(Clone)]
pub(crate) struct ItemConst {
    pub _attrs: Vec<syn::Attribute>,
    pub _vis: syn::Visibility,
    pub ident: Pattern<Ident>,
    pub _generics: syn::Generics,
    pub _ty: Box<syn::Type>,
    pub expr: Pattern<Expr>,
}

#[derive(Clone)]
#[allow(dead_code)]
pub(crate) enum Expr {
    /// A slice literal expression: `[a, b, c, d]`.
    Array(syn::ExprArray),

    /// An assignment expression: `a = compute()`.
    Assign(syn::ExprAssign),

    /// An async block: `async { ... }`.
    Async(syn::ExprAsync),

    /// An await expression: `fut.await`.
    Await(syn::ExprAwait),

    /// A binary operation: `a + b`, `a += b`.
    Binary(ExprBinary),

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
    ///
    /// This variant is important for faithfully representing the precedence
    /// of expressions and is related to `None`-delimited spans in a
    /// `TokenStream`.
    Group(syn::ExprGroup),

    /// An `if` expression with an optional `else` block: `if expr { ... }
    /// else { ... }`.
    ///
    /// The `else` branch expression may only be an `If` or `Block`
    /// expression, not any of the other types of expression.
    If(syn::ExprIf),

    /// A square bracketed indexing expression: `vector[2]`.
    Index(syn::ExprIndex),

    /// The inferred value of a const generic argument, denoted `_`.
    Infer(syn::ExprInfer),

    /// A `let` guard: `let Some(x) = opt`.
    Let(syn::ExprLet),

    /// A literal in place of an expression: `1`, `"foo"`.
    Lit(ExprLit),

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
    ///
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
    ///
    /// The `rest` provides the value of the remaining fields as in `S { a:
    /// 1, b: 1, ..rest }`.
    Struct(syn::ExprStruct),

    /// A try-expression: `expr?`.
    Try(syn::ExprTry),

    /// A try block: `try { ... }`.
    TryBlock(syn::ExprTryBlock),

    /// A tuple expression: `(a, b, c, d)`.
    Tuple(syn::ExprTuple),

    /// A unary operation: `!x`, `*x`.
    Unary(ExprUnary),

    /// An unsafe block: `unsafe { ... }`.
    Unsafe(syn::ExprUnsafe),

    /// A while loop: `while expr { ... }`.
    While(syn::ExprWhile),

    /// A yield expression: `yield expr`.
    Yield(syn::ExprYield),
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

#[derive(Clone)]
pub struct ExprBinary {
    pub _attrs: Vec<syn::Attribute>,
    pub left: Pattern<Expr>,
    pub op: syn::BinOp,
    pub right: Pattern<Expr>,
}

#[derive(Clone)]
pub struct ExprUnary {
    pub _attrs: Vec<syn::Attribute>,
    pub op: syn::UnOp,
    pub expr: Pattern<Expr>,
}

#[derive(Clone)]
pub struct ExprLit {
    pub _attrs: Vec<syn::Attribute>,
    pub lit: Pattern<syn::Lit>,
}
