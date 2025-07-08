use derive_macro::CmpSyn;
use molt_lib::{NodeId, NodeList, Pattern, WithSpan};
use proc_macro2::TokenStream;

use crate::attr::Attribute;
use crate::error::Result;
use crate::expr::{Expr, ExprConst, ExprLit, ExprMacro, ExprPath, ExprRange, Member, RangeLimits};
pub use crate::expr::{
    ExprConst as PatConst, ExprLit as PatLit, ExprMacro as PatMacro, ExprPath as PatPath,
    ExprRange as PatRange,
};
use crate::ident::{AnyIdent, Ident};
use crate::lit::Lit;
use crate::mac::{self, Macro};
use crate::parse::{
    ListOrItem, Parse, ParseBuffer, ParseList, ParseListOrItem, ParsePat, ParseStream, PosMarker,
};
use crate::path::{self, Path, QSelf};
use crate::punctuated::Punctuated;
use crate::ty::Type;
use crate::{Stmt, token, verbatim};

#[derive(Debug, CmpSyn)]
/// A pattern in a local binding, function signature, match expression, or
/// various other places.
pub enum Pat {
    /// A const block: `const { ... }`.
    Const(PatConst),

    /// A pattern that binds a new variable: `ref mut binding @ SUBPATTERN`.
    Ident(PatIdent),

    /// A literal pattern: `0`.
    Lit(PatLit),

    /// A macro in pattern position.
    Macro(PatMacro),

    /// A pattern that matches any one of a set of cases.
    Or(PatOr),

    /// A parenthesized pattern: `(A | B)`.
    Paren(PatParen),

    /// A path pattern like `Color::Red`, optionally qualified with a
    /// self-type.
    ///
    /// Unqualified path patterns can legally refer to variants, structs,
    /// constants or associated constants. Qualified path patterns like
    /// `<A>::B::C` and `<A as Trait>::B::C` can only legally refer to
    /// associated constants.
    Path(PatPath),

    /// A range pattern: `1..=2`.
    Range(PatRange),

    /// A reference pattern: `&mut var`.
    Reference(PatReference),

    /// The dots in a tuple or slice pattern: `[0, 1, ..]`.
    Rest(PatRest),

    /// A dynamically sized slice pattern: `[a, b, ref i @ .., y, z]`.
    Slice(PatSlice),

    /// A struct or struct variant pattern: `Variant { x, y, .. }`.
    Struct(PatStruct),

    /// A tuple pattern: `(a, b)`.
    Tuple(PatTuple),

    /// A tuple struct or tuple variant pattern: `Variant(x, y, .., z)`.
    TupleStruct(PatTupleStruct),

    /// A type ascription pattern: `foo: f64`.
    Type(PatType),

    /// Tokens in pattern position not interpreted by Syn.
    Verbatim(TokenStream),

    /// A pattern that matches any value: `_`.
    Wild(PatWild),
}

/// Parse a pattern that does _not_ involve `|` at the top level.
///
/// This parser matches the behavior of the `$:pat_param` macro_rules
/// matcher, and on editions prior to Rust 2021, the behavior of
/// `$:pat`.
///
/// In Rust syntax, some examples of where this syntax would occur are
/// in the argument pattern of functions and closures. Patterns using
/// `|` are not allowed to occur in these positions.
///
/// ```compile_fail
/// fn f(Some(_) | None: Option<T>) {
///     let _ = |Some(_) | None: Option<T>| {};
///     //       ^^^^^^^^^^^^^^^^^^^^^^^^^??? :(
/// }
/// ```
///
/// ```console
/// error: top-level or-patterns are not allowed in function parameters
///  --> src/main.rs:1:6
///   |
/// 1 | fn f(Some(_) | None: Option<T>) {
///   |      ^^^^^^^^^^^^^^ help: wrap the pattern in parentheses: `(Some(_) | None)`
/// ```
pub struct PatSingle;

/// Parse a pattern, possibly involving `|`, but not a leading `|`.
pub struct PatMulti;

/// Parse a pattern, possibly involving `|`, possibly including a
/// leading `|`.
///
/// This parser matches the behavior of the Rust 2021 edition's `$:pat`
/// macro_rules matcher.
///
/// In Rust syntax, an example of where this syntax would occur is in
/// the pattern of a `match` arm, where the language permits an optional
/// leading `|`, although it is not idiomatic to write one there in
/// handwritten code.
///
/// ```
/// # let wat = None;
/// match wat {
///     | None | Some(false) => {}
///     | Some(true) => {}
/// }
/// ```
///
/// The compiler accepts it only to facilitate some situations in
/// macro-generated code where a macro author might need to write:
///
/// ```
/// # macro_rules! doc {
/// #     ($value:expr, ($($conditions1:pat),*), ($($conditions2:pat),*), $then:expr) => {
/// match $value {
///     $(| $conditions1)* $(| $conditions2)* => $then
/// }
/// #     };
/// # }
/// #
/// # doc!(true, (true), (false), {});
/// # doc!(true, (), (true, false), {});
/// # doc!(true, (true, false), (), {});
/// ```
///
/// Expressing the same thing correctly in the case that either one (but
/// not both) of `$conditions1` and `$conditions2` might be empty,
/// without leading `|`, is complex.
///
/// Use [`Pat::parse_multi`] instead if you are not intending to support
/// macro-generated macro input.
pub struct PatMultiLeadingVert;

#[derive(Debug, CmpSyn)]
/// A pattern that binds a new variable: `ref mut binding @ SUBPATTERN`.
///
/// It may also be a unit struct or struct variant (e.g. `None`), or a
/// constant; these cannot be distinguished syntactically.
pub struct PatIdent {
    pub attrs: Vec<Attribute>,
    pub by_ref: Option<Token![ref]>,
    pub mutability: Option<Token![mut]>,
    pub ident: NodeId<Ident>,
    pub subpat: Option<(Token![@], NodeId<Pat>)>,
}

#[derive(Debug, CmpSyn)]
/// A pattern that matches any one of a set of cases.
pub struct PatOr {
    pub attrs: Vec<Attribute>,
    pub leading_vert: Option<Token![|]>,
    pub cases: NodeList<Pat, Token![|]>,
}

#[derive(Debug, CmpSyn)]
/// A parenthesized pattern: `(A | B)`.
pub struct PatParen {
    pub attrs: Vec<Attribute>,
    pub paren_token: token::Paren,
    pub pat: NodeId<Pat>,
}

#[derive(Debug, CmpSyn)]
/// A reference pattern: `&mut var`.
pub struct PatReference {
    pub attrs: Vec<Attribute>,
    pub and_token: Token![&],
    pub mutability: Option<Token![mut]>,
    pub pat: NodeId<Pat>,
}

#[derive(Debug, CmpSyn)]
/// The dots in a tuple or slice pattern: `[0, 1, ..]`.
pub struct PatRest {
    pub attrs: Vec<Attribute>,
    pub dot2_token: Token![..],
}

#[derive(Debug, CmpSyn)]
/// A dynamically sized slice pattern: `[a, b, ref i @ .., y, z]`.
pub struct PatSlice {
    pub attrs: Vec<Attribute>,
    pub bracket_token: token::Bracket,
    pub elems: Punctuated<Pat, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A struct or struct variant pattern: `Variant { x, y, .. }`.
pub struct PatStruct {
    pub attrs: Vec<Attribute>,
    pub qself: Option<QSelf>,
    pub path: Path,
    pub brace_token: token::Brace,
    pub fields: Punctuated<FieldPat, Token![,]>,
    pub rest: Option<PatRest>,
}

#[derive(Debug, CmpSyn)]
/// A tuple pattern: `(a, b)`.
pub struct PatTuple {
    pub attrs: Vec<Attribute>,
    pub paren_token: token::Paren,
    pub elems: Punctuated<Pat, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A tuple struct or tuple variant pattern: `Variant(x, y, .., z)`.
pub struct PatTupleStruct {
    pub attrs: Vec<Attribute>,
    pub qself: Option<QSelf>,
    pub path: Path,
    pub paren_token: token::Paren,
    pub elems: NodeList<Pat, Token![,]>,
}

#[derive(Debug, CmpSyn)]
/// A type ascription pattern: `foo: f64`.
pub struct PatType {
    pub attrs: Vec<Attribute>,
    pub pat: NodeId<Pat>,
    pub colon_token: Token![:],
    pub ty: NodeId<Type>,
}

#[derive(Debug, CmpSyn)]
/// A pattern that matches any value: `_`.
pub struct PatWild {
    pub attrs: Vec<Attribute>,
    pub underscore_token: Token![_],
}

#[derive(Debug, CmpSyn)]
/// A single field in a struct pattern.
///
/// Patterns like the fields of Foo `{ x, ref y, ref mut z }` are treated
/// the same as `x: x, y: ref y, z: ref mut z` but there is no colon token.
pub struct FieldPat {
    pub attrs: Vec<Attribute>,
    pub member: Member,
    pub colon_token: Option<Token![:]>,
    pub pat: NodeId<Pat>,
}

impl ParsePat for PatSingle {
    type Target = Pat;

    fn parse_pat(input: ParseStream) -> Result<molt_lib::SpannedPat<Self::Target>> {
        let marker = input.marker();
        let begin = input.fork();
        let lookahead = input.lookahead1();
        let pat = if lookahead.peek_pat::<Ident>()
            && (input.peek2(Token![::])
                || input.peek2(Token![!])
                || input.peek2(token::Brace)
                || input.peek2(token::Paren)
                || input.peek2(Token![..]))
            || input.peek(Token![self]) && input.peek2(Token![::])
            || lookahead.peek(Token![::])
            || lookahead.peek(Token![<])
            || input.peek(Token![Self])
            || input.peek(Token![super])
            || input.peek(Token![crate])
        {
            pat_path_or_macro_or_struct_or_range(input)
        } else if lookahead.peek(Token![_]) {
            input.call(pat_wild).map(Pat::Wild)
        } else if input.peek(Token![box]) {
            pat_box(begin, input)
        } else if input.peek(Token![-]) || lookahead.peek(Lit) || lookahead.peek(Token![const]) {
            pat_lit_or_range(input)
        } else if lookahead.peek(Token![ref])
            || lookahead.peek(Token![mut])
            || input.peek(Token![self])
            || input.peek_pat::<Ident>()
        {
            input.call(pat_ident).map(Pat::Ident)
        } else if lookahead.peek(Token![&]) {
            input.call(pat_reference).map(Pat::Reference)
        } else if lookahead.peek(token::Paren) {
            input.call(pat_paren_or_tuple)
        } else if lookahead.peek(token::Bracket) {
            input.call(pat_slice).map(Pat::Slice)
        } else if lookahead.peek(Token![..]) && !input.peek(Token![...]) {
            pat_range_half_open(input)
        } else if lookahead.peek(Token![const]) {
            input.call(pat_const).map(Pat::Verbatim)
        } else {
            Err(lookahead.error())
        };
        Ok(pat?.pattern_with_span(input.span_from_marker(marker)))
    }
}

fn parse_pat_multi<T: ParseListOrItem<Target = Pat, Punct = token::Or>>(
    input: ParseStream,
) -> Result<molt_lib::SpannedPat<Pat>> {
    let marker = input.marker();
    let pat = input.parse_list_or_item::<T>()?;
    Ok(match pat {
        ListOrItem::Item(item) => item,
        ListOrItem::List(cases) => Pat::Or(PatOr {
            attrs: Vec::new(),
            leading_vert: None,
            cases,
        })
        .pattern_with_span(input.span_from_marker(marker)),
    })
}

impl ParsePat for PatMulti {
    type Target = Pat;

    fn parse_pat(input: ParseStream) -> Result<molt_lib::SpannedPat<Self::Target>> {
        parse_pat_multi::<Self>(input)
    }
}

impl ParsePat for PatMultiLeadingVert {
    type Target = Pat;

    fn parse_pat(input: ParseStream) -> Result<molt_lib::SpannedPat<Self::Target>> {
        parse_pat_multi::<Self>(input)
    }
}

impl Parse for PatType {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(PatType {
            attrs: Vec::new(),
            pat: input.parse_id::<PatSingle>()?,
            colon_token: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl ParseListOrItem for PatMulti {
    type Punct = Token![|];

    type Target = Pat;

    fn parse_list_or_item(input: ParseStream) -> Result<ListOrItem<Self::Target, Self::Punct>> {
        let leading_vert: Option<Token![|]> = input.parse()?;
        multi_pat_impl(input, leading_vert)
    }
}

impl ParseListOrItem for PatMultiLeadingVert {
    type Punct = Token![|];

    type Target = Pat;

    fn parse_list_or_item(input: ParseStream) -> Result<ListOrItem<Self::Target, Self::Punct>> {
        multi_pat_impl(input, None)
    }
}

fn multi_pat_impl(
    input: ParseStream,
    leading_vert: Option<Token![|]>,
) -> Result<ListOrItem<Pat, Token![|]>> {
    let pat = PatSingle::parse_pat(input)?;
    if leading_vert.is_some()
        || input.peek(Token![|]) && !input.peek(Token![||]) && !input.peek(Token![|=])
    {
        let mut cases = Punctuated::new();
        cases.push_value(input.add_pat(pat));
        while input.peek(Token![|]) && !input.peek(Token![||]) && !input.peek(Token![|=]) {
            let punct: Token![|] = input.parse()?;
            cases.push_punct(punct);
            let pat = input.parse_id::<PatSingle>()?;
            cases.push_value(pat);
        }
        Ok(ListOrItem::List(NodeList::Real(
            cases.into_iter().collect(),
        )))
    } else {
        Ok(ListOrItem::Item(pat))
    }
}

fn pat_path_or_macro_or_struct_or_range(input: ParseStream) -> Result<Pat> {
    let marker = input.marker();
    let expr_style = true;
    let (qself, path) = path::qpath(input, expr_style)?;

    if qself.is_none() && input.peek(Token![!]) && !input.peek(Token![!=]) && path.is_mod_style() {
        let bang_token: Token![!] = input.parse()?;
        let (delimiter, tokens) = mac::parse_delimiter(input)?;
        return Ok(Pat::Macro(ExprMacro {
            attrs: Vec::new(),
            mac: Macro {
                path,
                bang_token,
                delimiter,
                tokens,
            },
        }));
    }

    if input.peek(token::Brace) {
        pat_struct(input, qself, path).map(Pat::Struct)
    } else if input.peek(token::Paren) {
        pat_tuple_struct(input, qself, path).map(Pat::TupleStruct)
    } else if input.peek(Token![..]) {
        pat_range(input, marker, qself, path)
    } else {
        Ok(Pat::Path(ExprPath {
            attrs: Vec::new(),
            qself,
            path,
        }))
    }
}

fn pat_wild(input: ParseStream) -> Result<PatWild> {
    Ok(PatWild {
        attrs: Vec::new(),
        underscore_token: input.parse()?,
    })
}

fn pat_box(begin: ParseBuffer, input: ParseStream) -> Result<Pat> {
    input.parse::<Token![box]>()?;
    input.parse_id::<PatSingle>()?;
    Ok(Pat::Verbatim(verbatim::between(&begin, input)))
}

fn pat_ident(input: ParseStream) -> Result<PatIdent> {
    Ok(PatIdent {
        attrs: Vec::new(),
        by_ref: input.parse()?,
        mutability: input.parse()?,
        ident: {
            if input.peek(Token![self]) {
                input.parse_id::<AnyIdent>()?
            } else {
                input.parse()?
            }
        },
        subpat: {
            if input.peek(Token![@]) {
                let at_token: Token![@] = input.parse()?;
                let subpat = input.parse_id::<PatSingle>()?;
                Some((at_token, subpat))
            } else {
                None
            }
        },
    })
}

impl ParseList for PatMultiLeadingVert {
    type Item = Pat;
    type Punct = Token![,];

    fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Pat>>> {
        let mut elems = Punctuated::new();
        while !input.is_empty() {
            let value = input.parse_id::<PatMultiLeadingVert>()?;
            elems.push_value(value);
            if input.is_empty() {
                break;
            }
            let punct: Token![,] = input.parse()?;
            elems.push_punct(punct);
        }
        Ok(elems.into_iter().collect())
    }
}

fn pat_tuple_struct(
    input: ParseStream,
    qself: Option<QSelf>,
    path: Path,
) -> Result<PatTupleStruct> {
    let content;
    let paren_token = parenthesized!(content in input);

    let elems = content.parse_list::<PatMultiLeadingVert>()?;

    Ok(PatTupleStruct {
        attrs: Vec::new(),
        qself,
        path,
        paren_token,
        elems,
    })
}

fn pat_struct(input: ParseStream, qself: Option<QSelf>, path: Path) -> Result<PatStruct> {
    let content;
    let brace_token = braced!(content in input);

    let mut fields = Punctuated::new();
    let mut rest = None;
    while !content.is_empty() {
        let attrs = content.call(Attribute::parse_outer)?;
        if content.peek(Token![..]) {
            rest = Some(PatRest {
                attrs,
                dot2_token: content.parse()?,
            });
            break;
        }
        let mut value = content.call(field_pat)?;
        value.attrs = attrs;
        fields.push_value(value);
        if content.is_empty() {
            break;
        }
        let punct: Token![,] = content.parse()?;
        fields.push_punct(punct);
    }

    Ok(PatStruct {
        attrs: Vec::new(),
        qself,
        path,
        brace_token,
        fields,
        rest,
    })
}

fn field_pat(input: ParseStream) -> Result<FieldPat> {
    let marker = input.marker();
    let begin = input.fork();
    let boxed: Option<Token![box]> = input.parse()?;
    let by_ref: Option<Token![ref]> = input.parse()?;
    let mutability: Option<Token![mut]> = input.parse()?;

    let member = if boxed.is_some() || by_ref.is_some() || mutability.is_some() {
        input.parse().map(Member::Named)
    } else {
        input.parse()
    }?;

    if boxed.is_none() && by_ref.is_none() && mutability.is_none() && input.peek(Token![:])
        || !member.is_named()
    {
        return Ok(FieldPat {
            attrs: Vec::new(),
            member,
            colon_token: Some(input.parse()?),
            pat: input.parse_id::<PatMultiLeadingVert>()?,
        });
    }

    let ident = match member {
        Member::Named(ident) => ident,
        Member::Unnamed(_) => unreachable!(),
    };

    let pat = if boxed.is_some() {
        Pat::Verbatim(verbatim::between(&begin, input))
    } else {
        Pat::Ident(PatIdent {
            attrs: Vec::new(),
            by_ref,
            mutability,
            ident,
            subpat: None,
        })
    }
    .with_span(input.span_from_marker(marker));
    let pat = input.add(pat);

    Ok(FieldPat {
        attrs: Vec::new(),
        member: Member::Named(ident),
        colon_token: None,
        pat,
    })
}

fn pat_range(
    input: ParseStream,
    marker: PosMarker,
    qself: Option<QSelf>,
    path: Path,
) -> Result<Pat> {
    let start_span = input.span_from_marker(marker);
    let limits = RangeLimits::parse_obsolete(input)?;
    let end = input.call_spanned(pat_range_bound)?;
    if let (RangeLimits::Closed(_), Pattern::Real(None)) = (&limits, &*end) {
        return Err(input.error("expected range upper bound"));
    }
    let end = end
        .map_real(|end| end.map(PatRangeBound::into_expr))
        .transpose();
    Ok(Pat::Range(ExprRange {
        attrs: Vec::new(),
        start: Some(
            input.add(
                Expr::Path(ExprPath {
                    attrs: Vec::new(),
                    qself,
                    path,
                })
                .with_span(start_span),
            ),
        ),
        limits,
        end: end.map(|end| input.add_pat(end)),
    }))
}

fn pat_range_half_open(input: ParseStream) -> Result<Pat> {
    let limits: RangeLimits = input.parse()?;
    let end = input.call_spanned(pat_range_bound)?.transpose();
    if end.is_some() {
        Ok(Pat::Range(ExprRange {
            attrs: Vec::new(),
            start: None,
            limits,
            end: end.map(|end| input.add_pat(end.map_real(PatRangeBound::into_expr))),
        }))
    } else {
        match limits {
            RangeLimits::HalfOpen(dot2_token) => Ok(Pat::Rest(PatRest {
                attrs: Vec::new(),
                dot2_token,
            })),
            RangeLimits::Closed(_) => Err(input.error("expected range upper bound")),
        }
    }
}

fn pat_paren_or_tuple(_: ParseStream) -> Result<Pat> {
    todo!()
    // let content;
    // let paren_token = parenthesized!(content in input);

    // let mut elems = Punctuated::new();
    // while !content.is_empty() {
    //     let value = PatMultiLeadingVert::parse_pat(&content)?;
    //     if content.is_empty() {
    //         if elems.is_empty() && !matches!(&*value, Pattern::Real(Pat::Rest(_))) {
    //             return Ok(Pat::Paren(PatParen {
    //                 attrs: Vec::new(),
    //                 paren_token,
    //                 pat: input.add_pat(value),
    //             }));
    //         }
    //         elems.push_value(value);
    //         break;
    //     }
    //     elems.push_value(value);
    //     let punct = content.parse()?;
    //     elems.push_punct(punct);
    // }

    // Ok(Pat::Tuple(PatTuple {
    //     attrs: Vec::new(),
    //     paren_token,
    //     elems,
    // }))
}

fn pat_reference(input: ParseStream) -> Result<PatReference> {
    Ok(PatReference {
        attrs: Vec::new(),
        and_token: input.parse()?,
        mutability: input.parse()?,
        pat: input.parse_id::<PatSingle>()?,
    })
}

fn pat_lit_or_range(input: ParseStream) -> Result<Pat> {
    let start = input.call_spanned(pat_range_bound)?.transpose().unwrap();
    if input.peek(Token![..]) {
        let limits = RangeLimits::parse_obsolete(input)?;
        let end = input.call_spanned(pat_range_bound)?.transpose();
        if let (RangeLimits::Closed(_), None) = (&limits, &end) {
            return Err(input.error("expected range upper bound"));
        }
        Ok(Pat::Range(ExprRange {
            attrs: Vec::new(),
            start: Some(input.add_pat(start.map_real(|start| start.into_expr()))),
            limits,
            end: end.map(|end| input.add_pat(end.map_real(|end| end.into_expr()))),
        }))
    } else {
        Ok(start.take().unwrap_real().into_pat())
    }
}

// Patterns that can appear on either side of a range pattern.
enum PatRangeBound {
    Const(ExprConst),
    Lit(ExprLit),
    Path(ExprPath),
}

impl PatRangeBound {
    fn into_expr(self) -> Expr {
        match self {
            PatRangeBound::Const(pat) => Expr::Const(pat),
            PatRangeBound::Lit(pat) => Expr::Lit(pat),
            PatRangeBound::Path(pat) => Expr::Path(pat),
        }
    }

    fn into_pat(self) -> Pat {
        match self {
            PatRangeBound::Const(pat) => Pat::Const(pat),
            PatRangeBound::Lit(pat) => Pat::Lit(pat),
            PatRangeBound::Path(pat) => Pat::Path(pat),
        }
    }
}

fn pat_range_bound(input: ParseStream) -> Result<Option<PatRangeBound>> {
    if input.is_empty()
        || input.peek(Token![|])
        || input.peek(Token![=])
        || input.peek(Token![:]) && !input.peek(Token![::])
        || input.peek(Token![,])
        || input.peek(Token![;])
        || input.peek(Token![if])
    {
        return Ok(None);
    }

    let lookahead = input.lookahead1();
    let expr = if lookahead.peek(Lit) {
        PatRangeBound::Lit(input.parse()?)
    } else if lookahead.peek_pat::<Ident>()
        || lookahead.peek(Token![::])
        || lookahead.peek(Token![<])
        || lookahead.peek(Token![self])
        || lookahead.peek(Token![Self])
        || lookahead.peek(Token![super])
        || lookahead.peek(Token![crate])
    {
        PatRangeBound::Path(input.parse()?)
    } else if lookahead.peek(Token![const]) {
        PatRangeBound::Const(input.parse()?)
    } else {
        return Err(lookahead.error());
    };

    Ok(Some(expr))
}

fn pat_slice(_: ParseStream) -> Result<PatSlice> {
    todo!()
    // let content;
    // let bracket_token = bracketed!(content in input);

    // let mut elems = Punctuated::new();
    // while !content.is_empty() {
    //     let value = PatMultiLeadingVert::parse_pat(&content)?;
    //     match &*value {
    //         Pattern::Real(Pat::Range(pat)) if pat.start.is_none() || pat.end.is_none() => {
    //             let (start, end) = match pat.limits {
    //                 RangeLimits::HalfOpen(dot_dot) => (dot_dot.spans[0], dot_dot.spans[1]),
    //                 RangeLimits::Closed(dot_dot_eq) => {
    //                     (dot_dot_eq.spans[0], dot_dot_eq.spans[2])
    //                 }
    //             };
    //             let msg = "range pattern is not allowed unparenthesized inside slice pattern";
    //             return Err(error::new2(start, end, msg));
    //         }
    //         _ => {}
    //     }
    //     elems.push_value(value);
    //     if content.is_empty() {
    //         break;
    //     }
    //     let punct = content.parse()?;
    //     elems.push_punct(punct);
    // }

    // Ok(PatSlice {
    //     attrs: Vec::new(),
    //     bracket_token,
    //     elems,
    // })
}

fn pat_const(input: ParseStream) -> Result<TokenStream> {
    let begin = input.fork();
    input.parse::<Token![const]>()?;

    let content;
    braced!(content in input);
    content.call(Attribute::parse_inner)?;
    content.parse_list::<Stmt>()?;

    Ok(verbatim::between(&begin, input))
}
