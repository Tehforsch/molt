use derive_macro::CmpSyn;
use molt_lib::{NodeId, NodeList, Pattern, WithSpan};

use crate::rust_grammar::error::Result;
use crate::rust_grammar::expr::{Expr, ExprBlock, ExprPath};
use crate::rust_grammar::generics::TypeParamBound;
use crate::rust_grammar::ident::{AnyIdent, Ident};
use crate::rust_grammar::lifetime::Lifetime;
use crate::rust_grammar::lit::Lit;
use crate::rust_grammar::parse::{Parse, ParseStream};
use crate::rust_grammar::punctuated::Punctuated;
use crate::rust_grammar::token;
use crate::rust_grammar::ty::{ReturnType, Type};

#[derive(Debug, CmpSyn)]
/// A path at which a named item is exported (e.g. `std::collections::HashMap`).
pub struct Path {
    pub leading_colon: Option<Token![::]>,
    pub segments: Punctuated<PathSegment, Token![::]>,
}

impl<T> From<T> for Path
where
    T: Into<PathSegment>,
{
    fn from(segment: T) -> Self {
        let mut path = Path {
            leading_colon: None,
            segments: Punctuated::new(),
        };
        path.segments.push_value(segment.into());
        path
    }
}

#[derive(Debug, CmpSyn)]
/// A segment of a path together with any path arguments on that segment.
pub struct PathSegment {
    pub ident: NodeId<Ident>,
    pub arguments: PathArguments,
}

impl<T> From<T> for PathSegment
where
    T: Into<NodeId<Ident>>,
{
    fn from(ident: T) -> Self {
        PathSegment {
            ident: ident.into(),
            arguments: PathArguments::None,
        }
    }
}

#[derive(Debug, CmpSyn)]
/// Angle bracketed or parenthesized arguments of a path segment.
///
/// ## Angle bracketed
///
/// The `<'a, T>` in `std::slice::iter<'a, T>`.
///
/// ## Parenthesized
///
/// The `(A, B) -> C` in `Fn(A, B) -> C`.
#[derive(Default)]
pub enum PathArguments {
    #[default]
    None,
    /// The `<'a, T>` in `std::slice::iter<'a, T>`.
    AngleBracketed(AngleBracketedGenericArguments),
    /// The `(A, B) -> C` in `Fn(A, B) -> C`.
    Parenthesized(ParenthesizedGenericArguments),
}

impl PathArguments {
    pub fn is_empty(&self) -> bool {
        match self {
            PathArguments::None => true,
            PathArguments::AngleBracketed(bracketed) => bracketed.args.is_empty(),
            PathArguments::Parenthesized(_) => false,
        }
    }

    pub fn is_none(&self) -> bool {
        match self {
            PathArguments::None => true,
            PathArguments::AngleBracketed(_) | PathArguments::Parenthesized(_) => false,
        }
    }
}

#[derive(Debug, CmpSyn)]
/// An individual generic argument, like `'a`, `T`, or `Item = T`.
pub enum GenericArgument {
    /// A lifetime argument.
    Lifetime(Lifetime),
    /// A type argument.
    Type(NodeId<Type>),
    /// A const expression. Must be inside of a block.
    ///
    /// NOTE: Identity expressions are represented as Type arguments, as
    /// they are indistinguishable syntactically.
    Const(Expr),
    /// A binding (equality constraint) on an associated type: the `Item =
    /// u8` in `Iterator<Item = u8>`.
    AssocType(AssocType),
    /// An equality constraint on an associated constant: the `PANIC =
    /// false` in `Trait<PANIC = false>`.
    AssocConst(AssocConst),
    /// An associated type bound: `Iterator<Item: Display>`.
    Constraint(Constraint),
}

#[derive(Debug, CmpSyn)]
/// Angle bracketed arguments of a path segment: the `<K, V>` in `HashMap<K,
/// V>`.
pub struct AngleBracketedGenericArguments {
    pub colon2_token: Option<Token![::]>,
    pub lt_token: Token![<],
    pub args: Punctuated<GenericArgument, Token![,]>,
    pub gt_token: Token![>],
}

#[derive(Debug, CmpSyn)]
/// A binding (equality constraint) on an associated type: the `Item = u8`
/// in `Iterator<Item = u8>`.
pub struct AssocType {
    pub ident: NodeId<Ident>,
    pub generics: Option<AngleBracketedGenericArguments>,
    pub eq_token: Token![=],
    pub ty: NodeId<Type>,
}

#[derive(Debug, CmpSyn)]
/// An equality constraint on an associated constant: the `PANIC = false` in
/// `Trait<PANIC = false>`.
pub struct AssocConst {
    pub ident: NodeId<Ident>,
    pub generics: Option<AngleBracketedGenericArguments>,
    pub eq_token: Token![=],
    pub value: Expr,
}

#[derive(Debug, CmpSyn)]
/// An associated type bound: `Iterator<Item: Display>`.
pub struct Constraint {
    pub ident: NodeId<Ident>,
    pub generics: Option<AngleBracketedGenericArguments>,
    pub colon_token: Token![:],
    pub bounds: Punctuated<TypeParamBound, Token![+]>,
}

#[derive(Debug, CmpSyn)]
/// Arguments of a function path segment: the `(A, B) -> C` in `Fn(A,B) ->
/// C`.
pub struct ParenthesizedGenericArguments {
    pub paren_token: token::Paren,
    /// `(A, B)`
    pub inputs: NodeList<Type, Token![,]>,
    /// `C`
    pub output: ReturnType,
}

#[derive(Debug, CmpSyn)]
/// The explicit Self type in a qualified path: the `T` in `<T as
/// Display>::fmt`.
///
/// The actual path, including the trait and the associated item, is stored
/// separately. The `position` field represents the index of the associated
/// item qualified with this Self type.
///
/// ```text
/// <Vec<T> as a::b::Trait>::AssociatedItem
///  ^~~~~~    ~~~~~~~~~~~~~~^
///  ty        position = 3
///
/// <Vec<T>>::AssociatedItem
///  ^~~~~~   ^
///  ty       position = 0
/// ```
pub struct QSelf {
    pub lt_token: Token![<],
    pub ty: NodeId<Type>,
    pub position: usize,
    pub as_token: Option<Token![as]>,
    pub gt_token: Token![>],
}

impl Parse for Path {
    fn parse(input: ParseStream) -> Result<Self> {
        Self::parse_helper(input, false)
    }
}

impl Parse for GenericArgument {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Lifetime) && !input.peek2(Token![+]) {
            return Ok(GenericArgument::Lifetime(input.parse()?));
        }

        if input.peek_pat::<Lit>() || input.peek(token::Brace) {
            return const_argument(input).map(GenericArgument::Const);
        }

        let (span, mut argument) = input.parse_spanned_pat::<Type>()?.decompose();

        match argument {
            Pattern::Real(Type::Path(mut ty))
                if ty.qself.is_none()
                    && ty.path.leading_colon.is_none()
                    && ty.path.segments.len() == 1
                    && match &ty.path.segments[0].arguments {
                        PathArguments::None | PathArguments::AngleBracketed(_) => true,
                        PathArguments::Parenthesized(_) => false,
                    } =>
            {
                if let Some(eq_token) = input.parse::<Option<Token![=]>>()? {
                    let segment = ty.path.segments.pop().unwrap().into_value();
                    let ident = segment.ident;
                    let generics = match segment.arguments {
                        PathArguments::None => None,
                        PathArguments::AngleBracketed(arguments) => Some(arguments),
                        PathArguments::Parenthesized(_) => unreachable!(),
                    };
                    return if input.peek_pat::<Lit>() || input.peek(token::Brace) {
                        Ok(GenericArgument::AssocConst(AssocConst {
                            ident,
                            generics,
                            eq_token,
                            value: const_argument(input)?,
                        }))
                    } else {
                        Ok(GenericArgument::AssocType(AssocType {
                            ident,
                            generics,
                            eq_token,
                            ty: input.parse()?,
                        }))
                    };
                }

                if let Some(colon_token) = input.parse::<Option<Token![:]>>()? {
                    let segment = ty.path.segments.pop().unwrap().into_value();
                    return Ok(GenericArgument::Constraint(Constraint {
                        ident: segment.ident,
                        generics: match segment.arguments {
                            PathArguments::None => None,
                            PathArguments::AngleBracketed(arguments) => Some(arguments),
                            PathArguments::Parenthesized(_) => unreachable!(),
                        },
                        colon_token,
                        bounds: {
                            let mut bounds = Punctuated::new();
                            loop {
                                if input.peek(Token![,]) || input.peek(Token![>]) {
                                    break;
                                }
                                bounds.push_value({
                                    let allow_precise_capture = false;
                                    let allow_tilde_const = true;
                                    TypeParamBound::parse_single(
                                        input,
                                        allow_precise_capture,
                                        allow_tilde_const,
                                    )?
                                });
                                if !input.peek(Token![+]) {
                                    break;
                                }
                                let punct: Token![+] = input.parse()?;
                                bounds.push_punct(punct);
                            }
                            bounds
                        },
                    }));
                }

                argument = Pattern::Real(Type::Path(ty));
            }
            _ => {}
        }

        Ok(GenericArgument::Type(
            input.add_pat(argument.with_span(span)),
        ))
    }
}

pub(crate) fn const_argument(input: ParseStream) -> Result<Expr> {
    let lookahead = input.lookahead1();

    if input.peek_pat::<Lit>() {
        let lit = input.parse()?;
        return Ok(Expr::Lit(lit));
    }

    if input.peek_pat::<Ident>() {
        let ident: NodeId<Ident> = input.parse()?;
        return Ok(Expr::Path(ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: Path::from(ident),
        }));
    }

    if input.peek(token::Brace) {
        {
            let block: ExprBlock = input.parse()?;
            return Ok(Expr::Block(block));
        }
    }

    Err(lookahead.error())
}

impl AngleBracketedGenericArguments {
    /// Parse `::<…>` with mandatory leading `::`.
    ///
    /// The ordinary [`Parse`] impl for `AngleBracketedGenericArguments`
    /// parses optional leading `::`.
    pub fn parse_turbofish(input: ParseStream) -> Result<Self> {
        let colon2_token: Token![::] = input.parse()?;
        Self::do_parse(Some(colon2_token), input)
    }

    fn do_parse(colon2_token: Option<Token![::]>, input: ParseStream) -> Result<Self> {
        Ok(AngleBracketedGenericArguments {
            colon2_token,
            lt_token: input.parse()?,
            args: {
                let mut args = Punctuated::new();
                loop {
                    if input.peek(Token![>]) {
                        break;
                    }
                    let value: GenericArgument = input.parse()?;
                    args.push_value(value);
                    if input.peek(Token![>]) {
                        break;
                    }
                    let punct: Token![,] = input.parse()?;
                    args.push_punct(punct);
                }
                args
            },
            gt_token: input.parse()?,
        })
    }
}

impl Parse for AngleBracketedGenericArguments {
    fn parse(input: ParseStream) -> Result<Self> {
        let colon2_token: Option<Token![::]> = input.parse()?;
        Self::do_parse(colon2_token, input)
    }
}

impl Parse for ParenthesizedGenericArguments {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(ParenthesizedGenericArguments {
            paren_token: parenthesized!(content in input),
            inputs: content.parse_list::<ParenthesizedGenericArguments>()?,
            output: input.call(ReturnType::without_plus)?,
        })
    }
}

impl Parse for PathSegment {
    fn parse(input: ParseStream) -> Result<Self> {
        Self::parse_helper(input, false)
    }
}

impl PathSegment {
    fn parse_helper(input: ParseStream, expr_style: bool) -> Result<Self> {
        if input.peek(Token![super])
            || input.peek(Token![self])
            || input.peek(Token![crate])
            || input.peek(Token![try])
        {
            let ident = input.parse_id::<AnyIdent>()?;
            return Ok(PathSegment::from(ident));
        }

        let ident = if input.peek(Token![Self]) {
            input.parse_id::<AnyIdent>()?
        } else {
            input.parse_id::<Ident>()?
        };

        if !expr_style
            && input.peek(Token![<])
            && !input.peek(Token![<=])
            && !input.peek(Token![<<=])
            || input.peek(Token![::]) && input.peek3(Token![<])
        {
            Ok(PathSegment {
                ident,
                arguments: PathArguments::AngleBracketed(input.parse()?),
            })
        } else {
            Ok(PathSegment::from(ident))
        }
    }
}

impl Path {
    /// Parse a `Path` containing no path arguments on any of its segments.
    pub fn parse_mod_style(input: ParseStream) -> Result<Self> {
        Ok(Path {
            leading_colon: input.parse()?,
            segments: {
                let mut segments = Punctuated::new();
                loop {
                    if !input.peek_pat::<Ident>()
                        && !input.peek(Token![super])
                        && !input.peek(Token![self])
                        && !input.peek(Token![Self])
                        && !input.peek(Token![crate])
                    {
                        break;
                    }
                    let ident = input.parse_id::<AnyIdent>()?;
                    segments.push_value(PathSegment::from(ident));
                    if !input.peek(Token![::]) {
                        break;
                    }
                    let punct = input.parse()?;
                    segments.push_punct(punct);
                }
                if segments.is_empty() {
                    return Err(input.parse_id::<Ident>().unwrap_err());
                } else if segments.trailing_punct() {
                    return Err(input.error("expected path segment after `::`"));
                }
                segments
            },
        })
    }

    fn parse_helper(input: ParseStream, expr_style: bool) -> Result<Self> {
        let mut path = Path {
            leading_colon: input.parse()?,
            segments: {
                let mut segments = Punctuated::new();
                let value = PathSegment::parse_helper(input, expr_style)?;
                segments.push_value(value);
                segments
            },
        };
        Path::parse_rest(input, &mut path, expr_style)?;
        Ok(path)
    }

    pub(crate) fn parse_rest(input: ParseStream, path: &mut Self, expr_style: bool) -> Result<()> {
        while input.peek(Token![::]) && !input.peek3(token::Paren) {
            let punct: Token![::] = input.parse()?;
            path.segments.push_punct(punct);
            let value = PathSegment::parse_helper(input, expr_style)?;
            path.segments.push_value(value);
        }
        Ok(())
    }

    pub(crate) fn is_mod_style(&self) -> bool {
        self.segments
            .iter()
            .all(|segment| segment.arguments.is_none())
    }
}

pub(crate) fn qpath(input: ParseStream, expr_style: bool) -> Result<(Option<QSelf>, Path)> {
    if input.peek(Token![<]) {
        let lt_token: Token![<] = input.parse()?;
        let this = input.parse()?;
        let path = if input.peek(Token![as]) {
            let as_token: Token![as] = input.parse()?;
            let path: Path = input.parse()?;
            Some((as_token, path))
        } else {
            None
        };
        let gt_token: Token![>] = input.parse()?;
        let colon2_token: Token![::] = input.parse()?;
        let mut rest = Punctuated::new();
        loop {
            let path = PathSegment::parse_helper(input, expr_style)?;
            rest.push_value(path);
            if !input.peek(Token![::]) {
                break;
            }
            let punct: Token![::] = input.parse()?;
            rest.push_punct(punct);
        }
        let (position, as_token, path) = match path {
            Some((as_token, mut path)) => {
                let pos = path.segments.len();
                path.segments.push_punct(colon2_token);
                path.segments.extend(rest.into_pairs());
                (pos, Some(as_token), path)
            }
            None => {
                let path = Path {
                    leading_colon: Some(colon2_token),
                    segments: rest,
                };
                (0, None, path)
            }
        };
        let qself = QSelf {
            lt_token,
            ty: this,
            position,
            as_token,
            gt_token,
        };
        Ok((Some(qself), path))
    } else {
        let path = Path::parse_helper(input, expr_style)?;
        Ok((None, path))
    }
}
