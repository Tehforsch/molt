use std::ops::ControlFlow;

use crate::{NodeId, Pattern};
use proc_macro2::{Delimiter, TokenStream, TokenTree};

use crate::parser::parse::ParseStream;
use crate::parser::punctuated::Punctuated;
use crate::rust_grammar::expr::Expr;
use crate::rust_grammar::generics::TypeParamBound;
use crate::rust_grammar::path::{Path, PathArguments};
use crate::rust_grammar::ty::{ReturnType, Type};

pub(crate) fn requires_semi_to_be_stmt(input: ParseStream, expr: NodeId<Expr>) -> bool {
    match input.ctx().get(expr).real() {
        Some(Expr::Macro(expr)) => !expr.mac.delimiter.is_brace(),
        _ => requires_comma_to_be_match_arm(input, expr),
    }
}

pub(crate) fn requires_comma_to_be_match_arm(input: ParseStream, expr: NodeId<Expr>) -> bool {
    match input.ctx().get(expr).real() {
        Some(e) => match e {
            Expr::If(_)
            | Expr::Match(_)
            | Expr::Block(_) | Expr::Unsafe(_) // both under ExprKind::Block in rustc
            | Expr::While(_)
            | Expr::Loop(_)
            | Expr::ForLoop(_)
            | Expr::TryBlock(_)
            | Expr::Const(_) => false,

            Expr::Array(_)
            | Expr::Assign(_)
            | Expr::Async(_)
            | Expr::Await(_)
            | Expr::Binary(_)
            | Expr::Break(_)
            | Expr::Call(_)
            | Expr::Cast(_)
            | Expr::Closure(_)
            | Expr::Continue(_)
            | Expr::Field(_)
            | Expr::Group(_)
            | Expr::Index(_)
            | Expr::Infer(_)
            | Expr::Let(_)
            | Expr::Lit(_)
            | Expr::Macro(_)
            | Expr::MethodCall(_)
            | Expr::Paren(_)
            | Expr::Path(_)
            | Expr::Range(_)
            | Expr::RawAddr(_)
            | Expr::Reference(_)
            | Expr::Repeat(_)
            | Expr::Return(_)
            | Expr::Struct(_)
            | Expr::Try(_)
            | Expr::Tuple(_)
            | Expr::Unary(_)
            | Expr::Yield(_)
            | Expr::Verbatim(_) => true,
        },
        // A variable is like an atomic expression,
        // so we require a comma.
        None => true,
    }
}

/// Whether the expression's last token is `}`.
pub(crate) fn expr_trailing_brace(input: ParseStream, mut expr: NodeId<Expr>) -> bool {
    loop {
        match input.ctx().get(expr) {
            // We count a variable as an atomic expression,
            // so it doesn't have trailing braces.
            Pattern::Pat(_) => return false,
            Pattern::Real(e) => match e {
                Expr::Async(_)
                | Expr::Block(_)
                | Expr::Const(_)
                | Expr::ForLoop(_)
                | Expr::If(_)
                | Expr::Loop(_)
                | Expr::Match(_)
                | Expr::Struct(_)
                | Expr::TryBlock(_)
                | Expr::Unsafe(_)
                | Expr::While(_) => return true,

                Expr::Binary(e) => {
                    expr = e.right;
                }
                Expr::Assign(e) => {
                    expr = e.right;
                }
                Expr::Break(e) => match e.expr {
                    Some(e) => expr = e,
                    None => return false,
                },
                Expr::Cast(e) => return type_trailing_brace(input, e.ty),
                Expr::Closure(e) => expr = e.body,
                Expr::Let(e) => expr = e.expr,
                Expr::Macro(e) => return e.mac.delimiter.is_brace(),
                Expr::Range(e) => match e.end {
                    Some(end) => expr = end,
                    None => return false,
                },
                Expr::RawAddr(e) => expr = e.expr,
                Expr::Reference(e) => expr = e.expr,
                Expr::Return(e) => match e.expr {
                    Some(e) => expr = e,
                    None => return false,
                },
                Expr::Unary(e) => expr = e.expr,
                Expr::Verbatim(e) => return tokens_trailing_brace(e),
                Expr::Yield(e) => match e.expr {
                    Some(e) => expr = e,
                    None => return false,
                },

                Expr::Array(_)
                | Expr::Await(_)
                | Expr::Call(_)
                | Expr::Continue(_)
                | Expr::Field(_)
                | Expr::Group(_)
                | Expr::Index(_)
                | Expr::Infer(_)
                | Expr::Lit(_)
                | Expr::MethodCall(_)
                | Expr::Paren(_)
                | Expr::Path(_)
                | Expr::Repeat(_)
                | Expr::Try(_)
                | Expr::Tuple(_) => return false,
            },
        }
    }

    fn type_trailing_brace(input: ParseStream, mut ty: NodeId<Type>) -> bool {
        loop {
            match input.ctx().get(ty) {
                Pattern::Pat(_) => return false,
                Pattern::Real(t) => match t {
                    Type::BareFn(t) => match &t.output {
                        ReturnType::Default => return false,
                        ReturnType::Type(_, ret) => ty = *ret,
                    },
                    Type::ImplTrait(t) => match last_type_in_bounds(&t.bounds) {
                        ControlFlow::Break(trailing_brace) => return trailing_brace,
                        ControlFlow::Continue(t) => ty = t,
                    },
                    Type::Macro(t) => return t.mac.delimiter.is_brace(),
                    Type::Path(t) => match last_type_in_path(&t.path) {
                        Some(t) => ty = t,
                        None => return false,
                    },
                    Type::Ptr(t) => ty = t.elem,
                    Type::Reference(t) => ty = t.elem,
                    Type::TraitObject(t) => match last_type_in_bounds(&t.bounds) {
                        ControlFlow::Break(trailing_brace) => return trailing_brace,
                        ControlFlow::Continue(t) => ty = t,
                    },
                    Type::Verbatim(t) => return tokens_trailing_brace(t),

                    Type::Array(_)
                    | Type::Group(_)
                    | Type::Infer(_)
                    | Type::Never(_)
                    | Type::Paren(_)
                    | Type::Slice(_)
                    | Type::Tuple(_) => return false,
                },
            }
        }
    }

    fn last_type_in_path(path: &Path) -> Option<NodeId<Type>> {
        match &path.segments.last().unwrap().arguments {
            PathArguments::None | PathArguments::AngleBracketed(_) => None,
            PathArguments::Parenthesized(arg) => match &arg.output {
                ReturnType::Default => None,
                ReturnType::Type(_, ret) => Some(*ret),
            },
        }
    }

    fn last_type_in_bounds(
        bounds: &Punctuated<TypeParamBound, Token![+]>,
    ) -> ControlFlow<bool, NodeId<Type>> {
        match bounds.last().unwrap() {
            TypeParamBound::Trait(t) => match last_type_in_path(&t.path) {
                Some(t) => ControlFlow::Continue(t),
                None => ControlFlow::Break(false),
            },
            TypeParamBound::Lifetime(_) | TypeParamBound::PreciseCapture(_) => {
                ControlFlow::Break(false)
            }
            TypeParamBound::Verbatim(t) => ControlFlow::Break(tokens_trailing_brace(t)),
        }
    }

    fn tokens_trailing_brace(tokens: &TokenStream) -> bool {
        if let Some(TokenTree::Group(last)) = tokens.clone().into_iter().last() {
            last.delimiter() == Delimiter::Brace
        } else {
            false
        }
    }
}
