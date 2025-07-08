use derive_macro::CmpSyn;
use molt_lib::{NodeId, NodeList};

use crate::attr::Attribute;
use crate::expr::Expr;
use crate::item::Item;
use crate::mac::Macro;
use crate::pat::Pat;
use crate::token;

#[derive(Debug, CmpSyn)]
/// A braced block containing Rust statements.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct Block {
    pub brace_token: token::Brace,
    /// Statements in a block
    pub stmts: NodeList<Stmt, Token![;]>,
}

pub struct StmtAllowNoSemi;

#[derive(Debug, CmpSyn)]
/// A statement, usually ending in a semicolon.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub enum Stmt {
    /// A local (let) binding.
    Local(Local),

    /// An item definition.
    Item(Item),

    /// Expression, with or without trailing semicolon.
    Expr(NodeId<Expr>, Option<Token![;]>),

    /// A macro invocation in statement position.
    ///
    /// Syntactically it's ambiguous which other kind of statement this
    /// macro would expand to. It can be any of local variable (`let`),
    /// item, or expression.
    Macro(StmtMacro),
}

#[derive(Debug, CmpSyn)]
/// A local `let` binding: `let x: u64 = s.parse()?;`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct Local {
    pub attrs: Vec<Attribute>,
    pub let_token: Token![let],
    pub pat: NodeId<Pat>,
    pub init: Option<LocalInit>,
    pub semi_token: Token![;],
}

#[derive(Debug, CmpSyn)]
/// The expression assigned in a local `let` binding, including optional
/// diverging `else` block.
///
/// `LocalInit` represents `= s.parse()?` in `let x: u64 = s.parse()?` and
/// `= r else { return }` in `let Ok(x) = r else { return }`.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct LocalInit {
    pub eq_token: Token![=],
    pub expr: NodeId<Expr>,
    pub diverge: Option<(Token![else], NodeId<Expr>)>,
}

#[derive(Debug, CmpSyn)]
/// A macro invocation in statement position.
///
/// Syntactically it's ambiguous which other kind of statement this macro
/// would expand to. It can be any of local variable (`let`), item, or
/// expression.
#[cfg_attr(docsrs, doc(cfg(feature = "full")))]
pub struct StmtMacro {
    pub attrs: Vec<Attribute>,
    pub mac: Macro,
    pub semi_token: Option<Token![;]>,
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    use crate::attr::Attribute;
    use crate::classify;
    use crate::error::Result;
    use crate::expr::{Expr, ExprBlock, ExprEarlierBoundaryRule, ExprMacro};
    use crate::ident::Ident;
    use crate::item;
    use crate::mac::{self, Macro};
    use crate::parse::discouraged::Speculative as _;
    use crate::parse::{Parse, ParseList, ParsePat, ParseStream};
    use crate::pat::{Pat, PatSingle, PatType};
    use crate::path::Path;
    use crate::stmt::{Block, Local, LocalInit, Stmt, StmtMacro};
    use crate::token;
    use molt_lib::{NodeId, Pattern, Spanned, SpannedPat, WithSpan};
    use proc_macro2::TokenStream;

    use super::StmtAllowNoSemi;

    struct AllowNoSemi(bool);

    impl ParseList for Stmt {
        type Item = Stmt;
        type Punct = Token![;];

        fn parse_list_real(input: ParseStream) -> Result<Vec<NodeId<Stmt>>> {
            let mut stmts = Vec::new();
            loop {
                let (span, semi) = input.parse_spanned::<Option<Token![;]>>()?.decompose();
                if let Some(semi) = semi {
                    stmts.push(
                        input.add(
                            Stmt::Expr(
                                input.add(Expr::Verbatim(TokenStream::new()).with_span(span)),
                                Some(semi),
                            )
                            .with_span(span),
                        ),
                    );
                    continue;
                }
                if input.is_empty() {
                    break;
                }
                let stmt = input.parse_pat::<StmtAllowNoSemi>()?;
                let requires_semicolon = match stmt.real() {
                    Some(Stmt::Expr(stmt, None)) => {
                        classify::requires_semi_to_be_stmt(input, *stmt)
                    }
                    Some(Stmt::Macro(stmt)) => {
                        stmt.semi_token.is_none() && !stmt.mac.delimiter.is_brace()
                    }
                    Some(Stmt::Local(_)) | Some(Stmt::Item(_)) | Some(Stmt::Expr(_, Some(_))) => {
                        false
                    }
                    // A pattern variable statement has an implicit semicolon
                    None => false,
                };
                stmts.push(input.add_pat(stmt));
                if input.is_empty() {
                    break;
                } else if requires_semicolon {
                    return Err(input.error("unexpected token, expected `;`"));
                }
            }
            Ok(stmts)
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Block {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(Block {
                brace_token: braced!(content in input),
                stmts: content.parse_list::<Stmt>()?,
            })
        }
    }

    impl ParsePat for Stmt {
        type Target = Stmt;

        fn parse_pat(input: ParseStream) -> Result<molt_lib::SpannedPat<Self::Target>> {
            parse_stmt(input, AllowNoSemi(false))
        }
    }

    impl ParsePat for StmtAllowNoSemi {
        type Target = Stmt;

        fn parse_pat(input: ParseStream) -> Result<molt_lib::SpannedPat<Self::Target>> {
            parse_stmt(input, AllowNoSemi(true))
        }
    }

    fn parse_stmt(input: ParseStream, allow_nosemi: AllowNoSemi) -> Result<SpannedPat<Stmt>> {
        let marker = input.marker();
        let begin = input.fork();
        let attrs = input.call(Attribute::parse_outer)?;

        // brace-style macros; paren and bracket macros get parsed as
        // expression statements.
        let ahead = input.fork();
        let mut is_item_macro = false;
        if let Ok(path) = ahead.call(Path::parse_mod_style) {
            if ahead.peek(Token![!]) {
                if ahead.peek2(Ident) || ahead.peek2(Token![try]) {
                    is_item_macro = true;
                } else if ahead.peek2(token::Brace)
                    && !(ahead.peek3(Token![.]) && !ahead.peek3(Token![..])
                        || ahead.peek3(Token![?]))
                {
                    input.advance_to(&ahead);
                    return stmt_mac(input, attrs, path);
                }
            }
        }

        if input.peek(Token![let]) && !input.peek(token::Group) {
            stmt_local(input, attrs)
        } else if input.peek(Token![pub])
            || input.peek(Token![crate]) && !input.peek2(Token![::])
            || input.peek(Token![extern])
            || input.peek(Token![use])
            || input.peek(Token![static])
                && (input.peek2(Token![mut])
                    || input.peek2(Ident)
                        && !(input.peek2(Token![async])
                            && (input.peek3(Token![move]) || input.peek3(Token![|]))))
            || input.peek(Token![const])
                && !(input.peek2(token::Brace)
                    || input.peek2(Token![static])
                    || input.peek2(Token![async])
                        && !(input.peek3(Token![unsafe])
                            || input.peek3(Token![extern])
                            || input.peek3(Token![fn]))
                    || input.peek2(Token![move])
                    || input.peek2(Token![|]))
            || input.peek(Token![unsafe]) && !input.peek2(token::Brace)
            || input.peek(Token![async])
                && (input.peek2(Token![unsafe])
                    || input.peek2(Token![extern])
                    || input.peek2(Token![fn]))
            || input.peek(Token![fn])
            || input.peek(Token![mod])
            || input.peek(Token![type])
            || input.peek(Token![struct])
            || input.peek(Token![enum])
            || input.peek(Token![union]) && input.peek2(Ident)
            || input.peek(Token![auto]) && input.peek2(Token![trait])
            || input.peek(Token![trait])
            || input.peek(Token![default])
                && (input.peek2(Token![unsafe]) || input.peek2(Token![impl]))
            || input.peek(Token![impl])
            || input.peek(Token![macro])
            || is_item_macro
        {
            let item = item::parsing::parse_rest_of_item(begin, attrs, input)?;
            Ok(Stmt::Item(item).pattern_with_span(input.span_from_marker(marker)))
        } else {
            stmt_expr(input, allow_nosemi, attrs)
        }
    }

    fn stmt_mac(input: ParseStream, attrs: Vec<Attribute>, path: Path) -> Result<SpannedPat<Stmt>> {
        let marker = input.marker();
        let bang_token: Token![!] = input.parse()?;
        let (delimiter, tokens) = mac::parse_delimiter(input)?;
        let semi_token: Option<Token![;]> = input.parse()?;

        Ok(Stmt::Macro(StmtMacro {
            attrs,
            mac: Macro {
                path,
                bang_token,
                delimiter,
                tokens,
            },
            semi_token,
        })
        .pattern_with_span(input.span_from_marker(marker)))
    }

    fn stmt_local(input: ParseStream, attrs: Vec<Attribute>) -> Result<SpannedPat<Stmt>> {
        let marker = input.marker();
        let let_token: Token![let] = input.parse()?;

        let mut pat = input.parse_id::<PatSingle>()?;
        if input.peek(Token![:]) {
            let colon_token: Token![:] = input.parse()?;
            let ty = input.parse()?;
            pat = input.add(
                Pat::Type(PatType {
                    attrs: Vec::new(),
                    pat,
                    colon_token,
                    ty,
                })
                .with_span(input.span_from_marker(marker)),
            );
        }

        let init = if let Some(eq_token) = input.parse()? {
            let eq_token: Token![=] = eq_token;
            let expr: NodeId<Expr> = input.parse()?;

            let diverge = {
                if !classify::expr_trailing_brace(input, expr) && input.peek(Token![else]) {
                    let else_token: Token![else] = input.parse()?;
                    let block: Spanned<Block> = input.parse_spanned()?;
                    let diverge = block.map(|block| {
                        Expr::Block(ExprBlock {
                            attrs: Vec::new(),
                            label: None,
                            block,
                        })
                    });
                    Some((else_token, input.add(diverge)))
                } else {
                    None
                }
            };

            Some(LocalInit {
                eq_token,
                expr,
                diverge,
            })
        } else {
            None
        };

        let semi_token: Token![;] = input.parse()?;

        Ok(Stmt::Local(Local {
            attrs,
            let_token,
            pat,
            init,
            semi_token,
        })
        .pattern_with_span(input.span_from_marker(marker)))
    }

    fn stmt_expr(
        input: ParseStream,
        allow_nosemi: AllowNoSemi,
        mut attrs: Vec<Attribute>,
    ) -> Result<SpannedPat<Stmt>> {
        let marker = input.marker();
        let e = input.parse_pat::<ExprEarlierBoundaryRule>()?;

        let mut attr_target = None;
        loop {
            let ctx = input.ctx();
            let e = match attr_target {
                Some(id) => ctx.get(id),
                None => e.as_ref(),
            };
            let Pattern::Real(e) = e else {
                break;
            };
            attr_target = match e {
                Expr::Assign(e) => Some(e.left),
                Expr::Binary(e) => Some(e.left),
                Expr::Cast(e) => Some(e.expr),
                Expr::Array(_)
                | Expr::Async(_)
                | Expr::Await(_)
                | Expr::Block(_)
                | Expr::Break(_)
                | Expr::Call(_)
                | Expr::Closure(_)
                | Expr::Const(_)
                | Expr::Continue(_)
                | Expr::Field(_)
                | Expr::ForLoop(_)
                | Expr::Group(_)
                | Expr::If(_)
                | Expr::Index(_)
                | Expr::Infer(_)
                | Expr::Let(_)
                | Expr::Lit(_)
                | Expr::Loop(_)
                | Expr::Macro(_)
                | Expr::Match(_)
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
                | Expr::TryBlock(_)
                | Expr::Tuple(_)
                | Expr::Unary(_)
                | Expr::Unsafe(_)
                | Expr::While(_)
                | Expr::Yield(_)
                | Expr::Verbatim(_) => break,
            };
        }
        let mut e = e;
        {
            let mut ctx = input.ctx_mut();
            let attr_target = match attr_target {
                Some(id) => ctx.get_mut(id),
                None => e.as_mut(),
            };
            match attr_target {
                Pattern::Real(attr_target) => {
                    attrs.extend(attr_target.replace_attrs(Vec::new()));
                    attr_target.replace_attrs(attrs);
                }
                Pattern::Pat(_) => {
                    if !attrs.is_empty() {
                        panic!("Attr on var")
                    }
                }
            }
        }

        let (expr_span, e) = e.decompose();
        let semi_token: Option<Token![;]> = input.parse()?;
        let stmt_span = input.span_from_marker(marker);

        match e {
            Pattern::Real(Expr::Macro(ExprMacro { attrs, mac }))
                if semi_token.is_some() || mac.delimiter.is_brace() =>
            {
                Ok(Stmt::Macro(StmtMacro {
                    attrs,
                    mac,
                    semi_token,
                })
                .pattern_with_span(stmt_span))
            }
            _ => {
                let e = input.add_pat(e.with_span(expr_span));
                if semi_token.is_some() {
                    Ok(Stmt::Expr(e, semi_token).pattern_with_span(stmt_span))
                } else if allow_nosemi.0 || !classify::requires_semi_to_be_stmt(input, e) {
                    Ok(Stmt::Expr(e, None).pattern_with_span(stmt_span))
                } else {
                    Err(input.error("expected semicolon"))
                }
            }
        }
    }
}
