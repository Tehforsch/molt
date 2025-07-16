use crate::parser::error::Result;
use crate::parser::parse::{Parse, ParseStream};
use crate::parser::punctuated::Punctuated;
use crate::parser::token;
use crate::rust_grammar::data::{Fields, FieldsNamed, Variant};
use crate::rust_grammar::generics::WhereClause;

pub(crate) fn data_struct(
    input: ParseStream,
) -> Result<(Option<WhereClause>, Fields, Option<Token![;]>)> {
    let mut lookahead = input.lookahead1();
    let mut where_clause = None;
    if lookahead.peek(Token![where]) {
        where_clause = Some(input.parse()?);
        lookahead = input.lookahead1();
    }

    if where_clause.is_none() && lookahead.peek(token::Paren) {
        let fields = input.parse()?;

        lookahead = input.lookahead1();
        if lookahead.peek(Token![where]) {
            where_clause = Some(input.parse()?);
            lookahead = input.lookahead1();
        }

        if lookahead.peek(Token![;]) {
            let semi = input.parse()?;
            Ok((where_clause, Fields::Unnamed(fields), Some(semi)))
        } else {
            Err(lookahead.error())
        }
    } else if lookahead.peek(token::Brace) {
        let fields = input.parse()?;
        Ok((where_clause, Fields::Named(fields), None))
    } else if lookahead.peek(Token![;]) {
        let semi = input.parse()?;
        Ok((where_clause, Fields::Unit, Some(semi)))
    } else {
        Err(lookahead.error())
    }
}

pub(crate) fn data_enum(
    input: ParseStream,
) -> Result<(
    Option<WhereClause>,
    token::Brace,
    Punctuated<Variant, Token![,]>,
)> {
    let where_clause = input.parse()?;

    let content;
    let brace = braced!(content in input);
    let variants = content.parse_terminated(Variant::parse, Token![,])?;

    Ok((where_clause, brace, variants))
}

pub(crate) fn data_union(input: ParseStream) -> Result<(Option<WhereClause>, FieldsNamed)> {
    let where_clause = input.parse()?;
    let fields = input.parse()?;
    Ok((where_clause, fields))
}
