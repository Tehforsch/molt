use std::str::FromStr;

use codespan_reporting::files::Files;
use molt_lib::{Id, Match, MatchCtx, Span};
use rust_grammar::{Node, TokenStream};

use crate::{
    Error, FileId, Input, MatchResult,
    resolve::{TokenVar, get_vars_in_token_stream},
};

#[derive(Debug, thiserror::Error)]
pub enum TransformError {
    #[error("Overlapping spans.")]
    Overlap,
}

#[derive(Debug)]
struct Transformation {
    span: Span,
    new_code: String,
}
impl Transformation {
    fn apply(&self, code: &mut String) {
        code.replace_range(self.span.byte_range(), &self.new_code);
    }
}

pub fn transform(
    input: &Input,
    rust_file_id: FileId,
    match_result: MatchResult,
    input_var: Id,
    output_var: Id,
) -> Result<(), Error> {
    let transformations: Vec<_> = match_result
        .matches
        .iter()
        .map(|match_| make_transformation(&match_result.ctx, match_, input_var, output_var))
        .collect();
    // TODO: assert spans are ascending
    check_overlap(&transformations)?;
    let mut code = input.source(rust_file_id).unwrap().to_owned();
    // revert to make sure we don't invalidate the
    // spans higher up in the file.
    for transformation in transformations.into_iter().rev() {
        transformation.apply(&mut code);
    }
    write_to_file(input, rust_file_id, code)?;
    Ok(())
}

fn write_to_file(input: &Input, rust_file_id: FileId, code: String) -> Result<(), Error> {
    let path = input.name(rust_file_id).unwrap();
    let path = path.unwrap_path();
    std::fs::write(path, code)?;
    Ok(())
}

fn check_overlap(transformations: &[Transformation]) -> Result<(), Error> {
    let mut last_byte = None;
    for tf in transformations.iter() {
        if let Some(last_byte) = last_byte {
            if tf.span.byte_range().start <= last_byte {
                return Err(TransformError::Overlap.into());
            }
        }
        last_byte = Some(tf.span.byte_range().end);
    }
    Ok(())
}

fn make_transformation(
    ctx: &MatchCtx<Node>,
    match_: &Match,
    input: Id,
    output: Id,
) -> Transformation {
    let ast = match_.get_binding(input).ast.unwrap();
    let ast_span = ctx.ast_ctx.get_span(ast);
    Transformation {
        span: ast_span,
        new_code: get_transformed_code(ctx, match_, output),
    }
}

fn get_transformed_code(ctx: &MatchCtx<Node>, match_: &Match, output: Id) -> String {
    let binding = match_.get_binding(output);
    let mut code = if let Some(ast_binding) = binding.ast {
        ctx.print_ast(ast_binding).to_string()
    } else {
        ctx.print_pat(binding.pat.unwrap()).to_string()
    };
    loop {
        let variables = contained_variables(&code);
        if variables.is_empty() {
            break;
        }
        replace_first_variable(ctx, match_, &mut code, variables);
    }
    code
}

fn replace_first_variable(
    ctx: &MatchCtx<Node>,
    match_: &Match,
    sc: &mut String,
    mut vars: Vec<TokenVar>,
) {
    if let Some(var) = vars.pop() {
        let id = ctx.pat_ctx.get_id_by_name(&var.name);
        let new_code = get_transformed_code(ctx, match_, id);
        sc.replace_range(var.span.byte_range(), &new_code);
    }
}

fn contained_variables(code: &str) -> Vec<TokenVar> {
    let mut vars = vec![];
    let tokens = TokenStream::from_str(code).unwrap();
    get_vars_in_token_stream(&mut vars, tokens);
    vars
}
