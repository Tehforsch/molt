use std::io::{self, Write};
use std::str::FromStr;

use codespan_reporting::files::Files;
use molt_lib::{Id, Match, MatchCtx, Span};
use rust_grammar::{Node, TokenStream};

use crate::molt_grammar::TokenVar;
use crate::resolve::get_vars_in_token_stream;
use crate::{Error, FileId, Input, MatchResult};

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
        let range = self.span.byte_range();
        code.replace_range(range, &self.new_code);
    }

    fn show_diff(&self, _: &str) {}
}

pub fn transform(
    input: &Input,
    rust_file_id: FileId,
    match_result: MatchResult,
    transforms: Vec<(Id, Id)>,
    interactive: bool,
) -> Result<(), Error> {
    if !transforms.is_empty() {
        let code =
            get_transformed_contents(input, rust_file_id, match_result, &transforms, interactive)?;
        write_to_file(input, rust_file_id, code)?;
    }
    Ok(())
}

pub fn get_transformed_contents(
    input: &Input,
    rust_file_id: FileId,
    match_result: MatchResult<'_>,
    transforms: &[(Id, Id)],
    interactive: bool,
) -> Result<String, Error> {
    let mut all_transformations: Vec<_> = transforms
        .iter()
        .flat_map(|(input_var, output_var)| {
            match_result.matches.iter().map(|match_| {
                make_transformation(&match_result.ctx, match_, *input_var, *output_var)
            })
        })
        .collect();

    // Sort transformations by their spans to ensure proper ordering
    all_transformations.sort_by_key(|t| t.span.byte_range().start);

    check_overlap(&all_transformations)?;
    let mut code = input.source(rust_file_id).unwrap().to_owned();
    for transformation in all_transformations.into_iter().rev() {
        transformation.show_diff(&code);
        if !interactive || ask_user_for_confirmation() {
            transformation.apply(&mut code);
        }
    }
    Ok(code)
}

fn ask_user_for_confirmation() -> bool {
    print!("Apply this transformation? (y/N): ");
    io::stdout().flush().unwrap();

    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();

    match input.trim().to_lowercase().as_str() {
        "y" | "yes" => true,
        _ => false,
    }
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
    let ast = *match_.get_binding(input).ast.first().unwrap();
    let ast_span = ctx.ast_ctx.get_span(ast);
    Transformation {
        span: ast_span,
        new_code: get_transformed_code(ctx, match_, output),
    }
}

fn get_transformed_code(ctx: &MatchCtx<Node>, match_: &Match, output: Id) -> String {
    let binding = match_.get_binding(output);
    let mut code = if let Some(ast_binding) = binding.ast.first() {
        ctx.print_ast(*ast_binding).to_string()
    } else {
        let pat_id = binding.pat.unwrap();
        if pat_id.is_pat() {
            get_transformed_code(ctx, match_, pat_id)
        } else {
            ctx.print_pat(binding.pat.unwrap()).to_string()
        }
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
        let var_id = ctx.pat_ctx.get_id_by_name(&var.name);
        let new_code = get_transformed_code(ctx, match_, var_id);
        sc.replace_range(var.span.byte_range(), &new_code);
    }
}

fn contained_variables(code: &str) -> Vec<TokenVar> {
    let mut vars = vec![];
    let tokens = TokenStream::from_str(code).unwrap();
    get_vars_in_token_stream(&mut vars, tokens);
    vars
}
