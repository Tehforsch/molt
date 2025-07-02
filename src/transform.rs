use std::process::Command;
use std::str::FromStr;

use codespan_reporting::files::Files;
use molt_lib::{Id, MatchCtx, MatchRe, Span};
use rust_grammar::{Node, TokenStream};

use crate::{
    Error, FileId, Input, MatchResult, molt_grammar::TokenVar, resolve::get_vars_in_token_stream,
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
        let range = self.span.byte_range();
        println!("-------- ORIGINAL --------");
        println!("{}", &code[range.clone()]);
        println!("--------   NEW    --------");
        println!("{}", &self.new_code);
        code.replace_range(range, &self.new_code);
    }
}

pub fn transform(
    input: &Input,
    rust_file_id: FileId,
    match_result: MatchResult,
    input_var: Id,
    output_var: Id,
) -> Result<(), Error> {
    let code = get_transformed_contents(input, rust_file_id, match_result, input_var, output_var)?;
    write_to_file(input, rust_file_id, code)?;
    Ok(())
}

pub fn get_transformed_contents(
    input: &Input,
    rust_file_id: FileId,
    match_result: MatchResult<'_>,
    input_var: Id,
    output_var: Id,
) -> Result<String, Error> {
    let transformations: Vec<_> = match_result
        .matches
        .iter()
        .map(|match_| make_transformation(&match_result.ctx, match_, input_var, output_var))
        .collect();
    check_overlap(&transformations)?;
    let mut code = input.source(rust_file_id).unwrap().to_owned();
    for transformation in transformations.into_iter().rev() {
        transformation.apply(&mut code);
    }
    format_code(code)
}

fn format_code(code: String) -> Result<String, Error> {
    let mut cmd = Command::new("rustfmt")
        .arg("--emit=stdout")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()?;

    if let Some(stdin) = cmd.stdin.take() {
        use std::io::Write;
        let mut stdin = stdin;
        stdin.write_all(code.as_bytes())?;
    }

    let output = cmd.wait_with_output()?;

    if !output.status.success() {
        // If rustfmt fails, return the original code
        eprintln!(
            "rustfmt failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        return Ok(code);
    }

    Ok(String::from_utf8(output.stdout).unwrap_or(code))
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
    match_: &MatchRe,
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

fn get_transformed_code(ctx: &MatchCtx<Node>, match_: &MatchRe, output: Id) -> String {
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
    match_: &MatchRe,
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
