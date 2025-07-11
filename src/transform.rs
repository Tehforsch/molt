use std::io::{self, Write};
use std::path::Path;
use std::str::FromStr;

use codespan_reporting::files::Files;
use molt_lib::{Id, Match, MatchCtx, Span};
use rust_grammar::{Node, TokenStream};
use similar::{ChangeTag, TextDiff};

use crate::molt_grammar::TokenVar;
use crate::resolve::get_vars_in_token_stream;
use crate::{Error, FileId, Input, MatchResult};

pub const NUM_LINES_CONTEXT: usize = 4;

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

pub struct Transform<'a> {
    match_result: MatchResult<'a>,
    transforms: &'a [(Id, Id)],
    code: String,
    filename: &'a Path,
}

impl<'a> Transform<'a> {
    pub fn new(
        input: &'a Input,
        rust_file_id: FileId,
        match_result: MatchResult<'a>,
        transforms: &'a [(Id, Id)],
    ) -> Self {
        let code = input.source(rust_file_id).unwrap().to_owned();
        let filename = input.name(rust_file_id).unwrap().unwrap_path();
        Self {
            match_result,
            transforms,
            code,
            filename,
        }
    }

    fn prepare_transformations(&self) -> Result<Vec<Transformation>, Error> {
        let mut all_transformations: Vec<_> = self
            .transforms
            .iter()
            .flat_map(|(input_var, output_var)| {
                self.match_result.matches.iter().map(|match_| {
                    make_transformation(&self.match_result.ctx, match_, *input_var, *output_var)
                })
            })
            .collect();

        // Sort transformations by their spans to ensure proper ordering
        all_transformations.sort_by_key(|t| t.span.byte_range().start);

        check_overlap(&all_transformations)?;
        Ok(all_transformations)
    }

    pub fn get_transformed_contents(mut self, interactive: bool) -> Result<String, Error> {
        let all_transformations = self.prepare_transformations()?;

        for transformation in all_transformations.into_iter().rev() {
            transformation.show_diff(&self.code, self.filename);
            if !interactive || ask_user_for_confirmation() {
                transformation.apply(&mut self.code);
            }
        }
        Ok(self.code)
    }

    #[cfg(test)]
    pub fn get_diff_output(self) -> Result<String, Error> {
        let all_transformations = self.prepare_transformations()?;

        let mut diff_output = String::new();
        for transformation in all_transformations.into_iter().rev() {
            let diff_str = transformation.get_diff(&self.code, self.filename, false);
            diff_output.push_str(&diff_str);
        }
        Ok(diff_output)
    }
}

impl Transformation {
    fn apply(&self, code: &mut String) {
        let range = self.span.byte_range();
        code.replace_range(range, &self.new_code);
    }

    fn get_diff(&self, old_code: &str, filename: &Path, colorized: bool) -> String {
        let range = self.span.byte_range();
        let new_excerpt = &self.new_code;
        let mut new_code = old_code.to_string();
        new_code.replace_range(range, new_excerpt);

        let diff = TextDiff::from_lines(old_code, &new_code);
        format_diff(diff, filename, colorized)
    }

    fn show_diff(&self, old_code: &str, filename: &Path) {
        let diff_output = self.get_diff(old_code, filename, true);
        print!("{}", diff_output);
    }
}

fn format_diff(diff: TextDiff<str>, filename: &Path, colorized: bool) -> String {
    let mut output = String::new();

    if colorized {
        const BOLD: &str = "\x1b[1m";
        const CYAN: &str = "\x1b[36m";
        const RESET: &str = "\x1b[0m";
        output.push_str(&format!(
            "{}{}--- {}{}\n",
            BOLD,
            CYAN,
            filename.to_string_lossy(),
            RESET
        ));
    } else {
        output.push_str(&format!("--- {}\n", filename.to_string_lossy()));
    }

    for group in diff.grouped_ops(NUM_LINES_CONTEXT).iter() {
        for op in group {
            for change in diff.iter_changes(op) {
                let (sign, color) = if colorized {
                    match change.tag() {
                        ChangeTag::Delete => ("-", "\x1b[31m"),
                        ChangeTag::Insert => ("+", "\x1b[32m"),
                        ChangeTag::Equal => (" ", ""),
                    }
                } else {
                    match change.tag() {
                        ChangeTag::Delete => ("-", ""),
                        ChangeTag::Insert => ("+", ""),
                        ChangeTag::Equal => (" ", ""),
                    }
                };

                // Get line numbers for old and new
                let old_line = change.old_index().map(|i| i + 1);
                let new_line = change.new_index().map(|i| i + 1);

                // Format line numbers
                let line_info = match (old_line, new_line) {
                    (Some(old), Some(new)) => format!("{:4},{:4}", old, new),
                    (Some(old), None) => format!("{:4},-   ", old),
                    (None, Some(new)) => format!("-   ,{:4}", new),
                    (None, None) => "    ,    ".to_string(),
                };

                if colorized {
                    const CYAN: &str = "\x1b[36m";
                    const RESET: &str = "\x1b[0m";
                    if color.is_empty() {
                        output.push_str(&format!(
                            "{}{} {}{}{}",
                            CYAN, line_info, RESET, sign, change
                        ));
                    } else {
                        output.push_str(&format!(
                            "{}{} {}{}{}{}{}",
                            CYAN, line_info, RESET, color, sign, change, RESET
                        ));
                    }
                } else {
                    output.push_str(&format!("{} {}{}", line_info, sign, change));
                }
            }
        }
    }
    output.push('\n');
    output
}

pub fn transform(
    input: &Input,
    rust_file_id: FileId,
    match_result: MatchResult,
    transforms: Vec<(Id, Id)>,
    interactive: bool,
) -> Result<(), Error> {
    if !transforms.is_empty() {
        let transform = Transform::new(input, rust_file_id, match_result, &transforms);
        let code = transform.get_transformed_contents(interactive)?;
        write_to_file(input, rust_file_id, code)?;
    }
    Ok(())
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
