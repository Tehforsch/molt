mod diff;

use std::io::{self, Write};
use std::path::Path;
use std::str::FromStr;

use crate::rust_grammar::{Node, TokenStream};
use codespan_reporting::files::Files;
use molt_lib::{Config, Id, Match, MatchCtx, Span};

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

pub struct Transform<'a> {
    transformations: Vec<Transformation>,
    code: String,
    rust_file_path: &'a Path,
    config: &'a Config,
    cargo_root: Option<&'a Path>,
}

impl<'a> Transform<'a> {
    pub fn new(
        input: &'a Input,
        config: &'a Config,
        rust_file_id: FileId,
        cargo_root: Option<&'a Path>,
        match_result: MatchResult<'a>,
        transforms: &'a [(Id, Id)],
    ) -> Result<Self, Error> {
        let code = input.source(rust_file_id).unwrap().to_owned();
        let filename = input.name(rust_file_id).unwrap().unwrap_path();
        let transformations = prepare_transformations(&match_result, transforms)?;
        Ok(Self {
            transformations,
            code,
            rust_file_path: filename,
            config,
            cargo_root,
        })
    }

    pub fn get_transformed_contents(mut self) -> Result<String, Error> {
        for transformation in self.transformations.iter().rev() {
            transformation.show_diff(&self.code, self.rust_file_path);
            let not_interactive_or_user_said_yes =
                !self.config.interactive || ask_user_for_confirmation();
            let no_compile_check_or_code_still_compiles = !self.config.check_compilation
                || self.check_transformation_compiles(transformation)?;
            if not_interactive_or_user_said_yes && no_compile_check_or_code_still_compiles {
                self.code = transformation.apply(self.code.clone());
            }
        }
        Ok(self.code.clone())
    }

    #[cfg(test)]
    pub fn get_diff_output(self) -> Result<String, Error> {
        let mut diff_output = String::new();
        for transformation in self.transformations.into_iter().rev() {
            let diff_str = transformation.get_diff(&self.code, self.rust_file_path, false);
            diff_output.push_str(&diff_str);
        }
        Ok(diff_output)
    }

    fn check_transformation_compiles(
        &self,
        transformation: &Transformation,
    ) -> Result<bool, Error> {
        let original_code = &self.code;
        let new_code = transformation.apply(original_code.clone());
        self.write_code_to_file(new_code)?;
        let mut cmd = std::process::Command::new("cargo");
        cmd.arg("check");
        cmd.arg("--all-targets");
        if let Some(root) = self.cargo_root {
            cmd.current_dir(root);
        }
        let output = cmd.output()?;
        if output.status.success() {
            Ok(true)
        } else {
            self.write_code_to_file(original_code.to_string())?;
            Ok(false)
        }
    }

    fn write_code_to_file(&self, original_code: String) -> Result<(), Error> {
        std::fs::write(self.rust_file_path, original_code)?;
        Ok(())
    }
}

impl Transformation {
    fn apply(&self, mut code: String) -> String {
        let range = self.span.byte_range();
        code.replace_range(range, &self.new_code);
        code
    }
}

pub fn transform(
    input: &Input,
    config: &Config,
    rust_file_id: FileId,
    match_result: MatchResult,
    transforms: Vec<(Id, Id)>,
    cargo_root: Option<&Path>,
) -> Result<(), Error> {
    let transform = Transform::new(
        input,
        config,
        rust_file_id,
        cargo_root,
        match_result,
        &transforms,
    )?;
    if !transform.transformations.is_empty() {
        let new_code = transform.get_transformed_contents()?;
        write_to_file(input, rust_file_id, new_code)?;
    }
    Ok(())
}

fn ask_user_for_confirmation() -> bool {
    print!("Apply this transformation? (y/N): ");
    io::stdout().flush().unwrap();

    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();

    matches!(input.trim().to_lowercase().as_str(), "y" | "yes")
}

fn write_to_file(input: &Input, rust_file_id: FileId, code: String) -> Result<(), Error> {
    let path = input.name(rust_file_id).unwrap();
    let path = path.unwrap_path();
    std::fs::write(path, code)?;
    Ok(())
}

fn prepare_transformations(
    match_result: &MatchResult,
    transforms: &[(Id, Id)],
) -> Result<Vec<Transformation>, Error> {
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
    Ok(all_transformations)
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
