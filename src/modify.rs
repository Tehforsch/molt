mod diff;

use std::io::{self, Write};
use std::path::Path;
use std::str::FromStr;

use crate::rust_grammar::{Node, TokenStream};
use crate::{Config, Id, Match, MatchCtx, Span};
use codespan_reporting::files::Files;

use crate::ctrl_c::FileRestorer;
use crate::molt_grammar::TokenVar;
use crate::resolve::get_vars_in_token_stream;
use crate::{Error, FileId, Input, MatchResult};

#[derive(Debug, thiserror::Error)]
pub enum ModifyError {
    #[error("Overlapping spans.")]
    Overlap,
}

#[derive(Debug)]
struct Modification {
    span: Span,
    new_code: String,
}

pub struct Modify<'a> {
    modifications: Vec<Modification>,
    code: String,
    rust_file_path: &'a Path,
    config: &'a Config,
    cargo_root: Option<&'a Path>,
}

impl<'a> Modify<'a> {
    pub fn new(
        input: &'a Input,
        config: &'a Config,
        rust_file_id: FileId,
        cargo_root: Option<&'a Path>,
        match_result: MatchResult<'a>,
        modifies: &'a [(Id, Id)],
    ) -> Result<Self, Error> {
        let code = input.source(rust_file_id).unwrap().to_owned();
        let filename = input.name(rust_file_id).unwrap().unwrap_path();
        let modifications = prepare_modifications(&match_result, modifies)?;
        Ok(Self {
            modifications,
            code,
            rust_file_path: filename,
            config,
            cargo_root,
        })
    }

    pub fn get_modified_contents(mut self) -> Result<String, Error> {
        for modification in self.modifications.iter().rev() {
            modification.show_diff(&self.code, self.rust_file_path);
            let not_interactive_or_user_said_yes =
                !self.config.interactive || ask_user_for_confirmation();
            let no_compile_check_or_code_still_compiles =
                self.config.check.is_none() || self.post_modification_check_ok(modification)?;
            if not_interactive_or_user_said_yes && no_compile_check_or_code_still_compiles {
                self.code = modification.apply(self.code.clone());
            }
        }
        Ok(self.code.clone())
    }

    #[cfg(test)]
    pub fn get_diff_output(self) -> Result<String, Error> {
        let mut diff_output = String::new();
        for modification in self.modifications.into_iter().rev() {
            let diff_str = modification.get_diff(&self.code, self.rust_file_path, false);
            diff_output.push_str(&diff_str);
        }
        Ok(diff_output)
    }

    fn post_modification_check_ok(&self, modification: &Modification) -> Result<bool, Error> {
        let command = self.config.check.as_ref().unwrap();
        let original_code = &self.code;
        let new_code = modification.apply(original_code.clone());
        self.write_temporary_code_to_file(new_code, original_code.clone())?;

        // TODO: This is probably not the right way to
        // do this, but I am not sure how to do this robustly.
        let mut cmd = std::process::Command::new("/usr/bin/env");
        cmd.arg("sh").arg("-c").arg(command);
        if let Some(root) = self.cargo_root {
            cmd.current_dir(root);
        }
        let output = cmd.output()?;
        if output.status.success() {
            self.forget_temporary_modification()?;
            Ok(true)
        } else {
            self.restore_temporary_modification(original_code)?;
            Ok(false)
        }
    }

    fn write_temporary_code_to_file(
        &self,
        new_code: String,
        original_code: String,
    ) -> Result<(), Error> {
        std::fs::write(self.rust_file_path, new_code)?;
        FileRestorer::global()
            .remember_original_file_contents(self.rust_file_path, original_code)
            .map_err(|e| Error::Io(e))
    }

    fn forget_temporary_modification(&self) -> Result<(), Error> {
        FileRestorer::global()
            .forget_file(self.rust_file_path)
            .map_err(|e| Error::Io(e))
    }

    fn restore_temporary_modification(&self, original_code: &str) -> Result<(), Error> {
        std::fs::write(self.rust_file_path, original_code)?;
        self.forget_temporary_modification()
    }
}

impl Modification {
    fn apply(&self, mut code: String) -> String {
        let range = self.span.byte_range();
        code.replace_range(range, &self.new_code);
        code
    }
}

pub fn modify(
    input: &Input,
    config: &Config,
    rust_file_id: FileId,
    match_result: MatchResult,
    modifies: Vec<(Id, Id)>,
    cargo_root: Option<&Path>,
) -> Result<(), Error> {
    let modify = Modify::new(
        input,
        config,
        rust_file_id,
        cargo_root,
        match_result,
        &modifies,
    )?;
    if !modify.modifications.is_empty() {
        let new_code = modify.get_modified_contents()?;
        write_to_file(input, rust_file_id, new_code)?;
    }
    Ok(())
}

fn ask_user_for_confirmation() -> bool {
    print!("Apply this modification? (y/N): ");
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

fn prepare_modifications(
    match_result: &MatchResult,
    modifies: &[(Id, Id)],
) -> Result<Vec<Modification>, Error> {
    let mut all_modifications: Vec<_> = modifies
        .iter()
        .flat_map(|(input_var, output_var)| {
            match_result
                .matches
                .iter()
                .map(|match_| make_modification(&match_result.ctx, match_, *input_var, *output_var))
        })
        .collect();

    // Sort modifications by their spans to ensure proper ordering
    all_modifications.sort_by_key(|t| t.span.byte_range().start);

    check_overlap(&all_modifications)?;
    Ok(all_modifications)
}

fn check_overlap(modifications: &[Modification]) -> Result<(), Error> {
    let mut last_byte = None;
    for tf in modifications.iter() {
        if let Some(last_byte) = last_byte {
            if tf.span.byte_range().start <= last_byte {
                return Err(ModifyError::Overlap.into());
            }
        }
        last_byte = Some(tf.span.byte_range().end);
    }
    Ok(())
}

fn make_modification(ctx: &MatchCtx<Node>, match_: &Match, input: Id, output: Id) -> Modification {
    let ast = *match_.get_binding(input).ast.first().unwrap();
    let ast_span = ctx.ast_ctx.get_span(ast);
    Modification {
        span: ast_span,
        new_code: get_modified_code(ctx, match_, output),
    }
}

fn get_modified_code(ctx: &MatchCtx<Node>, match_: &Match, output: Id) -> String {
    let binding = match_.get_binding(output);
    let mut code = if let Some(ast_binding) = binding.ast.first() {
        ctx.print_ast(*ast_binding).to_string()
    } else {
        let pat_id = binding.pat.unwrap();
        if pat_id.is_pat() {
            get_modified_code(ctx, match_, pat_id)
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
        let new_code = get_modified_code(ctx, match_, var_id);
        sc.replace_range(var.span.byte_range(), &new_code);
    }
}

fn contained_variables(code: &str) -> Vec<TokenVar> {
    let mut vars = vec![];
    let tokens = TokenStream::from_str(code).unwrap();
    get_vars_in_token_stream(&mut vars, tokens);
    vars
}
