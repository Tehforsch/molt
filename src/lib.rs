mod error;
mod input;
mod lsp;
pub(crate) mod molt_grammar;
mod resolve;
#[cfg(test)]
mod tests;
mod transform;
mod type_check;
mod utils;

use std::path::{Path, PathBuf};

use codespan_reporting::diagnostic::Label;
use codespan_reporting::files::Files;
pub use error::{Error, emit_error};
pub use input::{Diagnostic, FileId, Input, MoltSource};
use lsp::LspClient;
use molt_grammar::{Command, MatchCommand, MoltFile, TransformCommand, UnresolvedMoltFile};
use molt_lib::{Config, Id, Match, MatchCtx, MatchPatternData, ParsingMode};
use rust_grammar::Node;

pub struct RustFile;

type Ctx = molt_lib::Ctx<Node>;
pub type PatCtx = Ctx;
pub type AstCtx = Ctx;

fn match_pattern_data<'a>(
    input: &'a Input,
    rust_file_id: crate::FileId,
    config: &'a Config,
) -> MatchPatternData<'a> {
    MatchPatternData {
        rust_path: input.name(rust_file_id).unwrap().unwrap_path(),
        rust_src: input.source(rust_file_id).unwrap(),
        molt_src: input.source(input.molt_file_id()).unwrap(),
        config,
    }
}

pub(crate) struct MatchResult<'a> {
    pub matches: Vec<Match>,
    pub ctx: MatchCtx<'a, Node>,
    pub var: Id,
}

impl MoltFile {
    pub(crate) fn new(input: &Input) -> Result<(Self, PatCtx), Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let unresolved: UnresolvedMoltFile = rust_grammar::parse_str(source, ParsingMode::Pat)
            .map_err(|e| Error::parse(e, file_id))?;
        unresolved.resolve(file_id)
    }

    pub(crate) fn match_pattern<'a>(
        &'a self,
        ast_ctx: &'a Ctx,
        pat_ctx: &'a Ctx,
        var: Id,
        data: MatchPatternData<'a>,
        lsp_client: &'a mut LspClient,
    ) -> MatchResult<'a> {
        let ctx = MatchCtx::new(pat_ctx, ast_ctx, data.clone());
        if ctx.config().debug_print {
            ctx.dump();
        }
        let pat_kind = ctx.pat_ctx.get_kind(var);
        let matches = ctx
            .ast_ctx
            .iter()
            .flat_map(|item| {
                let kind = ctx.ast_ctx.get_kind(item);
                if pat_kind != kind {
                    vec![]
                } else {
                    molt_lib::match_pattern(&ctx, &self.vars, var, item)
                }
            })
            .filter(|match_| {
                lsp_client.check_type_annotations(
                    &self.type_annotations,
                    ast_ctx,
                    pat_ctx,
                    &data,
                    match_,
                )
            })
            .collect();
        MatchResult { matches, ctx, var }
    }
}

impl RustFile {
    pub(crate) fn new(input: &Input, file_id: FileId) -> Result<(Self, Ctx), Error> {
        let source = input.source(file_id).unwrap();
        rust_grammar::parse_file(source, ParsingMode::Real)
            .map_err(|e| Error::parse(e, file_id))
            .map(|(_, ctx)| (RustFile, ctx))
    }
}

impl<'a> MatchResult<'a> {
    fn make_diagnostics(&self, file_id: FileId, print: Option<Id>) -> Vec<Diagnostic> {
        self.matches
            .iter()
            .map(|match_| {
                let match_var = print.unwrap_or(self.var);
                let binding = match_.get_binding(match_var);
                let span = self.ctx.ast_ctx.get_span(*binding.ast.first().unwrap());
                let var_name = |var| format!("${}", self.ctx.get_var(var).name());
                let mut diagnostic = Diagnostic::note().with_message("Match").with_labels(vec![
                    Label::primary(file_id, span.byte_range()).with_message(var_name(match_var)),
                ]);
                let mut keys = match_.iter_vars().collect::<Vec<_>>();
                keys.sort_by_key(|var| var_name(*var).to_string());
                for key in keys {
                    let binding = match_.get_binding(key);
                    if key == match_var {
                        continue;
                    }
                    for node in binding.ast.iter() {
                        diagnostic = diagnostic.with_note(format!(
                            "{} = {}",
                            var_name(key),
                            self.ctx.print_ast(*node),
                        ));
                    }
                }
                diagnostic
            })
            .collect()
    }
}

pub fn run(
    input: &Input,
    config: crate::Config,
    cargo_root: Option<&PathBuf>,
) -> Result<Vec<Diagnostic>, Error> {
    let mut diagnostics = vec![];
    let (mut molt_file, pat_ctx) = MoltFile::new(input)?;

    let mut lsp_client = {
        if molt_file.type_annotations.is_empty() {
            LspClient::uninitialized()
        } else {
            LspClient::new(
                input
                    .root()
                    .unwrap_or_else(|| panic!("Required LSP but no root directory given.")),
            )
            .map_err(|e| Error::Misc(format!("Failed to create LSP client: {e}")))?
        }
    };

    for rust_file_id in input.iter_rust_src() {
        let (_, ast_ctx) = RustFile::new(input, rust_file_id)?;
        let data = match_pattern_data(input, rust_file_id, &config);
        match molt_file.command() {
            Command::Match(MatchCommand {
                match_: pat_var,
                print,
            }) => {
                let match_result = molt_file.match_pattern(
                    &ast_ctx,
                    &pat_ctx,
                    pat_var.unwrap(),
                    data,
                    &mut lsp_client,
                );
                diagnostics.extend(match_result.make_diagnostics(rust_file_id, print));
            }
            Command::Transform(TransformCommand { transforms, match_ }) => {
                let match_result = molt_file.match_pattern(
                    &ast_ctx,
                    &pat_ctx,
                    match_.unwrap(),
                    data,
                    &mut lsp_client,
                );
                transform::transform(
                    input,
                    rust_file_id,
                    match_result,
                    transforms,
                    config.interactive,
                )?;
                if let Some(cargo_root) = cargo_root
                    && config.cargo_fmt
                {
                    run_cargo_fmt(cargo_root)?;
                }
            }
        };
    }
    Ok(diagnostics)
}

fn run_cargo_fmt(cargo_root: &Path) -> Result<(), Error> {
    let status = std::process::Command::new("cargo")
        .arg("fmt")
        .current_dir(cargo_root)
        .status()?;

    if !status.success() {
        return Err(Error::Misc(format!(
            "cargo fmt failed with exit code: {status}",
        )));
    }

    Ok(())
}
