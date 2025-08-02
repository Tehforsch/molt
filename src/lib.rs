#[macro_use]
mod parser;
mod cmp_syn;
mod config;
mod ctrl_c;
mod ctx;
mod error;
mod input;
mod lsp;
mod match_ctx;
mod match_pattern;
mod modify;
mod molt_grammar;
mod node;
mod node_list;
mod pattern;
mod resolve;
pub mod rule;
mod rust_grammar;
mod span;
#[cfg(test)]
mod tests;
mod type_check;
mod utils;

use std::path::{Path, PathBuf};

use crate::rust_grammar::Node;
pub use cmp_syn::CmpSyn;
use codespan_reporting::diagnostic::Label;
use codespan_reporting::files::Files;
pub use config::Config;
pub use ctx::{Ctx, Id, NodeId, Var, VarDecl};
pub use error::{Error, emit_error};
pub use input::{Diagnostic, FileId, Input, MoltSource};
use lsp::LspClient;
pub use match_ctx::{MatchCtx, MatchPatternData};
pub use match_pattern::{Binding, Match, Matcher, PatType, match_pattern};
use molt_grammar::{Command, MatchCommand, ModifyCommand, MoltFile, UnresolvedMoltFile};
pub use node::{KindType, NodeType, ToNode};
pub use node_list::{
    List, ListMatchingMode, NoPunct, NodeList, PatNodeList, RealNodeList, SetMatchingMode, Single,
    SingleMatchingMode,
};
pub use pattern::Pattern;
use rule::Rules;
pub use span::{Span, Spanned, SpannedPat, WithSpan};

struct RustFile;

type CtxR = crate::ctx::Ctx<Node>;
type PatCtx = CtxR;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParsingMode {
    Real,
    Pat,
}

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

struct MatchResult<'a> {
    matches: Vec<Match>,
    ctx: MatchCtx<'a, Node>,
    var: Id,
}

impl MoltFile {
    fn new(input: &Input) -> Result<(Self, PatCtx), Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let unresolved: UnresolvedMoltFile = crate::parser::parse_str(source, ParsingMode::Pat)
            .map_err(|e| Error::parse(e, file_id))?;
        unresolved.resolve(file_id)
    }

    fn get_rules(&self) -> Rules {
        let mut rules = Rules::default();
        for ruleset in self.rulesets.iter() {
            for key in ruleset.keys.iter() {
                rules[*key] = ruleset.rule;
            }
        }
        rules
    }

    fn match_pattern<'a>(
        &'a self,
        ast_ctx: &'a CtxR,
        pat_ctx: &'a CtxR,
        var: Id,
        data: MatchPatternData<'a>,
        lsp_client: &'a mut LspClient,
    ) -> MatchResult<'a> {
        let rules = &self.get_rules();
        let ctx = MatchCtx::new(pat_ctx, ast_ctx, data.clone());
        if ctx.config().debug_print {
            ctx.dump();
        }
        let pat_kind = ctx.pat_ctx.get_var(var).kind();
        let matches = ctx
            .ast_ctx
            .iter()
            .flat_map(|item| {
                let node: Pattern<&Node, Id> = ctx.ast_ctx.get(item);
                let is_of_kind = match node {
                    crate::Pattern::Real(node) => node.is_of_kind(pat_kind),
                    crate::Pattern::Pat(var) => ctx
                        .ast_ctx
                        .get_var(var)
                        .kind()
                        .is_comparable_to(pat_kind.into_node_kind()),
                };
                if is_of_kind {
                    crate::match_pattern(&ctx, &self.vars, var, item, rules)
                } else {
                    vec![]
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
    fn new(input: &Input, file_id: FileId) -> Result<(Self, CtxR), Error> {
        let source = input.source(file_id).unwrap();
        crate::rust_grammar::parse_file(source, ParsingMode::Real)
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
            Command::Modify(ModifyCommand { modifies, match_ }) => {
                let match_result = molt_file.match_pattern(
                    &ast_ctx,
                    &pat_ctx,
                    match_.unwrap(),
                    data,
                    &mut lsp_client,
                );
                modify::modify(
                    input,
                    &config,
                    rust_file_id,
                    match_result,
                    modifies,
                    cargo_root.map(|v| &**v),
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
