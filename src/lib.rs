mod error;
mod input;
pub(crate) mod molt_grammar;
mod resolve;

use codespan_reporting::{diagnostic::Label, files::Files};
use error::emit_error;
use molt_grammar::{Command, MoltFile};

pub use error::Error;
pub use input::{Diagnostic, FileId, Input, MoltSource};
use molt_lib::{Id, Match, MatchCtx};
use rust_grammar::{
    Node,
    parse::{Parse, ParseStream},
};

pub struct RustFile(rust_grammar::File);

impl Parse for RustFile {
    fn parse(input: ParseStream) -> rust_grammar::Result<Self> {
        Ok(Self(input.parse()?))
    }
}

type Ctx = molt_lib::Ctx<Node>;
pub type PatCtx = Ctx;
pub type AstCtx = Ctx;

fn parse_rust_file(code: &str) -> Result<(RustFile, Ctx), crate::Error> {
    Ok(rust_grammar::parse_ctx(code)?)
}

fn parse_molt_file(code: &str) -> Result<(MoltFile, Ctx), crate::Error> {
    Ok(rust_grammar::parse_ctx(code)?)
}

pub(crate) struct MatchResult {
    pub matches: Vec<Match>,
    pub ctx: MatchCtx<Node>,
    pub var: Id,
}

impl MoltFile {
    pub(crate) fn new(input: &Input) -> Result<(Self, PatCtx), Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let result = parse_molt_file(source);
        result.map_err(|err| {
            emit_error(input, file_id, &err);
            err
        })
    }

    pub(crate) fn match_pattern(
        &self,
        ast_ctx: Ctx,
        pat_ctx: Ctx,
        var: Id,
        rust_src: &str,
        molt_src: &str,
    ) -> MatchResult {
        let ctx = MatchCtx::new(pat_ctx, ast_ctx, rust_src, molt_src);
        ctx.dump();
        let pat_kind = ctx.pat_ctx.get_kind(var);
        let matches = ctx
            .ast_ctx
            .iter()
            .flat_map(|item| {
                dbg!(&item);
                let kind = ctx.ast_ctx.get_kind(item);
                if pat_kind != kind {
                    vec![]
                } else {
                    molt_lib::match_pattern(&ctx, &self.vars, var.clone(), item)
                }
            })
            .collect();
        MatchResult {
            matches,
            ctx,
            var: var.clone(),
        }
    }
}

impl RustFile {
    pub(crate) fn new(input: &Input, file_id: FileId) -> Result<(Self, Ctx), Error> {
        let source = input.source(file_id).unwrap();
        let result = parse_rust_file(source);
        match result {
            Ok((file, ctx)) => Ok((file, ctx)),
            Err(err) => {
                emit_error(input, file_id, &err);
                Err(err)
            }
        }
    }
}

impl MatchResult {
    fn make_diagnostics(&self, file_id: FileId) -> Vec<Diagnostic> {
        self.matches
            .iter()
            .map(|match_| {
                let binding = match_.get_binding(self.var);
                let span = self.ctx.ast_ctx.get_span(binding.ast.unwrap());
                let var_name = |var| self.ctx.get_var(var).name();
                let mut diagnostic = Diagnostic::note().with_message("Match").with_labels(vec![
                    Label::primary(file_id, span.byte_range()).with_message(var_name(self.var)),
                ]);
                let mut keys = match_.iter_vars().collect::<Vec<_>>();
                keys.sort_by_key(|var| var_name(*var).to_string());
                for key in keys {
                    let binding = match_.get_binding(key);
                    if key == self.var {
                        continue;
                    }
                    if let Some(node) = binding.ast {
                        diagnostic = diagnostic.with_note(format!(
                            "{} = {}",
                            var_name(key),
                            self.ctx.print_ast(node),
                        ));
                    }
                }
                diagnostic
            })
            .collect()
    }
}

pub fn run(input: &Input) -> Result<Vec<Diagnostic>, Error> {
    let mut diagnostics = vec![];
    for rust_file_id in input.iter_rust_src() {
        let (_, ast_ctx) = RustFile::new(&input, rust_file_id)?;
        let (mut molt_file, pat_ctx) = MoltFile::new(&input)?;
        molt_file.sort_vars(&pat_ctx)?;
        let command = molt_file.get_command()?;
        match command {
            Command::Match(pat_var) => {
                let match_result = molt_file.match_pattern(
                    ast_ctx,
                    pat_ctx,
                    pat_var,
                    input.source(rust_file_id).unwrap(),
                    input.source(input.molt_file_id()).unwrap(),
                );
                diagnostics.extend(match_result.make_diagnostics(rust_file_id));
            }
        };
    }
    Ok(diagnostics)
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use insta::assert_snapshot;

    use crate::{
        error::emit_diagnostic_str,
        input::{Input, MoltSource},
    };

    fn match_pattern(fname: &str) -> String {
        let rust_path = Path::new("test_data").join(format!("{}.rs", fname));
        let molt_path = Path::new("test_data").join(format!("{}.molt", fname));
        let input = Input::new(MoltSource::file(molt_path).unwrap())
            .with_rust_src_file(&rust_path)
            .unwrap();
        let diagnostics = super::run(&input).unwrap();
        diagnostics
            .into_iter()
            .map(|diagnostic| emit_diagnostic_str(&input, diagnostic))
            .collect::<Vec<_>>()
            .join("")
    }

    macro_rules! test_match_pattern {
        ($name: ident) => {
            #[test]
            fn $name() {
                assert_snapshot!(match_pattern(stringify!($name)));
            }
        };
    }

    test_match_pattern!(ident);
    test_match_pattern!(exprs);
    test_match_pattern!(multiple_vars);
    test_match_pattern!(function);
    test_match_pattern!(function2);
    test_match_pattern!(sum);
    // test_match_pattern!(indirection);
}
