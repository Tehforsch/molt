mod ctx;
mod error;
mod input;
mod match_pattern;
mod parser;
mod resolve;

use codespan_reporting::{diagnostic::Label, files::Files};
use ctx::{AstCtx, PatCtx};
use error::emit_error;
use match_pattern::MatchResult;
use parser::{Command, MoltFile, RustFile, VarId, parse_molt_file, parse_rust_file};

use crate::parser::CustomDebug;

pub use error::Error;
pub use input::{Diagnostic, FileId, Input, MoltSource};

impl MoltFile {
    pub(crate) fn new(input: &Input) -> Result<(Self, PatCtx), Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let result = parse_molt_file(source);
        match result {
            Ok((file, ctx)) => Ok((file, PatCtx::new(ctx))),
            Err(err) => {
                emit_error(input, file_id, &err);
                Err(err)
            }
        }
    }
}

impl RustFile {
    pub(crate) fn new(input: &Input, file_id: FileId) -> Result<(Self, AstCtx), Error> {
        let source = input.source(file_id).unwrap();
        let result = parse_rust_file(source);
        match result {
            Ok((file, ctx)) => Ok((file, AstCtx::new(ctx))),
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
                let span = self.ctx.get_span(binding.ast.unwrap()).unwrap();
                let var_name = |var| self.ctx.get_var(var).deb(&self.ctx);
                let mut diagnostic = Diagnostic::note().with_message("Match").with_labels(vec![
                    Label::primary(file_id, span.range()).with_message(var_name(self.var)),
                ]);
                let mut keys = match_.iter_vars().collect::<Vec<_>>();
                keys.sort_by_key(|var| var_name(*var));
                for key in keys {
                    let binding = match_.get_binding(key);
                    if key == self.var {
                        continue;
                    }
                    if let Some(node) = binding
                        .ast
                        .and_then(|value| self.ctx.get_pat_node(value).as_exact())
                    {
                        diagnostic = diagnostic.with_note(format!(
                            "{} = {}",
                            var_name(key),
                            node.deb(&self.ctx)
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
        let command = molt_file.get_command()?;
        match command {
            Command::Match(pat_var) => {
                let match_result = molt_file.match_pattern(ast_ctx, pat_ctx, pat_var);
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
