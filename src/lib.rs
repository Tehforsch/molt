mod ast;
mod convert;
mod ctx;
mod error;
mod grammar;
mod input;
mod mangle;
mod match_pattern;
mod parser;
mod spec;

use codespan_reporting::diagnostic::Label;
use ctx::AstCtx;
use grammar::{CustomDebug, GetSpan};
use match_pattern::MatchResult;
use spec::{Command, FullSpec, Spec};

pub use error::Error;
pub use input::{Diagnostic, FileId, Input, MoltSource};

impl MatchResult {
    fn make_diagnostics(&self, file_id: FileId) -> Vec<Diagnostic> {
        self.matches
            .iter()
            .map(|match_| {
                let binding = match_.get_binding(&self.var);
                let node = self.ctx.get_node(binding.ast.unwrap()).unwrap();
                let span = node.get_span(&self.ctx).unwrap();
                let mut diagnostic = Diagnostic::note().with_message("Match").with_labels(vec![
                    Label::primary(file_id, span.byte_range()).with_message(&self.var),
                ]);
                let mut keys = match_.iter_vars().collect::<Vec<_>>();
                keys.sort_by_key(|var| &var.name);
                for key in keys {
                    let binding = match_.get_binding(key);
                    if key == &self.var {
                        continue;
                    }
                    if let Some(node) = binding.ast.and_then(|value| self.ctx.get_node(value)) {
                        diagnostic =
                            diagnostic.with_note(format!("{} = {}", key, node.deb(&self.ctx)));
                    }
                }
                diagnostic
            })
            .collect()
    }
}

pub fn run(input: &Input) -> Result<Vec<Diagnostic>, Error> {
    let mut diagnostics = vec![];
    for (rust_file_id, rust_file) in input.iter_rust_src() {
        let ast_ctx = AstCtx::parse(rust_file)?;
        let (pat_ctx, spec) = FullSpec::new(&input)?;
        match &spec.command {
            Command::Transform(_, _) => {
                todo!()
            }
            Command::Match(pat_var) => {
                let match_result = spec.spec.match_pattern(ast_ctx, pat_ctx, pat_var);
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
    test_match_pattern!(indirection);
}
