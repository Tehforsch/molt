use std::path::Path;

use ast::Ast;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use error::Error;
use grammar::{CustomDebug, GetSpan};
use match_pattern::MatchResult;
use spec::{Command, FullSpec, Spec};

mod ast;
mod convert;
mod ctx;
mod error;
mod grammar;
mod mangle;
mod match_pattern;
mod parser;
mod spec;

pub fn run(path: &Path, spec_path: &Path) -> Result<(), Error> {
    println!("Checking {:?}", path);
    let ast = Ast::parse(path);
    let (ctx, spec) = FullSpec::from_path(spec_path)?;
    match &spec.command {
        Command::Transform(_, _) => {
            todo!()
        }
        Command::Match(pat_var) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = Config::default();
            let match_result = spec.spec.match_pattern(ast.ctx, ctx, pat_var);
            for diagnostic in match_result.make_diagnostics() {
                term::emit(&mut writer.lock(), &config, &ast.file, &diagnostic).unwrap();
            }
        }
    };
    Ok(())
}

impl MatchResult {
    fn make_diagnostics(&self) -> Vec<Diagnostic<()>> {
        self.matches
            .iter()
            .map(|match_| {
                let binding = match_.get_binding(&self.var);
                let node = self.ctx.get_node(binding.ast.unwrap()).unwrap();
                let span = node.get_span(&self.ctx).unwrap();
                let mut diagnostic =
                    Diagnostic::note()
                        .with_message("Match")
                        .with_labels(vec![
                            Label::primary((), span.byte_range()).with_message(&self.var)
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

#[cfg(test)]
mod tests {
    use std::path::Path;

    use codespan_reporting::{diagnostic::Diagnostic, term};
    use insta::assert_snapshot;

    use crate::{error::SourceFile, Ast, Command, FullSpec};

    fn match_pattern(fname: &str) -> String {
        let path = Path::new("test_data").join(format!("{}.rs", fname));
        let spec_path = Path::new("test_data").join(format!("{}.molt", fname));
        let ast = Ast::parse(&path);
        let (ctx, spec) = FullSpec::from_path(&spec_path).unwrap();
        let var = if let Command::Match(var) = &spec.command {
            var
        } else {
            panic!()
        };
        let diagnostics = spec
            .spec
            .match_pattern(ast.ctx, ctx, var)
            .make_diagnostics();
        diagnostics
            .into_iter()
            .map(|diagnostic| emit_diagnostic_str(&ast.file, diagnostic))
            .collect::<Vec<_>>()
            .join("")
    }

    fn emit_diagnostic_str(file: &SourceFile, diagnostic: Diagnostic<()>) -> String {
        use codespan_reporting::term::termcolor::Buffer;

        let mut writer = Buffer::no_color();
        let config = codespan_reporting::term::Config::default();
        term::emit(&mut writer, &config, file, &diagnostic).unwrap();
        String::from_utf8(writer.into_inner()).unwrap()
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
