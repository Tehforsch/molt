mod error;
mod input;
pub(crate) mod molt_grammar;
mod resolve;

pub use error::Error;
pub use error::emit_error;
pub use input::{Diagnostic, FileId, Input, MoltSource};

use codespan_reporting::{diagnostic::Label, files::Files};
use molt_grammar::{Command, MoltFile, UnresolvedMoltFile};

use molt_lib::{Id, Match, MatchCtx};
use rust_grammar::{
    Node,
    parse::{Parse, ParseStream},
};

pub struct RustFile;

impl Parse for RustFile {
    fn parse(input: ParseStream) -> rust_grammar::Result<Self> {
        let _: rust_grammar::File = input.parse()?;
        Ok(Self)
    }
}

type Ctx = molt_lib::Ctx<Node>;
pub type PatCtx = Ctx;
pub type AstCtx = Ctx;

pub(crate) struct MatchResult {
    pub matches: Vec<Match>,
    pub ctx: MatchCtx<Node>,
    pub var: Id,
}

impl MoltFile {
    pub(crate) fn new(input: &Input) -> Result<(Self, PatCtx), Error> {
        let file_id = input.molt_file_id();
        let source = input.source(file_id).unwrap();
        let unresolved: UnresolvedMoltFile =
            rust_grammar::parse_str(source).map_err(|e| Error::parse(e, file_id))?;
        unresolved.resolve(file_id)
    }

    pub(crate) fn match_pattern(
        &self,
        ast_ctx: Ctx,
        pat_ctx: Ctx,
        var: Id,
        rust_src: &str,
        molt_src: &str,
        debug_print: bool,
    ) -> MatchResult {
        let ctx = MatchCtx::new(pat_ctx, ast_ctx, rust_src, molt_src, debug_print);
        ctx.dump();
        let pat_kind = ctx.pat_ctx.get_kind(var);
        let matches = ctx
            .ast_ctx
            .iter()
            .flat_map(|item| {
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
        rust_grammar::parse_ctx(source).map_err(|e| Error::parse(e, file_id))
    }
}

impl MatchResult {
    fn make_diagnostics(&self, file_id: FileId) -> Vec<Diagnostic> {
        self.matches
            .iter()
            .map(|match_| {
                let binding = match_.get_binding(self.var);
                let span = self.ctx.ast_ctx.get_span(binding.ast.unwrap());
                let var_name = |var| format!("${}", self.ctx.get_var(var).name());
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

pub fn run(input: &Input, debug_print: bool) -> Result<Vec<Diagnostic>, Error> {
    let mut diagnostics = vec![];
    for rust_file_id in input.iter_rust_src() {
        let (_, ast_ctx) = RustFile::new(&input, rust_file_id)?;
        let (mut molt_file, pat_ctx) = MoltFile::new(&input)?;
        match molt_file.command() {
            Command::Match(pat_var) => {
                let pat_var = *pat_var;
                let match_result = molt_file.match_pattern(
                    ast_ctx,
                    pat_ctx,
                    pat_var,
                    input.source(rust_file_id).unwrap(),
                    input.source(input.molt_file_id()).unwrap(),
                    debug_print,
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

    use crate::{
        Error, MoltFile, RustFile,
        error::{emit_diagnostic_str, make_error_diagnostic},
        input::{Contents, Input, MoltSource},
    };

    fn parse_rust(path: &str) {
        let rust_path = Path::new("test_data").join(format!("{}/main.rs", path));
        let input = Input::new(MoltSource::FromCli(Contents::new("".to_string())))
            .with_rust_src_file(&rust_path)
            .unwrap();
        for rust_file_id in input.iter_rust_src() {
            RustFile::new(&input, rust_file_id).unwrap();
        }
    }

    fn parse_molt(path: &str) -> (Input, Result<MoltFile, Error>) {
        let input = Input::new(MoltSource::file(path).unwrap());
        let map = MoltFile::new(&input).map(|(file, _)| file);
        (input, map)
    }

    fn match_pattern(path: &str, fname: &str) -> String {
        let rust_path = Path::new("test_data").join(format!("{}/main.rs", path));
        let molt_path = Path::new("test_data").join(format!("{}/{}.molt", path, fname));
        let input = Input::new(MoltSource::file(molt_path).unwrap())
            .with_rust_src_file(&rust_path)
            .unwrap();
        let diagnostics = super::run(&input, false);
        match diagnostics {
            Ok(diagnostics) => diagnostics
                .into_iter()
                .map(|diagnostic| emit_diagnostic_str(&input, diagnostic))
                .collect::<Vec<_>>()
                .join(""),
            Err(err) => emit_diagnostic_str(&input, make_error_diagnostic(&err)),
        }
    }

    macro_rules! test_match_pattern {
        ($dir_name: ident, ($($test_name: ident),* $(,)?)) => {
            mod $dir_name {
                mod match_ {
                    $(
                        #[test]
                        fn $test_name() {
                            insta::assert_snapshot!(super::super::match_pattern(stringify!($dir_name), stringify!($test_name)));
                        }
                    )*
                }

                #[test]
                fn parse() {
                    super::parse_rust(stringify!($dir_name));
                }
            }
        };
    }

    macro_rules! molt_test_err {
        ($dir_name: ident, ($($test_name: ident),* $(,)?)) => {
            $(
                #[test]
                fn $test_name() {
                    let path = format!(
                        "test_data/{}/{}.molt",
                        stringify!($dir_name),
                        stringify!($test_name)
                    );
                    let (input, result) = parse_molt(&path);
                    let err = match result {
                        Err(e) => e,
                        Ok(_) => panic!("No error during parsing when one was expected"),
                    };
                    insta::assert_snapshot!(emit_diagnostic_str(&input, make_error_diagnostic(&err)));
                }
            )*
        };
    }

    macro_rules! molt_test_ok {
        ($dir_name: ident, ($($test_name: ident),* $(,)?)) => {
            $(
                #[test]
                fn $test_name() {
                    let path = format!(
                        "test_data/{}/{}.molt",
                        stringify!($dir_name),
                        stringify!($test_name)
                    );
                    let (_, result) = parse_molt(&path);
                    match result {
                        Err(e) => panic!("Error during parsing when none was expected {}", e),
                        Ok(_) => {},
                    }
                }
            )*
        };
    }

    test_match_pattern!(consts, (exprs));
    test_match_pattern!(let_, (let_));
    test_match_pattern!(closure, (closure));
    test_match_pattern!(control_flow, ());
    test_match_pattern!(arrays, (array));
    test_match_pattern!(ranges, ());
    test_match_pattern!(expr, (expr, function_chain, stmts, nested_stmt,));
    test_match_pattern!(macros, (macros));
    test_match_pattern!(
        types,
        (
            types,
            nested_type,
            field_type,
            generic_default,
            const_param,
            predicate,
            pat_type,
        )
    );
    test_match_pattern!(fields, (fields, field_var));

    molt_test_err!(
        molt_grammar,
        (undefined_var, non_inferable_command, early_boundary_rule,)
    );

    molt_test_ok!(molt_grammar, (trailing_brace));
}
