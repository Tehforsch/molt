mod error;
mod input;
mod lsp;
pub(crate) mod molt_grammar;
mod resolve;
mod transform;
mod type_check;

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

pub fn run(input: &Input, config: crate::Config) -> Result<Vec<Diagnostic>, Error> {
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
                transform::transform(input, rust_file_id, match_result, transforms)?;
            }
        };
    }
    Ok(diagnostics)
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use molt_lib::Config;

    use crate::error::{emit_diagnostic_str, make_error_diagnostic};
    use crate::input::{Contents, Input, MoltSource};
    use crate::lsp::LspClient;
    use crate::{Command, Error, MoltFile, RustFile, match_pattern_data};

    fn parse_rust(path: &str) {
        let root = Path::new("test_data").join(path);
        let rust_path = root.join("main.rs");
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

    fn make_input(path: &str, fname: &str) -> Input {
        let root = Path::new("test_data").join(path);
        let rust_path = root.join("main.rs");
        let molt_path = root.join(format!("{fname}.molt"));

        Input::new(MoltSource::file(molt_path).unwrap())
            .with_root(root)
            .with_rust_src_file(&rust_path)
            .unwrap()
    }

    fn match_pattern(path: &str, fname: &str) -> String {
        let input = make_input(path, fname);
        let diagnostics = super::run(&input, Config::default());
        match diagnostics {
            Ok(diagnostics) => diagnostics
                .into_iter()
                .map(|diagnostic| emit_diagnostic_str(&input, diagnostic))
                .collect::<Vec<_>>()
                .join(""),
            Err(err) => emit_diagnostic_str(&input, make_error_diagnostic(&err)),
        }
    }

    fn transform(path: &str, fname: &str) -> String {
        let input = make_input(path, fname);
        let (molt_file, pat_ctx) = MoltFile::new(&input).unwrap();
        let rust_file_id = input.iter_rust_src().next().unwrap();
        let (_, ast_ctx) = RustFile::new(&input, rust_file_id).unwrap();
        let config = Config::default();
        let Command::Transform(ref tr) = molt_file.command else {
            panic!()
        };

        let mut lsp_client = LspClient::uninitialized();

        let data = match_pattern_data(&input, rust_file_id, &config);

        let match_result = molt_file.match_pattern(
            &ast_ctx,
            &pat_ctx,
            tr.match_.unwrap(),
            data,
            &mut lsp_client,
        );
        crate::transform::get_transformed_contents(
            &input,
            rust_file_id,
            match_result,
            &tr.transforms,
        )
        .unwrap()
    }

    macro_rules! test_match_pattern {
        ($dir_name: ident, ($($test_name: ident),* $(,)?) $(,$add: literal)?) => {
            mod $dir_name {
                fn get_dir_name() -> String {
                    let name = stringify!($dir_name);
                    $(
                        let name = format!("{}{}", name, $add);
                    )?
                    name.to_string()
                }

                mod match_ {
                    $(
                        #[test]
                        fn $test_name() {
                            let name = super::get_dir_name();
                            insta::assert_snapshot!(super::super::match_pattern(&name, stringify!($test_name)));
                        }
                    )*
                }

                #[test]
                fn parse() {
                    let name = get_dir_name();
                    super::parse_rust(&name);
                }
            }
        };
    }

    macro_rules! test_transform {
        ($dir_name: ident, ($($test_name: ident),* $(,)?)) => {
            mod $dir_name {
                mod match_ {
                    $(
                        #[test]
                        fn $test_name() {
                            insta::assert_snapshot!(super::super::transform(stringify!($dir_name), stringify!($test_name)));
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

    macro_rules! molt_grammar_test_err {
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

    macro_rules! molt_grammar_test_ok {
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
    test_match_pattern!(type_annotations, (ident, expr), "/src");
    test_match_pattern!(
        expr,
        (
            expr,
            function_chain,
            stmts,
            nested_stmt,
            return_,
            array_len,
            assign_lhs,
            assign_rhs,
            await_,
            binary,
            call,
            cast,
            closure,
            field_access,
            for_loop,
            if_
        )
    );
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
            const_,
            impl_,
            static_,
            type_alias,
            trait_const,
            impl_const,
            impl_type,
            receiver,
            type_array,
            as_trait,
            type_slice,
            type_bare_fn,
        )
    );
    test_match_pattern!(fields, (fields, field_var));
    test_match_pattern!(lists, (single, all));
    test_transform!(transform, (rename, example10));

    molt_grammar_test_err!(
        molt_grammar,
        (undefined_var, non_inferable_command, early_boundary_rule)
    );

    molt_grammar_test_ok!(molt_grammar, (trailing_brace, unnecessary_semicolon));
}
