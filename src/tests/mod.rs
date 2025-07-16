mod download;

use std::path::Path;
use std::process::{self, Stdio};

use crate::Config;

use crate::error::{emit_diagnostic_str, make_error_diagnostic};
use crate::input::{Contents, Input, MoltSource};
use crate::lsp::LspClient;
use crate::transform::Transform;
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
    // This is super hacky, but it keeps the tests involving
    // rust-analyzer from polluting the test dir.
    unsafe {
        std::env::set_var("CARGO_BUILD_TARGET_DIR", std::env::temp_dir());
    }
    let input = make_input(path, fname);
    let diagnostics = super::run(&input, Config::test(), None);
    match diagnostics {
        Ok(diagnostics) => diagnostics
            .into_iter()
            .map(|diagnostic| emit_diagnostic_str(&input, diagnostic))
            .collect::<Vec<_>>()
            .join(""),
        Err(err) => emit_diagnostic_str(&input, make_error_diagnostic(&err)),
    }
}

fn format_code(code: String) -> Result<String, Error> {
    let mut cmd = process::Command::new("rustfmt")
        .arg("--emit=stdout")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    if let Some(stdin) = cmd.stdin.take() {
        use std::io::Write;
        let mut stdin = stdin;
        stdin.write_all(code.as_bytes())?;
    }

    let output = cmd.wait_with_output()?;

    if !output.status.success() {
        // If rustfmt fails, return the original code
        eprintln!(
            "rustfmt failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        return Ok(code);
    }

    Ok(String::from_utf8(output.stdout).unwrap_or(code))
}

fn run_transform_with<T>(path: &str, fname: &str, f: impl FnOnce(Transform) -> T) -> T {
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

    let transform = Transform::new(
        &input,
        &config,
        rust_file_id,
        None,
        match_result,
        &tr.transforms,
    )
    .unwrap();
    f(transform)
}

fn transform(path: &str, fname: &str) -> String {
    run_transform_with(path, fname, |transform| {
        let code = transform.get_transformed_contents().unwrap();
        format_code(code).unwrap()
    })
}

fn transform_diff(path: &str, fname: &str) -> String {
    run_transform_with(path, fname, |transform| {
        transform.get_diff_output().unwrap()
    })
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

            mod diff {
                $(
                    #[test]
                    fn $test_name() {
                        insta::assert_snapshot!(super::super::transform_diff(stringify!($dir_name), stringify!($test_name)));
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
test_match_pattern!(closure, (closure, inputs));
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
test_match_pattern!(
    pat,
    (
        pat,
        tuple,
        struct_,
        tuple_struct,
        slice,
        reference,
        reference_mut,
        or,
        paren,
        subpattern,
        nested,
        tuple_list,
        slice_list,
    )
);
test_transform!(transform, (rename, inner));
test_match_pattern!(transform2, (example10)); // TODO: Make this a transform test once the functionality exists.

test_match_pattern!(array_elements, (elements));
test_match_pattern!(function_args, (fn_));
test_match_pattern!(tuple_elements, (elements));
test_match_pattern!(tuple_types, (elements));
test_match_pattern!(struct_fields, (named, unnamed));
test_match_pattern!(attrs, (expr));
test_match_pattern!(literals, (expr_lit, match_pat, lit));
test_match_pattern!(
    visibility,
    (
        fn_, const_, enum_, field, module, static_, struct_, trait_, type_alias, rule_mod_1,
        rule_mod_2, rule_mod_3,
    )
);
test_match_pattern!(fns, (fns, rule, unsafe_rule, async_rule, const_rule));

molt_grammar_test_err!(
    molt_grammar,
    (
        undefined_var,
        non_inferable_command,
        early_boundary_rule,
        syntax_error1,
        syntax_error2,
        syntax_error3,
        syntax_error4,
    )
);

molt_grammar_test_ok!(molt_grammar, (trailing_brace, unnecessary_semicolon));
