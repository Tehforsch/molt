use syn::Ident;

use crate::{
    Error, Input, MoltFile,
    ctx::Ctx,
    error::{emit_diagnostic_str, make_error_diagnostic},
    parser::VarDecl,
};

use super::{Mode, Parse};

fn parse_molt<T: Parse>(code: &str) -> Result<(T, Ctx), Error> {
    super::parse_file::<T>(code, Mode::Molt)
}

fn parse_molt_ok<T: Parse + std::fmt::Debug>(code: &str) -> T {
    parse_molt(code).unwrap().0
}

fn parse_molt_err<T: Parse + std::fmt::Debug>(code: &str) -> String {
    let e = match parse_molt::<T>(code) {
        Ok(_) => panic!(),
        Err(e) => e,
    };
    let input = Input::fake_molt(code);
    emit_diagnostic_str(&input, make_error_diagnostic(input.molt_file_id(), &e))
}

macro_rules! parse_test_ok {
    ($name: ident, $ty: ty, $($code: literal$(,)?)*) => {
        #[test]
        fn $name() {
            $(
                insta::assert_debug_snapshot!(crate::parser::tests::parse_molt_ok::<$ty>(
                    $code
                ));
            )*
        }
    };
}

macro_rules! parse_test_err {
    ($name: ident, $ty: ty, $($code: literal$(,)?)*) => {
        #[test]
        fn $name() {
            $(
                insta::assert_snapshot!(crate::parser::tests::parse_molt_err::<$ty>(
                    $code
                ));
            )*
        }
    };
}

parse_test_ok!(var_decl, VarDecl, "let foo: Ident = { bar };");
parse_test_ok!(single_decl_file, MoltFile, "let foo: Ident = { bar };");
parse_test_ok!(
    multi_decl_file,
    MoltFile,
    "let foo: Ident; let foo: Ident = { bar };"
);
parse_test_ok!(single_decl_node, MoltFile, "let foo: Lit = { \"hi\" };");
