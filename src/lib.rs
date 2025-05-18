use std::path::Path;

use quote::ToTokens;
use rust_ast::RustAst;
use transformation::Transformation;

mod convert;
mod error;
mod grammar;
mod mangle;
mod parser;
mod rust_ast;
mod transform;
mod transformation;

pub fn dbgp(a: &impl ToTokens) {
    let s = quote::quote! { #a };
    dbg!(s.to_string());
}

pub fn apply_transform(path: &Path, transform_path: &Path) -> Result<String, ()> {
    let ast = RustAst::new(syn::parse_file(&std::fs::read_to_string(path).unwrap()).unwrap());
    let tf = Transformation::from_path(transform_path)?;
    let ast = tf.transform(ast);
    Ok(ast.dump())
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use insta::assert_snapshot;

    fn apply_transform(fname: &str) -> String {
        let path = Path::new("test_data").join(format!("{}.rs", fname));
        let transform_path = Path::new("test_data").join(format!("{}.transform", fname));
        super::apply_transform(&path, &transform_path).unwrap()
    }

    macro_rules! make_test {
        ($name: ident) => {
            #[test]
            fn $name() {
                assert_snapshot!(apply_transform(stringify!($name)));
            }
        };
    }

    make_test!(rename);
}
