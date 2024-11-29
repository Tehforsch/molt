use std::path::Path;

use grammar::Ast;
use quote::ToTokens;
use transform::Transform;

mod grammar;
mod transform;

pub fn dbgp(a: &impl ToTokens) {
    let s = quote::quote! { #a };
    dbg!(s.to_string());
}

pub fn apply_transform(path: &Path, transform_path: &Path) -> String {
    let mut ast = Ast::new(syn::parse_file(&std::fs::read_to_string(path).unwrap()).unwrap());
    let transform = Transform::from_path(transform_path);
    let ast = ast.apply_transform(transform);
    ast.dump()
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use insta::assert_snapshot;

    fn apply_transform(fname: &str) -> String {
        let path = Path::new("tests").join(format!("{}.rs", fname));
        let transform_path = Path::new("tests").join(format!("{}.transform", fname));
        super::apply_transform(&path, &transform_path)
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
