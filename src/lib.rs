use std::path::Path;

use ast::Ast;
use error::Error;
use quote::ToTokens;
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

pub fn dbgp(a: &impl ToTokens) {
    let s = quote::quote! { #a };
    dbg!(s.to_string());
}

pub fn run(path: &Path, spec_path: &Path) -> Result<(), Error> {
    println!("Checking {:?}", path);
    let ast = Ast::parse(path);
    let (ctx, spec) = FullSpec::from_path(spec_path)?;
    match &spec.command {
        Command::Transform(_, _) => {
            todo!()
        }
        Command::Match(pat_var) => {
            spec.spec
                .match_pattern(ast, ctx, spec.spec.find_node_for_var(pat_var));
        }
    };
    Ok(())
}

#[cfg(test)]
mod tests {
    // use std::path::Path;

    // use insta::assert_snapshot;

    // use crate::{ast::RustAst, FullSpec};

    // fn match_pattern(fname: &str) -> String {
    //     let path = Path::new("test_data").join(format!("{}.rs", fname));
    //     let spec_path = Path::new("test_data").join(format!("{}.molt", fname));
    //     let ast = RustAst::new(syn::parse_file(&std::fs::read_to_string(path).unwrap()).unwrap());
    //     let spec = FullSpec::from_path(&spec_path).unwrap();
    //     let var = match spec.command {
    //         crate::Command::Transform(_, _) => panic!("Transform command in file"),
    //         crate::Command::Match(var) => var,
    //     };
    //     let spec = spec.spec;
    //     let result = spec.match_pattern(ast, spec.find_var(&var));
    //     match result {
    //         Ok(_) => format!("Match"),
    //         Err(_) => format!("No match"),
    //     }
    // }

    // macro_rules! test_match_pattern {
    //     ($name: ident) => {
    //         #[test]
    //         fn $name() {
    //             assert_snapshot!(match_pattern(stringify!($name)));
    //         }
    //     };
    // }

    // test_match_pattern!(rename);
}
