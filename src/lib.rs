use std::path::Path;

use ast::Ast;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use error::Error;
use grammar::{CustomDebug, GetSpan};
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
            show_matches(ast.file, spec.spec.match_pattern(ast.ctx, ctx, pat_var));
        }
    };
    Ok(())
}

fn show_matches(file: SimpleFile<String, String>, result: match_pattern::MatchResult) {
    let ctx = result.ctx;
    for match_ in result.matches.iter() {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();
        // print main match
        let binding = match_.get_binding(&result.var);
        let node = ctx.get_node(binding.ast.unwrap()).unwrap();
        let span = node.get_span(&ctx);
        let mut diagnostic =
            Diagnostic::note()
                .with_message("Match")
                .with_labels(vec![
                    Label::primary((), span.byte_range()).with_message(&result.var)
                ]);
        for (key, binding) in match_.iter_bindings() {
            if key == &result.var {
                continue;
            }
            if let Some(node) = binding.ast.and_then(|value| ctx.get_node(value)) {
                diagnostic = diagnostic.with_note(format!("{} = {}", key, node.deb(&ctx)));
            }
        }
        term::emit(&mut writer.lock(), &config, &file, &diagnostic).unwrap();
    }
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
