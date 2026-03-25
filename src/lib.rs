#![allow(private_interfaces)]
#[macro_use]
mod parser;
mod change_buffer;
mod cmp_syn;
mod config;
mod ctrl_c;
mod ctx;
mod error;
mod input;
mod match_pattern;
mod modify;
mod molt_lang;
mod node;
mod node_list;
pub(crate) mod rule;
mod rust_grammar;
mod span;
mod storage;
mod term;
#[cfg(test)]
mod tests;
mod writer;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::{
    ctx::Mode,
    input::FilePath,
    modify::{FileModificationResult, Modify},
    molt_lang::RuntimeCtx,
    parser::parse::ParseResult,
    rust_grammar::Node,
};
use codespan_reporting::files::Files;

// TODO make the CmpSyn trait private
// and fix the resulting warnings.
pub use cmp_syn::CmpSyn;
pub use config::Config;
pub use error::{Error, emit_error};
pub use input::{Contents, Input, Source};
pub use writer::Writer;

pub(crate) use ctx::{Ctx, CtxVar, NodeId, RawNodeId};
pub(crate) use input::{Diagnostic, FileId};
pub(crate) use match_pattern::{Match, Matcher};
pub(crate) use modify::ModMap;
pub(crate) use node::{NodeType, ToNode};
pub(crate) use node_list::NodeList;
pub(crate) use span::{Span, Spanned, SpannedTerm, WithSpan};
pub(crate) use term::Term;

struct RustFile;

impl RustFile {
    fn new(input: &Input, file_id: FileId) -> Result<ParseResult<Self>, Error> {
        let source = input.source(file_id).unwrap();
        let x: ParseResult<RustFile> = crate::rust_grammar::parse_file(source, Mode::Real)
            .map_err(|e| Error::parse(e, file_id))?
            .map(|_| RustFile);
        Ok(x)
    }
}

pub struct RunResult {
    pub modifications_by_file: HashMap<FileId, FileModificationResult>,
}

pub fn run_internal(
    input: &Input,
    writer: &Writer,
    config: crate::Config,
    cargo_root: Option<&PathBuf>,
) -> Result<RunResult, Error> {
    let molt_file = molt_lang::MoltFile::new(input)?;
    let mut result = RunResult {
        modifications_by_file: HashMap::default(),
    };

    if input.iter_rust_src().count() == 0 {
        // Fake some context so we can run.
        // TODO: This whole thing is super ugly,
        // but language tests feel hard otherwise
        let rust_file_id = FileId::Rust(usize::MAX);
        let real_ctx = Ctx::new(Mode::Real);
        let context = RuntimeCtx {
            real_id: rust_file_id,
            molt_id: input.molt_file_id(),
            real_ctx: &real_ctx,
            input,
            writer,
            config: &config,
            pats: &molt_file.pats,
            type_defs: &molt_file.type_defs,
        };
        crate::molt_lang::Interpreter::run_dry(&molt_file, &context).map_err(Error::Interpreter)?;
    } else {
        molt_file.check_has_main_fn_with_input()?;
        for rust_file_id in input.iter_rust_src() {
            let parse_result = RustFile::new(input, rust_file_id)?;
            let context = RuntimeCtx {
                real_id: rust_file_id,
                molt_id: input.molt_file_id(),
                real_ctx: &parse_result.ctx,
                input,
                writer,
                config: &config,
                pats: &molt_file.pats,
                type_defs: &molt_file.type_defs,
            };
            let modifications = crate::molt_lang::Interpreter::run(&molt_file, context.clone())
                .map_err(Error::Interpreter)?;
            let new_code = Modify::run(
                context,
                rust_file_id,
                cargo_root.map(|path| path.as_path()),
                modifications,
            )?;
            result
                .modifications_by_file
                .insert(rust_file_id, new_code.clone());
            match input.name(rust_file_id).unwrap() {
                FilePath::Path(path) => {
                    write_to_file(path, new_code)?;
                }
                FilePath::FromString => {
                    // Test run
                }
            }
        }
    }
    if let Some(cargo_root) = cargo_root
        && !result.modifications_by_file.is_empty()
    {
        run_cargo_fmt(cargo_root)?;
    }
    Ok(result)
}

pub fn run(
    input: &Input,
    writer: &Writer,
    config: crate::Config,
    cargo_root: Option<&PathBuf>,
) -> Result<RunResult, Error> {
    emit_error(
        writer,
        input,
        run_internal(input, writer, config, cargo_root),
    )
}

fn write_to_file(path: &Path, modification: FileModificationResult) -> Result<(), Error> {
    std::fs::write(path, modification.new_code.code()).map_err(Error::Io)?;
    Ok(())
}

#[allow(unused)]
fn run_cargo_fmt(cargo_root: &Path) -> Result<(), Error> {
    let status = std::process::Command::new("cargo")
        .arg("fmt")
        .current_dir(cargo_root)
        .status()?;

    if !status.success() {
        return Err(Error::Misc(format!(
            "cargo fmt failed with exit code: {status}",
        )));
    }

    Ok(())
}
