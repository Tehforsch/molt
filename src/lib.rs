#![allow(private_interfaces)]
#[macro_use]
mod parser;
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
mod pattern;
pub(crate) mod rule;
mod rust_grammar;
mod span;
mod storage;
#[cfg(test)]
mod tests;
mod writer;

use std::path::{Path, PathBuf};

use crate::{molt_lang::Context, rust_grammar::Node};
use codespan_reporting::files::Files;

// TODO make the CmpSyn trait private
// and fix the resulting warnings.
pub use cmp_syn::CmpSyn;
pub use config::Config;
pub(crate) use ctx::{Ctx, Id, NodeId, Var};
pub use error::{Error, emit_error};
pub use input::{Contents, Input, Source};
pub(crate) use input::{Diagnostic, FileId};
pub(crate) use match_pattern::{Match, Matcher};
pub(crate) use node::{KindType, NodeType, ToNode};
pub(crate) use node_list::{
    List, ListMatchingMode, NodeList, PatNodeList, RealNodeList, Single, SingleMatchingMode,
};
pub(crate) use pattern::Pattern;
pub(crate) use span::{Span, Spanned, SpannedPat, WithSpan};
pub use writer::Writer;

struct RustFile;

/// Used in the parsing logic and the AST context
/// to remember whether an item or variable belongs
/// to real source code or code within molt patterns.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Mode {
    /// Represents something within the real source code.
    Real,
    /// Represents something within a pattern in the molt code.
    Molt,
}

impl RustFile {
    fn new(input: &Input, file_id: FileId) -> Result<(Self, Ctx<Node>), Error> {
        let source = input.source(file_id).unwrap();
        crate::rust_grammar::parse_file(source, Mode::Real)
            .map_err(|e| Error::parse(e, file_id))
            .map(|(_, ctx)| (RustFile, ctx))
    }
}

pub fn run_internal(
    input: &Input,
    writer: &Writer,
    config: crate::Config,
    _cargo_root: Option<&PathBuf>,
) -> Result<(), Error> {
    let molt_file = molt_lang::MoltFile::new(input)?;

    if input.iter_rust_src().count() == 0 {
        // Fake some context so we can run.
        // TODO: This whole thing is super ugly,
        // but language tests feel hard otherwise
        let rust_file_id = FileId::Rust(usize::MAX);
        let real_ctx = Ctx::new(Mode::Real);
        let context = Context {
            real_id: rust_file_id,
            molt_id: input.molt_file_id(),
            real_ctx: &real_ctx,
            input,
            writer,
            config: &config,
            pats: &molt_file.pats,
        };
        crate::molt_lang::Interpreter::run_dry(&molt_file, context).map_err(Error::Interpreter)?;
    } else {
        molt_file.check_has_main_fn_with_input()?;
        for rust_file_id in input.iter_rust_src() {
            let (_, real_ctx) = RustFile::new(input, rust_file_id)?;
            let context = Context {
                real_id: rust_file_id,
                molt_id: input.molt_file_id(),
                real_ctx: &real_ctx,
                input,
                writer,
                config: &config,
                pats: &molt_file.pats,
            };
            crate::molt_lang::Interpreter::run(&molt_file, context).map_err(Error::Interpreter)?;
        }
    }
    Ok(())
}

pub fn run(
    input: &Input,
    writer: &Writer,
    config: crate::Config,
    _cargo_root: Option<&PathBuf>,
) -> Result<(), Error> {
    emit_error(
        writer,
        input,
        run_internal(input, writer, config, _cargo_root),
    )
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
