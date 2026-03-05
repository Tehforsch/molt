#[macro_use]
mod parser;
mod cmp_syn;
mod config;
mod ctrl_c;
mod ctx;
mod error;
mod input;
mod match_ctx;
mod match_pattern;
mod modify;
mod molt_lang;
mod node;
mod node_list;
mod pattern;
pub mod rule;
mod rust_grammar;
mod span;
#[cfg(test)]
mod tests;
mod writer;

use std::path::{Path, PathBuf};

use crate::{rust_grammar::Node, writer::Writer};
pub use cmp_syn::CmpSyn;
use codespan_reporting::files::Files;
pub use config::Config;
pub use ctx::{Ctx, Id, NodeId, Var, VarDecl};
pub use error::{Error, emit_error};
pub use input::{Diagnostic, FileId, Input, MoltSource};
pub use match_ctx::{MatchCtx, MatchPatternData};
pub use match_pattern::{Binding, Match, Matcher, match_pattern, match_pattern2};
pub use node::{KindType, NodeType, ToNode};
pub use node_list::{
    List, ListMatchingMode, NoPunct, NodeList, PatNodeList, RealNodeList, SetMatchingMode, Single,
    SingleMatchingMode,
};
pub use pattern::Pattern;
pub use span::{Span, Spanned, SpannedPat, WithSpan};

struct RustFile;

struct SrcData<'src> {
    ctx: &'src Ctx<Node>,
    src: &'src str,
    file_id: FileId,
}

impl<'src> SrcData<'src> {
    fn print(&self, id: Id) {
        println!("{}", self.ctx.print(id, self.src))
    }
}

/// Used in the parsing logic and the AST context
/// to remember whether an item or variable belongs
/// to real source code or code within molt patterns.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Mode {
    /// Represents something within the real source code.
    Real,
    /// Represents something within a pattern in the molt code.
    Molt,
}

fn match_pattern_data<'a>(
    input: &'a Input,
    rust_file_id: crate::FileId,
    config: &'a Config,
) -> MatchPatternData<'a> {
    MatchPatternData {
        real_path: input.name(rust_file_id).unwrap().unwrap_path(),
        real_src: input.source(rust_file_id).unwrap(),
        molt_src: input.source(input.molt_file_id()).unwrap(),
        config,
    }
}

impl RustFile {
    fn new(input: &Input, file_id: FileId) -> Result<(Self, Ctx<Node>), Error> {
        let source = input.source(file_id).unwrap();
        crate::rust_grammar::parse_file(source, Mode::Real)
            .map_err(|e| Error::parse(e, file_id))
            .map(|(_, ctx)| (RustFile, ctx))
    }
}

pub fn run(
    input: &Input,
    config: crate::Config,
    _cargo_root: Option<&PathBuf>,
) -> Result<(), Error> {
    let molt_file = molt_lang::MoltFile::new(input)?;

    for rust_file_id in input.iter_rust_src() {
        let (_, real_ctx) = RustFile::new(input, rust_file_id)?;
        let data = match_pattern_data(input, rust_file_id, &config);
        let writer = Writer::new(input);
        let src = SrcData {
            ctx: &real_ctx,
            src: input.source(rust_file_id).unwrap(),
            file_id: rust_file_id,
        };
        crate::molt_lang::Interpreter::run(&molt_file, src, data, writer)
            .map_err(Error::Interpreter)?;
    }
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
