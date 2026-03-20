#![allow(unused)]
mod diff;

use std::collections::HashMap;
use std::io::{self, Write};
use std::path::Path;

use crate::change_buffer::ChangeBuffer;
use crate::input::FilePath;
use crate::molt_lang::{Context, PatId, PatVar};
use crate::rust_grammar::Node;
use crate::{Config, Id, Match, Span, Var};
use codespan_reporting::files::Files;

use crate::ctrl_c::FileRestorer;
use crate::{Error, FileId, Input};

#[derive(Debug, thiserror::Error)]
pub enum ModifyError {
    #[error("Overlapping spans.")]
    Overlap,
}

pub type Result<T, E = ModifyError> = std::result::Result<T, E>;

#[derive(Default)]
pub(crate) struct ModMap {
    mods: Vec<Modification>,
}

impl ModMap {
    fn iter(&self) -> impl Iterator<Item = &Modification> {
        self.mods.iter()
    }

    pub(crate) fn extend(&mut self, modifications: Vec<Modification>) {
        self.mods.extend(modifications);
    }
}

#[derive(Debug)]
pub struct Modification {
    pub old: NodeSpec,
    pub new: NodeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeSpec {
    Real(Id), // Refers to a node in the real file
    Molt {
        id: Id,
        pat: PatId,
        vars: Vec<NodeSpec>,
    }, // Refers to a node in the molt file
}

#[derive(Clone)]
pub struct FileModificationResult {
    pub new_code: ChangeBuffer,
    pub num_modifications: usize,
}

pub struct Modify<'a> {
    code: ChangeBuffer,
    rust_file_path: FilePath<'a>,
    cargo_root: Option<&'a Path>,
    ctx: Context<'a>,
}

impl<'a> Modify<'a> {
    pub fn run(
        ctx: Context<'a>,
        rust_file_id: FileId,
        cargo_root: Option<&'a Path>,
        modifications: ModMap,
    ) -> Result<FileModificationResult, Error> {
        let code = ctx.input.source(rust_file_id).unwrap().to_owned();
        let filename = ctx.input.name(rust_file_id).unwrap();
        let mut modify = Self {
            code: ChangeBuffer::new(code),
            ctx,
            rust_file_path: filename,
            cargo_root,
        };
        let num_modifications = modifications.mods.len();
        for m in modifications.mods.into_iter() {
            modify.apply(m)?;
        }
        Ok(FileModificationResult {
            new_code: modify.code,
            num_modifications,
        })
    }

    fn apply(&mut self, m: Modification) -> Result<()> {
        assert!(matches!(m.old, NodeSpec::Real(_))); // TODO: resolve chain if not the case
        let span = self.ctx.get_span(&m.old);
        self.code.make_change(span, &self.get_modified_code(&m.new));
        Ok(())
    }

    fn get_modified_code(&self, new: &NodeSpec) -> String {
        self.ctx.print(new)
    }
}
