#![allow(unused)]
mod diff;

use std::collections::HashMap;
use std::io::{self, Write};
use std::path::Path;

use crate::change_buffer::ChangeBuffer;
use crate::ctx::Mode;
use crate::input::FilePath;
use crate::molt_lang::{PatId, PatVar, RuntimeCtx};
use crate::rust_grammar::Node;
use crate::{Config, CtxVar, Match, RawNodeId, Span};
use codespan_reporting::files::Files;

use crate::ctrl_c::FileRestorer;
use crate::{Error, FileId, Input};

#[derive(Debug, thiserror::Error)]
pub enum ModifyError {
    #[error("Overlapping spans.")]
    Overlap,
}

pub type Result<T, E = ModifyError> = std::result::Result<T, E>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum RealNodeRef {
    /// Represents a node in the real file.
    Real(RawNodeId),
    /// Represents a list in the real file.
    List(Vec<RealNodeRef>),
}

impl RealNodeRef {
    pub(crate) fn from_target(old: NodeRef) -> RealNodeRef {
        match old {
            NodeRef::Real(id) => {
                assert_eq!(id.mode(), Mode::Real);
                Self::Real(id)
            }
            NodeRef::List(nodes) => {
                Self::List(nodes.into_iter().map(RealNodeRef::from_target).collect())
            }
            NodeRef::Molt { id, pat, vars } => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeRef {
    /// Represents a node in the real file.
    Real(RawNodeId),
    /// Represents a list.
    List(Vec<NodeRef>),
    /// Represents a node in the molt file.
    Molt {
        id: RawNodeId,
        pat: PatId,
        vars: Vec<NodeRef>,
    },
}

#[derive(Default)]
pub(crate) struct ModMap {
    mods: HashMap<RealNodeRef, NodeRef>,
}

impl ModMap {
    pub(crate) fn extend(&mut self, other: ModMap) {
        self.mods.extend(other.mods);
    }

    pub(crate) fn insert(&mut self, old: RealNodeRef, new: NodeRef) {
        self.mods.insert(old, new);
    }
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
    ctx: RuntimeCtx<'a>,
}

impl<'a> Modify<'a> {
    pub fn run(
        ctx: RuntimeCtx<'a>,
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
        for (src, dst) in modifications.mods.into_iter() {
            modify.apply(src, dst)?;
        }
        Ok(FileModificationResult {
            new_code: modify.code,
            num_modifications,
        })
    }

    fn apply(&mut self, old: RealNodeRef, new: NodeRef) -> Result<()> {
        assert!(matches!(old, RealNodeRef::Real(_))); // TODO: work with lists?
        let span = self.ctx.get_span(&old);
        self.code.make_change(span, &self.get_modified_code(&new));
        Ok(())
    }

    fn get_modified_code(&self, new: &NodeRef) -> String {
        self.ctx.print(new)
    }
}
