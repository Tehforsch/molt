#![allow(unused)]
mod diff;

use std::collections::HashMap;
use std::io::{self, Write};
use std::path::Path;

use crate::input::FilePath;
use crate::molt_lang::{Context, PatId};
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

#[derive(Debug)]
pub struct Change {
    pub new_code: String,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum NodeSpec {
    Real(Id),                    // Refers to a node in the real file
    Molt { id: Id, pat: PatId }, // Refers to a node in the molt file
}

pub struct FileModificationResult {
    pub new_code: String,
    pub num_modifications: usize,
}

pub struct Modify<'a> {
    code: String,
    rust_file_path: FilePath<'a>,
    cargo_root: Option<&'a Path>,
    ctx: Context<'a>,

    changes: Vec<Change>,
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
            code,
            ctx,
            rust_file_path: filename,
            cargo_root,
            changes: vec![],
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
        let change = self.get_change(m)?;
        let range = change.span.byte_range();
        self.code.replace_range(range, &change.new_code);
        self.changes.push(change);
        Ok(())
    }

    fn get_change(&self, m: Modification) -> Result<Change> {
        assert!(matches!(m.old, NodeSpec::Real(_))); // TODO: resolve chain if not the case
        let span = self.ctx.get_span(m.old);
        let span = self.translate_span(span);
        Ok(Change {
            span,
            new_code: self.get_modified_code(m.new),
        })
    }

    fn get_modified_code(&self, new: NodeSpec) -> String {
        let mut code = self.ctx.print(new);
        while let Some(var) = self.contained_variables(new).next() {
            self.replace_first_variable(var);
        }
        code
    }

    fn contained_variables(&self, new: NodeSpec) -> impl Iterator<Item = &Var<Node>> {
        vec![].into_iter() // TODO obviously
    }

    fn replace_first_variable(&self, var: &Var<Node>) {
        todo!()
    }

    fn translate_span(&self, span: Span) -> Span {
        let start = span.byte_range().start;
        let end = span.byte_range().end;
        Span::new(
            self.translate_byte_position(start),
            self.translate_byte_position(end),
        )
    }

    fn translate_byte_position(&self, pos: usize) -> usize {
        let mut pos = pos;
        for change in &self.changes {
            let range = change.span.byte_range();
            let old_len = range.end - range.start;
            let new_len = change.new_code.len();
            if pos >= range.end {
                pos = pos - old_len + new_len;
            }
        }
        pos
    }
}
