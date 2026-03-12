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
    Rust(Id),                       // Refers to a concrete node in the current rust file
    MoltPat { pat: PatId },         // Refers to a pattern in the molt file
    MoltVar { id: Id, pat: PatId }, // Refers to a variable in a pattern in the molt file
}

pub struct Modify<'a> {
    code: String,
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
    ) -> Result<(), Error> {
        let code = ctx.input.source(rust_file_id).unwrap().to_owned();
        let filename = ctx.input.name(rust_file_id).unwrap();
        let modify = Self {
            code,
            ctx,
            rust_file_path: filename,
            cargo_root,
        };
        for m in modifications.mods.into_iter() {
            modify.apply(m)?;
        }
        Ok(())
    }

    fn apply(&self, m: Modification) -> Result<()> {
        let change = self.get_change(m)?;
        Ok(())
    }

    fn get_change(&self, m: Modification) -> Result<Change> {
        assert!(matches!(m.old, NodeSpec::Rust(_))); // TODO: resolve chain if not the case
        let span = self.ctx.get_span(m.old);
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
}
