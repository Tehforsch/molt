#![allow(unused)]
mod diff;

use std::cmp::Reverse;
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

struct ModNode {
    old: RealNodeRef,
    new: NodeRef,
    span: Span,
    children: Vec<ModNode>,
}

fn build_mod_tree(mods: ModMap, ctx: &RuntimeCtx) -> Vec<ModNode> {
    let mut nodes: Vec<ModNode> = mods
        .mods
        .into_iter()
        .map(|(old, new)| {
            let span = ctx.get_span(&old);
            ModNode {
                old,
                new,
                span,
                children: vec![],
            }
        })
        .collect();

    nodes.sort_by_key(|n| Reverse(n.span.size()));

    let mut roots = Vec::new();
    for node in nodes {
        insert_into_tree(&mut roots, node);
    }
    roots
}

fn insert_into_tree(nodes: &mut Vec<ModNode>, new_node: ModNode) {
    for existing in nodes.iter_mut() {
        if existing.span.contains(&new_node.span) {
            insert_into_tree(&mut existing.children, new_node);
            return;
        }
    }
    nodes.push(new_node);
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
        let num_modifications = modifications.mods.len();
        let roots = build_mod_tree(modifications, &ctx);
        let mut modify = Self {
            code: ChangeBuffer::new(code),
            ctx,
            rust_file_path: filename,
            cargo_root,
        };
        for root in &roots {
            let new_code = modify.print_with_children(&root.new, &root.children);
            modify.code.make_change(root.span, &new_code);
        }
        Ok(FileModificationResult {
            new_code: modify.code,
            num_modifications,
        })
    }

    fn print_with_children(&self, node: &NodeRef, children: &[ModNode]) -> String {
        match node {
            NodeRef::Real(id) => {
                if children.is_empty() {
                    return self.ctx.real_ctx.print(*id, self.ctx.real_code()).into();
                }
                let span = self.ctx.real_ctx.get_span(*id);
                let mut buf = ChangeBuffer::new_subspan(self.ctx.real_code().into(), span);
                for child in children {
                    if span.contains(&child.span) {
                        let child_code = self.print_with_children(&child.new, &child.children);
                        buf.make_change(child.span, &child_code);
                    }
                }
                buf.code()
            }
            NodeRef::Molt { pat, id, vars } => {
                let pat_data = self.ctx.get_pat(*pat);
                let ctx = &pat_data.ctx;
                let mut output =
                    ChangeBuffer::new_subspan(self.ctx.molt_code().into(), ctx.get_span(*id));
                assert_eq!(vars.len(), pat_data.vars.len());
                for (var, token_var) in vars.iter().zip(pat_data.vars.iter()) {
                    let span = token_var.span;
                    let new_code = self.print_with_children(var, children);
                    output.make_change(span, &new_code);
                }
                output.code()
            }
            NodeRef::List(ids) => ids
                .iter()
                .map(|id| self.print_with_children(id, children))
                .collect::<Vec<String>>()
                .join(" "),
        }
    }
}
