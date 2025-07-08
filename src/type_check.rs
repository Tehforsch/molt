use std::path::Path;

use lsp_types::{Position, Range};
use molt_lib::{Ctx, Match, MatchPatternData, NodeId};
use rust_grammar::{Node, Type};

use crate::lsp::LspClient;
use crate::molt_grammar::TypeAnnotation;

pub struct LspType {
    pub ctx: Ctx<Node>,
    pub src: String,
    pub type_: NodeId<Type>,
}

impl LspClient {
    pub fn check_type_annotations(
        &mut self,
        type_annotations: &[TypeAnnotation],
        ast_ctx: &molt_lib::Ctx<Node>,
        pat_ctx: &molt_lib::Ctx<Node>,
        data: &MatchPatternData,
        match_: &Match,
    ) -> bool {
        for type_annotation in type_annotations {
            let var_id = pat_ctx.get_id_by_name(&type_annotation.var_name);
            let binding = match_.get_binding(var_id);
            for ast_node in binding.ast.iter() {
                let span = ast_ctx.get_span(*ast_node);
                let range = get_range_from_span(data.rust_src, span);
                let type_ = loop {
                    match self.query_type(data.rust_path, range) {
                        Ok(type_) => break type_,
                        Err(e) if e.to_string().contains("content modified") => {
                            // Retry if content was modified
                            continue;
                        }
                        Err(e) => panic!("Failed to query type: {e}"),
                    }
                };
                match type_ {
                    Some(type_) => {
                        if !compare_types(
                            &type_.ctx,
                            pat_ctx,
                            type_.type_,
                            type_annotation.type_,
                            &type_.src,
                            data.molt_src,
                        ) {
                            return false;
                        }
                    }
                    None => return false,
                }
            }
        }
        true
    }

    fn query_type(
        &mut self,
        file_path: &Path,
        range: Range,
    ) -> Result<Option<LspType>, Box<dyn std::error::Error>> {
        loop {
            let content = std::fs::read_to_string(file_path)?;
            self.did_open(file_path, &content)?;
            let type_ = self.get_type_at_position(file_path, range);
            match type_ {
                Ok(type_) => return Ok(type_),
                Err(e) if e.to_string().contains("content modified") => {
                    continue;
                }
                Err(e) => return Err(e),
            }
        }
    }
}

fn get_range_from_span(rust_src: &str, span: molt_lib::Span) -> Range {
    let start = get_position_from_byte_offset(rust_src, span.byte_range().start);
    let end = get_position_from_byte_offset(rust_src, span.byte_range().end);
    Range { start, end }
}

fn get_position_from_byte_offset(rust_src: &str, byte_index: usize) -> Position {
    let line_starts: Vec<usize> = crate::input::line_starts(rust_src).collect();
    let line = line_starts
        .binary_search(&byte_index)
        .unwrap_or_else(|next_line| next_line - 1);
    let line_start = line_starts[line];
    let column = byte_index - line_start;
    Position {
        line: line as u32,
        character: column as u32,
    }
}

fn compare_types(
    ctx1: &Ctx<Node>,
    ctx2: &Ctx<Node>,
    ty1: NodeId<Type>,
    ty2: NodeId<Type>,
    src1: &str,
    src2: &str,
) -> bool {
    ctx1.print(ty1.into(), src1) == ctx2.print(ty2.into(), src2)
}
