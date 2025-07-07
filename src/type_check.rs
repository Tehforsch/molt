use crate::lsp;
use crate::molt_grammar::TypeAnnotation;
use molt_lib::{Ctx, MatchRe, NodeId};
use rust_grammar::{Node, Type};
use std::path::Path;

pub struct LspType {
    pub ctx: Ctx<Node>,
    pub src: String,
    pub type_: NodeId<Type>,
}

pub fn check_type_annotations(
    type_annotations: &[TypeAnnotation],
    ast_ctx: &molt_lib::Ctx<Node>,
    pat_ctx: &molt_lib::Ctx<Node>,
    rust_src: &str,
    molt_src: &str,
    rust_path: &Path,
    match_re: &MatchRe,
) -> bool {
    for type_annotation in type_annotations {
        let var_id = pat_ctx.get_id_by_name(&type_annotation.var_name);
        let binding = match_re.get_binding(var_id);
        if let Some(ast_node) = binding.ast.first() {
            let span = ast_ctx.get_span(*ast_node);
            let (line, column) = get_line_column(rust_src, span.byte_range());
            let type_ = query_type_at_position(&rust_path, line, column).unwrap();
            match type_ {
                Some(type_) => {
                    if !compare_types(
                        &type_.ctx,
                        pat_ctx,
                        type_.type_,
                        type_annotation.type_,
                        &type_.src,
                        molt_src,
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

fn get_line_column(rust_src: &str, byte_range: std::ops::Range<usize>) -> (u32, u32) {
    let line_starts: Vec<usize> = crate::input::line_starts(rust_src).collect();
    let byte_index = byte_range.start;
    let line = line_starts
        .binary_search(&byte_index)
        .unwrap_or_else(|next_line| next_line - 1);
    let line_start = line_starts[line];
    let column = byte_index - line_start;
    let (line, column) = (line as u32, column as u32);
    (line, column)
}

pub fn query_type_at_position(
    file_path: &Path,
    line: u32,
    character: u32,
) -> Result<Option<LspType>, Box<dyn std::error::Error>> {
    let mut lsp_client = lsp::LspClient::new()?;

    let current_dir = std::env::current_dir()?;

    lsp_client.initialize(&current_dir)?;

    let content = std::fs::read_to_string(file_path)?;
    lsp_client.did_open(file_path, &content)?;
    lsp_client.get_type_at_position(file_path, line, character)
}
