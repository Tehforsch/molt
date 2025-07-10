pub struct Position {
    pub line: u32,
    pub character: u32,
}

/// Get the line and column position from a byte offset in source code
pub fn get_position_from_byte_offset(source: &str, byte_index: usize) -> Position {
    let line_starts: Vec<usize> = crate::input::line_starts(source).collect();
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
