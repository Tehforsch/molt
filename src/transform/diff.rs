use std::path::Path;

use similar::{ChangeTag, TextDiff};

use crate::transform::Transformation;

pub const NUM_LINES_CONTEXT: usize = 4;

impl Transformation {
    pub fn get_diff(&self, old_code: &str, filename: &Path, colorized: bool) -> String {
        let range = self.span.byte_range();
        let new_excerpt = &self.new_code;
        let mut new_code = old_code.to_string();
        new_code.replace_range(range, new_excerpt);

        let diff = TextDiff::from_lines(old_code, &new_code);
        format_diff(diff, filename, colorized)
    }

    pub fn show_diff(&self, old_code: &str, filename: &Path) {
        let diff_output = self.get_diff(old_code, filename, true);
        print!("{}", diff_output);
    }
}

pub fn format_diff(diff: TextDiff<str>, filename: &Path, colorized: bool) -> String {
    let mut output = String::new();

    if colorized {
        const BOLD: &str = "\x1b[1m";
        const CYAN: &str = "\x1b[36m";
        const RESET: &str = "\x1b[0m";
        output.push_str(&format!(
            "{}{}--- {}{}\n",
            BOLD,
            CYAN,
            filename.to_string_lossy(),
            RESET
        ));
    } else {
        output.push_str(&format!("--- {}\n", filename.to_string_lossy()));
    }

    for group in diff.grouped_ops(NUM_LINES_CONTEXT).iter() {
        for op in group {
            for change in diff.iter_changes(op) {
                let (sign, color) = if colorized {
                    match change.tag() {
                        ChangeTag::Delete => ("-", "\x1b[31m"),
                        ChangeTag::Insert => ("+", "\x1b[32m"),
                        ChangeTag::Equal => (" ", ""),
                    }
                } else {
                    match change.tag() {
                        ChangeTag::Delete => ("-", ""),
                        ChangeTag::Insert => ("+", ""),
                        ChangeTag::Equal => (" ", ""),
                    }
                };

                // Get line numbers for old and new
                let old_line = change.old_index().map(|i| i + 1);
                let new_line = change.new_index().map(|i| i + 1);

                // Format line numbers
                let line_info = match (old_line, new_line) {
                    (Some(old), Some(new)) => format!("{:4},{:4}", old, new),
                    (Some(old), None) => format!("{:4},-   ", old),
                    (None, Some(new)) => format!("-   ,{:4}", new),
                    (None, None) => "    ,    ".to_string(),
                };

                if colorized {
                    const CYAN: &str = "\x1b[36m";
                    const RESET: &str = "\x1b[0m";
                    if color.is_empty() {
                        output.push_str(&format!(
                            "{}{} {}{}{}",
                            CYAN, line_info, RESET, sign, change
                        ));
                    } else {
                        output.push_str(&format!(
                            "{}{} {}{}{}{}{}",
                            CYAN, line_info, RESET, color, sign, change, RESET
                        ));
                    }
                } else {
                    output.push_str(&format!("{} {}{}", line_info, sign, change));
                }
            }
        }
    }
    output.push('\n');
    output
}
