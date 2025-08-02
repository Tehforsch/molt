use std::path::Path;

use similar::{ChangeTag, TextDiff};

use crate::modify::Modification;

#[derive(Clone)]
enum Color {
    Red,
    Green,
    Cyan,
    Bold,
    None,
}

impl Color {
    fn to_ansi(&self) -> &'static str {
        match self {
            Color::Red => "\x1b[31m",
            Color::Green => "\x1b[32m",
            Color::Cyan => "\x1b[36m",
            Color::Bold => "\x1b[1m",
            Color::None => "",
        }
    }
}

#[derive(Clone)]
struct ColoredText {
    text: String,
    color: Color,
}

impl ColoredText {
    fn new(text: String, color: Color) -> Self {
        Self { text, color }
    }

    fn print(&self, colorized: bool) -> String {
        if colorized && !matches!(self.color, Color::None) {
            const RESET: &str = "\x1b[0m";
            format!("{}{}{}", self.color.to_ansi(), self.text, RESET)
        } else {
            self.text.clone()
        }
    }
}

const NUM_LINES_CONTEXT: usize = 4;

impl Modification {
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
        print!("{diff_output}");
    }
}

fn format_diff(diff: TextDiff<str>, filename: &Path, colorized: bool) -> String {
    let mut output = String::new();

    let header = ColoredText::new(format!("--- {}\n", filename.to_string_lossy()), Color::Bold);
    output.push_str(&header.print(colorized));

    for group in diff.grouped_ops(NUM_LINES_CONTEXT).iter() {
        for op in group {
            for change in diff.iter_changes(op) {
                let (sign, color) = match change.tag() {
                    ChangeTag::Delete => ("-", Color::Red),
                    ChangeTag::Insert => ("+", Color::Green),
                    ChangeTag::Equal => (" ", Color::None),
                };

                // Get line numbers for old and new
                let old_line = change.old_index().map(|i| i + 1);
                let new_line = change.new_index().map(|i| i + 1);

                // Format line numbers
                let line_info = match (old_line, new_line) {
                    (Some(old), Some(new)) => format!("{old:4},{new:4}"),
                    (Some(old), None) => format!("{old:4},-   "),
                    (None, Some(new)) => format!("-   ,{new:4}"),
                    (None, None) => "    ,    ".to_string(),
                };

                let line_numbers = ColoredText::new(format!("{line_info} "), Color::Cyan);
                let change_text = ColoredText::new(format!("{sign}{change}"), color);

                output.push_str(&line_numbers.print(colorized));
                output.push_str(&change_text.print(colorized));
            }
        }
    }
    output.push('\n');
    output
}
