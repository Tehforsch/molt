use std::path::Path;

use molt::{Config, Contents, Error, Input, Source, Writer, emit_error};
use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag, TagEnd};
use similar::{ChangeTag, TextDiff};
use std::fmt::Write;

struct Section {
    name: Option<String>,
    code_blocks: Vec<CodeBlock>,
}

#[derive(Clone, Copy)]
enum Lang {
    Molt,
    MoltError,
    Rust,
    RustReference,
    Output,
}

impl Lang {
    fn new(lang: &str) -> Option<Self> {
        match lang {
            "molt" => Some(Self::Molt),
            "molt error" => Some(Self::MoltError),
            "rust" => Some(Self::Rust),
            "rust reference" => Some(Self::RustReference),
            "output" => Some(Self::Output),
            _ => None,
        }
    }
}

struct CodeBlock {
    lang: Lang,
    content: String,
}

struct TestConfig {
    should_error: bool,
}

fn parse_markdown(content: &str) -> Vec<Section> {
    let parser = Parser::new(content);
    let mut sections = Vec::new();
    let mut current_section = Section {
        name: None,
        code_blocks: Vec::new(),
    };
    let mut in_heading = false;
    let mut in_code_block = false;
    let mut current_lang = String::new();
    let mut current_code = String::new();
    let mut heading_name = String::new();

    for event in parser {
        match event {
            Event::Start(Tag::Heading { .. }) => {
                if !current_section.code_blocks.is_empty() {
                    sections.push(current_section);
                    current_section = Section {
                        name: None,
                        code_blocks: Vec::new(),
                    };
                }
                in_heading = true;
                heading_name.clear();
            }
            Event::End(TagEnd::Heading(..)) => {
                in_heading = false;
                current_section.name = Some(heading_name.clone());
            }
            Event::Text(text) if in_heading => {
                heading_name.push_str(&text);
            }
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
                in_code_block = true;
                current_lang = lang.to_string();
                current_code.clear();
            }
            Event::Text(text) if in_code_block => {
                current_code.push_str(&text);
            }
            Event::End(TagEnd::CodeBlock) => {
                in_code_block = false;
                current_section.code_blocks.push(CodeBlock {
                    lang: Lang::new(&current_lang)
                        .unwrap_or_else(|| panic!("Unexpected lang in md block: {current_lang}")),
                    content: current_code.clone(),
                });
            }
            _ => {}
        }
    }

    if !current_section.code_blocks.is_empty() {
        sections.push(current_section);
    }

    sections
}

fn colored_diff(expected: &str, actual: &str) -> String {
    let diff = TextDiff::from_lines(expected, actual);
    let mut out = String::new();
    writeln!(out, "\x1b[1m--- expected\x1b[0m").unwrap();
    writeln!(out, "\x1b[1m+++ actual\x1b[0m").unwrap();
    for group in diff.grouped_ops(3) {
        for op in &group {
            for change in diff.iter_changes(op) {
                let (sign, color) = match change.tag() {
                    ChangeTag::Delete => ('-', "\x1b[31m"),
                    ChangeTag::Insert => ('+', "\x1b[32m"),
                    ChangeTag::Equal => (' ', ""),
                };
                if color.is_empty() {
                    write!(out, " {change}").unwrap();
                } else {
                    write!(out, "{color}{sign}{change}\x1b[0m").unwrap();
                }
                if change.missing_newline() {
                    writeln!(out).unwrap();
                }
            }
        }
    }
    out
}

fn filter_code_blocks(section: &Section, f: impl Fn(Lang) -> bool) -> Vec<&CodeBlock> {
    section
        .code_blocks
        .iter()
        .filter(|block| f(block.lang))
        .collect()
}

fn run_on_str(
    md_file: &Path,
    section_name: Option<&str>,
    c: TestConfig,
    molt_src: &str,
    rust_sources: &[&CodeBlock],
) -> Result<String, Error> {
    let mut input = Input::new(Source::String(Contents::new(molt_src.to_string())));
    let (sources, references): (Vec<_>, Vec<_>) = rust_sources
        .iter()
        .partition::<Vec<&CodeBlock>, _>(|src| matches!(src.lang, Lang::Rust));
    for src in sources {
        input = input.with_rust_src(src.content.to_string())?;
    }
    let writer = Writer::buffer();
    if c.should_error {
        // If an error is expected, ignore the result since any error will be
        // written to the `writer` and therefore appear in the output so we can
        // test against it.
        let _ = molt::run(&input, &writer, Config::default(), None);
    } else {
        let result = molt::run(&input, &writer, Config::default(), None);
        match result {
            Err(_) => {
                let error_writer = Writer::buffer();
                let _ = emit_error(&error_writer, &input, result);
                let rendered = error_writer.into_string().unwrap();
                eprintln!("{rendered}");
                panic!(
                    "{}: section {:?}: molt run failed",
                    md_file.display(),
                    section_name,
                );
            }
            Ok(result) => {
                check_modifications(md_file, section_name, result, &references);
            }
        }
    }
    Ok(writer.into_string().unwrap())
}

fn check_modifications(
    md_file: &Path,
    section_name: Option<&str>,
    result: molt::RunResult,
    references: &[&CodeBlock],
) {
    if result.modifications_by_file.is_empty() {
        // Language test without any match/transformation
        // logic.
        return;
    }
    assert_eq!(result.modifications_by_file.len(), 1);
    let result = result.modifications_by_file.into_iter().next().unwrap().1;
    if references.is_empty() {
        assert_eq!(
            result.num_modifications, 0,
            "Modifications were performed, but none are specified in the test."
        );
    } else {
        assert_eq!(references.len(), 1);
        let expected = &references[0].content;
        let actual = result.new_code.code();
        if actual != *expected {
            eprintln!("{}", colored_diff(expected, &actual));
            panic!(
                "{}: section {:?}: transformation mismatch",
                md_file.display(),
                section_name,
            );
        }
    }
}

fn run_section(md_file: &Path, section: &Section) {
    let molt_blocks =
        filter_code_blocks(section, |lang| matches!(lang, Lang::Molt | Lang::MoltError));
    let rust_blocks = filter_code_blocks(section, |lang| {
        matches!(lang, Lang::Rust | Lang::RustReference)
    });
    let output_blocks = filter_code_blocks(section, |lang| matches!(lang, Lang::Output));

    assert!(!section.code_blocks.is_empty());
    assert_eq!(
        molt_blocks.len(),
        1,
        "{}: section {:?}: expected exactly one molt block, found {}",
        md_file.display(),
        section.name,
        molt_blocks.len()
    );
    assert!(
        output_blocks.len() <= 1,
        "{}: section {:?}: expected at most one output block, found {}",
        md_file.display(),
        section.name,
        output_blocks.len()
    );

    let should_error = match &molt_blocks[0].lang {
        Lang::Molt => false,
        Lang::MoltError => true,
        Lang::Rust | Lang::RustReference | Lang::Output => unreachable!(),
    };
    let section_name = section.name.as_deref();
    let config = TestConfig { should_error };
    let output = run_on_str(
        md_file,
        section_name,
        config,
        &molt_blocks[0].content,
        &rust_blocks,
    )
    .unwrap_or_else(|e| {
        panic!(
            "{}: section {:?}: failed to set up test: {e:?}",
            md_file.display(),
            section_name
        )
    });

    let expected = match output_blocks.first() {
        Some(block) => block.content.trim_end(),
        None => "",
    };
    let output = output.trim_end();
    if output != expected {
        eprintln!("{}", colored_diff(expected, output));
        panic!(
            "{}: section {:?}: output mismatch",
            md_file.display(),
            section_name,
        );
    }
}

fn mdtest(path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let content = std::fs::read_to_string(path)?;
    let sections = parse_markdown(&content);

    for section in &sections {
        run_section(path, section);
    }
    Ok(())
}

datatest_stable::harness!(mdtest, "tests/markdown/", r"\.md$");
