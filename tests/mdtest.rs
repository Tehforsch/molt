use std::path::Path;

use molt::{Config, Contents, Error, Input, Source, Writer};
use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag, TagEnd};

struct Section {
    name: String,
    code_blocks: Vec<CodeBlock>,
}

enum Lang {
    Molt,
    MoltError,
    Rust,
}

impl Lang {
    fn new(lang: &str) -> Option<Self> {
        match lang {
            "molt" => Some(Self::Molt),
            "molt error" => Some(Self::MoltError),
            "rust" => Some(Self::Rust),
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
    let mut current_section: Option<Section> = None;
    let mut in_heading = false;
    let mut in_code_block = false;
    let mut current_lang = String::new();
    let mut current_code = String::new();

    for event in parser {
        match event {
            Event::Start(Tag::Heading { .. }) => {
                if let Some(section) = current_section.take() {
                    sections.push(section);
                }
                in_heading = true;
                current_section = Some(Section {
                    name: String::new(),
                    code_blocks: Vec::new(),
                });
            }
            Event::End(TagEnd::Heading(..)) => {
                in_heading = false;
            }
            Event::Text(text) if in_heading => {
                if let Some(ref mut section) = current_section {
                    section.name.push_str(&text);
                }
            }
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
                in_code_block = true;
                current_lang = Lang::new(lang)
                    .unwrap_or_else(|| panic!("Unexpected lang in md block: {lang}"));
                current_code.clear();
            }
            Event::Text(text) if in_code_block => {
                current_code.push_str(&text);
            }
            Event::End(TagEnd::CodeBlock) => {
                in_code_block = false;
                if let Some(ref mut section) = current_section {
                    section.code_blocks.push(CodeBlock {
                        lang: current_lang.clone(),
                        content: current_code.clone(),
                    });
                }
            }
            _ => {}
        }
    }

    if let Some(section) = current_section {
        sections.push(section);
    }

    sections
}

fn filter_code_blocks(section: &Section, f: impl Fn(&str) -> bool) -> Vec<&CodeBlock> {
    section
        .code_blocks
        .iter()
        .filter(|block| f(&block.lang))
        .collect()
}

pub fn run_on_str(c: TestConfig, molt_src: &str, rust_sources: &[&str]) -> Result<String, Error> {
    let mut input = Input::new(Source::String(Contents::new(molt_src.to_string())));
    for src in rust_sources {
        input = input.with_rust_src(src.to_string())?;
    }
    let writer = Writer::buffer();
    // Intentionally ignore the error here for now, so I see parse errors.
    // Eventually, we should probably configure which tests are supposed
    // to error and which ones arent.
    let _ = molt::run(&input, &writer, Config::default(), None);
    Ok(writer.into_string().unwrap())
}

fn run_section(md_file: &Path, section: &Section) {
    let molt_blocks = filter_code_blocks(section, |lang| lang == "molt" || lang == "molt error");
    let rust_blocks = filter_code_blocks(section, |lang| lang == "rust");
    let unknown_blocks = filter_code_blocks(section, |lang| {
        lang != "rust" && lang != "molt" && lang != "molt error"
    });

    assert!(!section.code_blocks.is_empty());
    assert!(unknown_blocks.is_empty());
    assert_eq!(
        molt_blocks.len(),
        1,
        "{}: section {:?}: expected exactly one molt block, found {}",
        md_file.display(),
        section.name,
        molt_blocks.len()
    );

    let rust_sources: Vec<&str> = rust_blocks.iter().map(|b| b.content.as_str()).collect();
    let output = run_on_str(&molt_blocks[0].content, &rust_sources).unwrap_or_else(|e| {
        panic!(
            "{}: section {:?}: molt run failed: {e:?}",
            md_file.display(),
            section.name
        )
    });

    let snapshot_name = format!(
        "{}__{}",
        md_file.file_stem().unwrap().to_str().unwrap(),
        section.name.replace(' ', "_")
    );
    insta::with_settings!({
        snapshot_path => "snapshots",
    }, {
        insta::assert_snapshot!(snapshot_name, output);
    });
}

fn mdtest(path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let content = std::fs::read_to_string(path)?;
    let sections = parse_markdown(&content);

    for section in &sections {
        run_section(path, section);
    }
    Ok(())
}

datatest_stable::harness!(mdtest, "markdown_tests/", r"\.md$");
