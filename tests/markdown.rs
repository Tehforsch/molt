use std::path::Path;

use molt::{Config, Contents, Error, Input, Source, Writer, emit_error};
use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag, TagEnd};

struct Section {
    name: Option<String>,
    code_blocks: Vec<CodeBlock>,
}

#[derive(Clone, Copy)]
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

fn filter_code_blocks(section: &Section, f: impl Fn(Lang) -> bool) -> Vec<&CodeBlock> {
    section
        .code_blocks
        .iter()
        .filter(|block| f(block.lang))
        .collect()
}

fn run_on_str(c: TestConfig, molt_src: &str, rust_sources: &[&str]) -> Result<String, Error> {
    let mut input = Input::new(Source::String(Contents::new(molt_src.to_string())));
    for src in rust_sources {
        input = input.with_rust_src(src.to_string())?;
    }
    let writer = Writer::buffer();
    if c.should_error {
        // If an error is expected, ignore the result since any error will be
        // written to the `writer` and therefore appear in the output so we can
        // test against it.
        let _ = molt::run(&input, &writer, Config::default(), None);
    } else {
        let result = molt::run(&input, &writer, Config::default(), None);
        // Otherwise, emit the error to stderr
        if result.is_err() {
            emit_error(&Writer::default(), &input, result).unwrap();
        }
    }
    Ok(writer.into_string().unwrap())
}

fn run_section(md_file: &Path, section: &Section) {
    let molt_blocks =
        filter_code_blocks(section, |lang| matches!(lang, Lang::Molt | Lang::MoltError));
    let rust_blocks = filter_code_blocks(section, |lang| matches!(lang, Lang::Rust));

    assert!(!section.code_blocks.is_empty());
    assert_eq!(
        molt_blocks.len(),
        1,
        "{}: section {:?}: expected exactly one molt block, found {}",
        md_file.display(),
        section.name,
        molt_blocks.len()
    );

    let rust_sources: Vec<&str> = rust_blocks.iter().map(|b| b.content.as_str()).collect();
    let should_error = match &molt_blocks[0].lang {
        Lang::Molt => false,
        Lang::MoltError => true,
        Lang::Rust => unreachable!(),
    };
    let config = TestConfig { should_error };
    let output = run_on_str(config, &molt_blocks[0].content, &rust_sources).unwrap_or_else(|e| {
        panic!(
            "{}: section {:?}: molt run failed: {e:?}",
            md_file.display(),
            section.name
        )
    });

    let file_stem = md_file.file_stem().unwrap().to_str().unwrap();
    let snapshot_name = match &section.name {
        Some(name) => format!("{file_stem}__{}", name.replace(' ', "_")),
        None => file_stem.to_string(),
    };
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
