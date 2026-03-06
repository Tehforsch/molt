use std::path::{Path, PathBuf};

use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag, TagEnd};
use walkdir::WalkDir;

use crate::config::Config;
use crate::input::{Contents, Input, Source};
use crate::molt_lang::{Context, MoltFile};
use crate::writer::Writer;
use crate::{Error, RustFile, emit_error};

struct Section {
    name: String,
    code_blocks: Vec<CodeBlock>,
}

struct CodeBlock {
    lang: String,
    content: String,
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
                current_lang = lang.to_string();
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

struct MarkdownTestSuite {
    path: PathBuf,
}

impl MarkdownTestSuite {
    fn run(self) -> usize {
        let md_files: Vec<_> = WalkDir::new(&self.path)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.path().extension().is_some_and(|ext| ext == "md"))
            .map(|e| e.into_path())
            .collect();

        assert!(
            !md_files.is_empty(),
            "no markdown files found in {:?}",
            self.path
        );

        let mut num_sections = 0;
        for md_file in md_files {
            let content = std::fs::read_to_string(&md_file)
                .unwrap_or_else(|e| panic!("failed to read {}: {e}", md_file.display()));

            let sections = parse_markdown(&content);

            for section in &sections {
                if let Err(e) = self.run_section(&md_file, section) {
                    panic!(
                        "{}: section {:?}: molt run failed: {e:?}",
                        md_file.display(),
                        section.name
                    );
                }
                num_sections += 1;
            }
        }
        num_sections
    }

    fn filter_code_blocks(section: &Section, f: impl Fn(&str) -> bool) -> Vec<&CodeBlock> {
        section
            .code_blocks
            .iter()
            .filter(|block| f(&block.lang))
            .collect()
    }

    fn run_section(&self, md_file: &Path, section: &Section) -> Result<(), Error> {
        let molt_blocks = Self::filter_code_blocks(section, |lang| lang == "molt");
        let rust_blocks = Self::filter_code_blocks(section, |lang| lang == "rust");
        let unknown_blocks =
            Self::filter_code_blocks(section, |lang| lang != "rust" && lang != "molt");

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

        let mut input = Input::new(Source::String(Contents::new(
            molt_blocks[0].content.clone(),
        )));
        for block in rust_blocks.iter() {
            input = input.with_rust_src(block.content.clone()).unwrap();
        }
        let test_writer = Writer::buffer();

        // Parse and resolve molt file.
        let molt_file = emit_error(&test_writer, &input, MoltFile::new(&input))?;

        for rust_file_id in input.iter_rust_src() {
            let (_, real_ctx) = RustFile::new(&input, rust_file_id)?;
            let context = Context {
                real_id: rust_file_id,
                molt_id: input.molt_file_id(),
                real_ctx: &real_ctx,
                input: &input,
                writer: &test_writer,
                config: &Config::default(),
            };
            crate::molt_lang::Interpreter::run(&molt_file, context).map_err(Error::Interpreter)?;
        }

        let output = test_writer.into_string().unwrap();
        let snapshot_name = format!(
            "{}__{}",
            md_file.file_stem().unwrap().to_str().unwrap(),
            section.name.replace(' ', "_")
        );
        insta::assert_snapshot!(snapshot_name, output);
        Ok(())
    }
}

#[test]
fn markdown_suite() {
    let count = MarkdownTestSuite {
        path: "markdown_tests/".into(),
    }
    .run();
    println!("Ran {count} tests.");
    insta::assert_snapshot!(format!("Ran {count} tests."))
}
