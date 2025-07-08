use std::ops::Range;
use std::path::{Path, PathBuf};

use codespan_reporting::files::{Error, Files};

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<FileId>;

pub struct Input {
    root: Option<PathBuf>,
    rust_src: Vec<RustSourceFile>,
    molt_src: MoltSource,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FileId {
    Rust(usize),
    Molt,
}

pub enum FilePath<'a> {
    Path(&'a Path),
    FromCli,
}

impl<'a> FilePath<'a> {
    pub fn unwrap_path(self) -> &'a Path {
        match self {
            FilePath::Path(path) => path,
            FilePath::FromCli => panic!("unwrap_path called on FromCli variant."),
        }
    }
}

pub struct Contents {
    contents: String,
    /// The starting byte indices in the source code.
    line_starts: Vec<usize>,
}

pub struct RustSourceFile(SourceFile);

pub enum MoltSource {
    FromCli(Contents),
    File(MoltSourceFile),
}

pub struct MoltSourceFile(SourceFile);

struct SourceFile {
    contents: Contents,
    path: PathBuf,
}

// Impls mostly taken from `codespan_reporting::SimpleFile`.
impl<'a> Files<'a> for Input {
    type FileId = FileId;

    type Name = FilePath<'a>;

    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, Error> {
        match id {
            FileId::Rust(idx) => Ok(FilePath::Path(&self.rust_src[idx].0.path)),
            FileId::Molt => Ok(match &self.molt_src {
                MoltSource::FromCli(_) => FilePath::FromCli,
                MoltSource::File(file) => FilePath::Path(&file.0.path),
            }),
        }
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, Error> {
        Ok(&self.get_contents(id).contents)
    }

    fn line_index(&self, id: FileId, byte_index: usize) -> Result<usize, Error> {
        let contents = self.get_contents(id);
        Ok(contents
            .line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(&self, id: FileId, line_index: usize) -> Result<Range<usize>, Error> {
        let contents = self.get_contents(id);
        let line_start = contents.line_start(line_index)?;

        let next_line_start = contents.line_start(line_index + 1)?;

        Ok(line_start..next_line_start)
    }
}

impl Input {
    pub fn new(molt_src: MoltSource) -> Self {
        Self {
            root: None,
            rust_src: vec![],
            molt_src,
        }
    }

    pub fn with_root(self, root: PathBuf) -> Self {
        Self {
            root: Some(root),
            rust_src: self.rust_src,
            molt_src: self.molt_src,
        }
    }
    pub fn with_rust_src_file<P: AsRef<Path>>(mut self, file: P) -> Result<Self, crate::Error> {
        self.rust_src.push(RustSourceFile::new(file)?);
        Ok(Self {
            molt_src: self.molt_src,
            rust_src: self.rust_src,
            root: self.root,
        })
    }

    pub fn with_rust_src_files<P: AsRef<Path>>(
        mut self,
        files: impl Iterator<Item = P>,
    ) -> Result<Self, crate::Error> {
        for file in files {
            self.rust_src.push(RustSourceFile::new(file)?);
        }
        Ok(Self {
            molt_src: self.molt_src,
            rust_src: self.rust_src,
            root: self.root,
        })
    }

    fn get_contents(&self, id: FileId) -> &Contents {
        match id {
            FileId::Rust(idx) => &self.rust_src[idx].0.contents,
            FileId::Molt => match &self.molt_src {
                MoltSource::FromCli(s) => s,
                MoltSource::File(file) => &file.0.contents,
            },
        }
    }

    pub(crate) fn molt_file_id(&self) -> FileId {
        FileId::Molt
    }

    pub(crate) fn iter_rust_src(&self) -> impl Iterator<Item = FileId> {
        self.rust_src
            .iter()
            .enumerate()
            .map(|(i, _)| FileId::Rust(i))
    }

    pub fn root(&self) -> Option<&PathBuf> {
        self.root.as_ref()
    }
}

impl<'a> std::fmt::Display for FilePath<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilePath::Path(path) => write!(f, "{:?}", path),
            FilePath::FromCli => write!(f, "CLI input"),
        }
    }
}

impl Contents {
    pub fn new(contents: String) -> Self {
        let line_starts = line_starts(&contents).collect();
        Self {
            contents,
            line_starts,
        }
    }

    /// Return the starting byte index of the line with the specified line index.
    /// Convenience method that already generates errors if necessary.
    fn line_start(&self, line_index: usize) -> Result<usize, Error> {
        use core::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => Ok(self
                .line_starts
                .get(line_index)
                .cloned()
                .expect("failed despite previous check")),
            Ordering::Equal => Ok(self.contents.len()),
            Ordering::Greater => Err(Error::LineTooLarge {
                given: line_index,
                max: self.line_starts.len() - 1,
            }),
        }
    }
}

pub fn line_starts(source: &str) -> impl '_ + Iterator<Item = usize> {
    core::iter::once(0).chain(source.match_indices('\n').map(|(i, _)| i + 1))
}

impl RustSourceFile {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, crate::Error> {
        Ok(RustSourceFile(SourceFile::from_path(path.as_ref())?))
    }
}

impl MoltSource {
    pub fn file<P: AsRef<Path>>(path: P) -> Result<Self, crate::Error> {
        Ok(MoltSource::File(MoltSourceFile(SourceFile::from_path(
            path.as_ref(),
        )?)))
    }
}

impl SourceFile {
    fn from_path(path: &Path) -> Result<SourceFile, crate::Error> {
        let source =
            std::fs::read_to_string(path).map_err(|e| crate::Error::Misc(e.to_string()))?;
        let source = Contents::new(source);
        Ok(Self {
            path: path.to_owned(),
            contents: source,
        })
    }
}
