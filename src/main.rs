mod cli;

use std::io;
use std::path::{Path, PathBuf};

use clap::Parser;
use cli::CliArgs;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, Config};
use molt::{Input, MoltSource, emit_error, run};
use walkdir::WalkDir;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("{0}")]
    Internal(#[from] molt::Error),
    #[error("{0}")]
    Io(#[from] io::Error),
    #[error("{0}")]
    Walkdir(#[from] walkdir::Error),
    #[error("No Cargo.toml found.")]
    CargoTomlNotFound,
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub(crate) fn get_cargo_toml(mut path: PathBuf) -> Result<PathBuf> {
    loop {
        let cargo_toml = path.join("Cargo.toml").canonicalize()?;
        if cargo_toml.is_file() {
            return Ok(cargo_toml);
        }
        if !path.pop() {
            break;
        }
    }
    Err(Error::CargoTomlNotFound)
}

fn get_cargo_source_files(path: &Path) -> Result<Vec<PathBuf>> {
    let mut src_files = vec![];
    for entry in WalkDir::new(path.parent().unwrap()) {
        let entry = entry?;
        let path = entry.path();
        let is_rust_src = path.extension().is_some_and(|ext| ext == "rs");
        if is_rust_src {
            src_files.push(path.to_owned());
        }
    }
    Ok(src_files)
}

fn main() -> Result<()> {
    let args = CliArgs::parse();
    let from_cargo = |path| {
        let cargo_toml = get_cargo_toml(path)?;
        let root = cargo_toml.parent().unwrap();
        Ok::<_, Error>((Some(root.to_owned()), get_cargo_source_files(&cargo_toml)?))
    };
    let (root, source_files) = if let Some(path) = args.input_file {
        if path.is_dir() {
            from_cargo(path)?
        } else {
            (None, vec![path.to_owned()])
        }
    } else {
        from_cargo(std::env::current_dir()?)?
    };
    let mut input = Input::new(MoltSource::file(&args.transform_file).unwrap())
        .with_rust_src_files(source_files.iter())
        .unwrap();
    if let Some(root) = root {
        input = input.with_root(root);
    }
    let config = molt_lib::Config::default();
    let diagnostics = emit_error(&input, run(&input, config))?;
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();
    for diagnostic in diagnostics {
        term::emit(&mut writer.lock(), &config, &input, &diagnostic).unwrap();
    }
    Ok(())
}
