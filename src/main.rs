mod cli;

use clap::Parser;
use cli::CliArgs;
use codespan_reporting::term::{
    self, Config,
    termcolor::{ColorChoice, StandardStream},
};
use molt::{Input, MoltSource, emit_error, run};
use std::io;
use std::path::{Path, PathBuf};
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
        let entry = entry.map_err(|e| e)?;
        let path = entry.path();
        let is_rust_src = path.extension().map_or(false, |ext| ext == "rs");
        if is_rust_src {
            src_files.push(path.to_owned());
        }
    }
    Ok(src_files)
}

fn main() -> Result<()> {
    let args = CliArgs::parse();
    let source_files = if let Some(path) = args.input_file {
        if path.is_dir() {
            get_cargo_source_files(&get_cargo_toml(path)?)?
        } else {
            vec![path.to_owned()]
        }
    } else {
        get_cargo_source_files(&get_cargo_toml(std::env::current_dir()?)?)?
    };
    let input = Input::new(MoltSource::file(&args.transform_file).unwrap())
        .with_rust_src_files(source_files.iter())
        .unwrap();
    let diagnostics = emit_error(&input, run(&input, args.debug_print))?;
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();
    for diagnostic in diagnostics {
        term::emit(&mut writer.lock(), &config, &input, &diagnostic).unwrap();
    }
    Ok(())
}
