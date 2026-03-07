mod cli;

use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use cli::CliArgs;
use ignore::WalkBuilder;
use molt::{Input, Source, Writer, run};

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("{0}")]
    Internal(#[from] molt::Error),
    #[error("{0}")]
    Io(#[from] io::Error),
    #[error("{0}")]
    Ignore(#[from] ignore::Error),
    #[error("No Cargo.toml found.")]
    CargoTomlNotFound,
    #[error("Unknown file type for input file: {0}")]
    UnknownInputFileType(String),
}

type Result<T, E = Error> = std::result::Result<T, E>;

fn get_cargo_toml(mut path: PathBuf) -> Result<PathBuf> {
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

fn get_rust_files_from_dir(dir: &Path) -> Result<Vec<PathBuf>> {
    let mut src_files = vec![];
    for entry in WalkBuilder::new(dir).build() {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
            src_files.push(path.to_owned());
        }
    }
    Ok(src_files)
}

fn get_rust_files_from_paths(paths: &[PathBuf]) -> Result<Vec<PathBuf>> {
    let mut src_files = vec![];
    for path in paths {
        if path.is_file() {
            if path.extension().is_some_and(|ext| ext == "rs") {
                src_files.push(path.to_owned());
            } else {
                return Err(Error::UnknownInputFileType(
                    path.to_string_lossy().to_string(),
                ));
            }
        } else if path.is_dir() {
            src_files.extend(get_rust_files_from_dir(path)?);
        }
    }
    Ok(src_files)
}

fn get_cargo_source_files(path: &Path) -> Result<Vec<PathBuf>> {
    get_rust_files_from_dir(path.parent().unwrap())
}

fn main() -> Result<ExitCode> {
    let args = CliArgs::parse();
    let from_cargo = |path| {
        let cargo_toml = get_cargo_toml(path)?;
        let root = cargo_toml.parent().unwrap();
        Ok::<_, Error>((Some(root.to_owned()), get_cargo_source_files(&cargo_toml)?))
    };
    let (root, source_files) = if args.input_files.is_empty() {
        from_cargo(std::env::current_dir()?)?
    } else {
        (None, get_rust_files_from_paths(&args.input_files)?)
    };
    let mut input = Input::new(Source::file(&args.molt_file).unwrap())
        .with_rust_src_files(source_files.iter())
        .unwrap();
    if let Some(ref root) = root {
        input = input.with_root(root.to_owned());
    }
    let config = molt::Config {
        debug_print: args.debug_print,
        cargo_fmt: args.cargo_fmt,
        interactive: args.interactive,
        check: args.check,
    };

    let writer = Writer::default();
    let result = run(&input, &writer, config, root.as_ref());
    if result.is_err() {
        Ok(ExitCode::FAILURE)
    } else {
        Ok(ExitCode::SUCCESS)
    }
}
