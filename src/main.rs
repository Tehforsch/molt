mod cli;

use cargo::{core::Workspace, sources::PathSource, GlobalContext};
use clap::Parser;
use cli::CliArgs;
use molt::run;
use std::{
    error::Error,
    path::{Path, PathBuf},
};

fn source_files(input_file: &Path) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    if std::fs::metadata(input_file)?.is_dir() {
        let cargo_toml = input_file.join("Cargo.toml");
        Ok(get_cargo_source_files(&cargo_toml)?)
    } else {
        Ok(vec![input_file.to_path_buf()])
    }
}

fn get_cargo_source_files(path: &Path) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let ctx = GlobalContext::default()?;
    let ws = Workspace::new(&path, &ctx)?;

    let mut src_files = vec![];

    for pkg in ws.members() {
        let src = PathSource::new(pkg.root(), pkg.package_id().source_id(), &ctx);
        src_files.extend(
            src.list_files(pkg)?
                .into_iter()
                .map(|entry| entry.to_path_buf())
                .filter(|path| path.extension().map(|ext| ext == "rs").unwrap_or(false)),
        );
    }

    Ok(src_files)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = CliArgs::parse();

    let source_files = source_files(&args.input_file)?;
    for file in source_files {
        run(&file, &args.transform_file)?;
        // println!("{}", output);
    }

    Ok(())
}
