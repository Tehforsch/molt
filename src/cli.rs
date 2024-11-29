use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    pub input_file: PathBuf,
    pub transform_file: PathBuf,
}
