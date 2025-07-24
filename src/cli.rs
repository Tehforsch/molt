use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    pub transform_file: PathBuf,
    pub input_files: Vec<PathBuf>,
    #[clap(short, long)]
    pub debug_print: bool,
    #[clap(short, long)]
    pub cargo_fmt: bool,
    #[clap(short, long)]
    pub interactive: bool,
    #[clap(long)]
    pub check: Option<String>,
}
