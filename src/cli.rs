use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    #[clap(short, long)]
    pub input_file: Option<PathBuf>,
    pub transform_file: PathBuf,
    #[clap(short, long)]
    pub debug_print: bool,
}
