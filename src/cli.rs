use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    #[clap(long)]
    pub input_file: Option<PathBuf>,
    pub transform_file: PathBuf,
    #[clap(short, long)]
    pub debug_print: bool,
    #[clap(short, long)]
    pub cargo_fmt: bool,
    #[clap(short, long)]
    pub interactive: bool,
    #[clap(long)]
    pub check: Option<String>,
}
