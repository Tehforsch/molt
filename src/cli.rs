use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    #[clap(short, long)]
    pub input_file: Option<PathBuf>,
    pub transform_file: PathBuf,
}
