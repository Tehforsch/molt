mod cli;

use clap::Parser;
use cli::CliArgs;
use molt::run;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let args = CliArgs::parse();

    run(&args.input_file, &args.transform_file)?;

    Ok(())
}
