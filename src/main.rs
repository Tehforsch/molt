mod cli;

use ast::apply_transform;
use clap::Parser;
use cli::CliArgs;

fn main() -> Result<(), ()> {
    let args = CliArgs::parse();
    let output = apply_transform(&args.input_file, &args.transform_file)?;
    println!("{output}");
    Ok(())
}
