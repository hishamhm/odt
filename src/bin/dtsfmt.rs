use clap::Parser as _;
use std::path::PathBuf;

#[derive(clap::Parser)]
#[command(version)]
struct Args {
    /// Input files
    #[arg(value_name = "input_path")]
    input_path: Vec<PathBuf>,

    /// Edit files in place
    #[arg(short = 'i', long)]
    in_place: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    if args.input_path.is_empty() {
        let source = std::io::read_to_string(std::io::stdin())?;
        let ast = odt::parse_untyped::parse(&source);
        let output = odt::print::format(ast);
        print!("{}", output);
        return Ok(());
    }
    for filename in args.input_path {
        let source = std::fs::read_to_string(filename.clone())?;
        let ast = odt::parse_untyped::parse(&source);
        let output = odt::print::format(ast);
        if args.in_place {
            // TODO:  Write to a temporary file and rename over the original.
            // Reopening the file like this can lose data (e.g. if the disk is full).
            std::fs::write(filename, output)?;
        } else {
            print!("{}", output);
        }
    }
    Ok(())
}
