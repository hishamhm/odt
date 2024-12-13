use clap::Parser as _;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

#[derive(clap::Parser)]
#[command(version)]
struct Args {
    /// Input file
    #[arg(value_name = "input_path")]
    input_path: PathBuf,

    /// Input format
    #[arg(short = 'I', long, value_name = "format", default_value = "dts")]
    in_format: String,

    /// Output format
    #[arg(short = 'O', long, value_name = "format", default_value = "dtb")]
    out_format: String,

    /// Output file (stdout if omitted)
    #[arg(short = 'o', long, value_name = "path")]
    out: Option<PathBuf>,

    /// Output dependency file
    #[arg(short = 'd', long, value_name = "path")]
    out_dependency: Option<PathBuf>,

    /// Add a directory to the include search path
    #[arg(short = 'i', long, value_name = "path")]
    include: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    assert!(args.in_format == "dts", "only DTS input is supported");
    assert!(args.out_format == "dtb", "only binary output is supported");
    // TODO: optionally read from stdin -- won't work via loader
    let loader = odt::fs::Loader::new(args.include);
    let input = &args.input_path;
    let dts = odt::parse::parse_with_includes(&loader, input).map_err(|e| loader.infer_path(e))?;
    let root = odt::eval::eval(&dts).map_err(|e| loader.infer_path(e))?;
    let dtb = odt::flat::serialize(&root);
    let mut goal = String::from("-");
    let mut writer: Box<dyn Write> = if let Some(outfile) = args.out {
        goal = outfile.to_string_lossy().into_owned();
        Box::new(BufWriter::new(std::fs::File::create(outfile)?))
    } else {
        Box::new(BufWriter::new(std::io::stdout()))
    };
    writer.write_all(&dtb)?;
    if let Some(depfile) = args.out_dependency {
        let content = loader.write_depfile(&goal);
        std::fs::write(depfile, content)?;
    }
    Ok(())
}
