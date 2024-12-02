use clap::Parser as _;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

#[derive(clap::Parser)]
#[command(version)]
struct Args {
    /// Input format
    #[arg(short = 'I', long, default_value = "dts")]
    in_format: String,

    /// Output format
    #[arg(short = 'O', long, default_value = "dtb")]
    out_format: String,

    /// Include search path
    #[arg(short = 'i', long)]
    include: Vec<PathBuf>,

    /// Output file (stdout if omitted)
    #[arg(short = 'o', long)]
    out: Option<PathBuf>,

    /// Output dependency file
    #[arg(short = 'd', long)]
    out_dependency: Option<PathBuf>,

    /// Input filename
    #[arg()]
    input_path: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    assert!(args.in_format == "dts", "only DTS input is supported");
    assert!(args.out_format == "dtb", "only binary output is supported");
    // TODO: optionally read from stdin -- won't work via loader
    let loader = dtsp::fs::Loader::new(args.include);
    let input = &args.input_path;
    let dts = dtsp::parse::parse_with_includes(&loader, input).map_err(|e| loader.infer_path(e))?;
    let root = dtsp::eval::eval(dts).map_err(|e| loader.infer_path(e))?;
    let dtb = dtsp::flat::serialize(&root);
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
