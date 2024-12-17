use clap::Parser as _;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

#[derive(clap::Parser)]
#[command(version)]
struct Args {
    /// Input file
    #[arg(value_name = "input_path")]
    input_path: Option<PathBuf>,

    /// Input format
    #[arg(short = 'I', long, value_name = "format", default_value = "dts")]
    in_format: Format,

    /// Output format
    #[arg(short = 'O', long, value_name = "format", default_value = "dtb")]
    out_format: Format,

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

#[derive(clap::ValueEnum, Clone, Copy, Debug, PartialEq)]
enum Format {
    /// devicetree source
    Dts,
    /// devicetree blob
    Dtb,
}

pub fn dtc_main(
    args: impl IntoIterator<Item = std::ffi::OsString>,
) -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse_from(args);
    assert!(args.in_format == Format::Dts, "only DTS input is supported");
    let loader = odt::fs::Loader::new(args.include);
    let input = &args.input_path.unwrap_or(odt::fs::Loader::STDIN.into());
    let dts = odt::parse::parse_with_includes(&loader, input).map_err(|e| loader.infer_path(e))?;
    let (tree, node_labels) = odt::merge::merge(&dts).map_err(|e| loader.infer_path(e))?;
    let mut goal = String::from("-");
    let mut writer: Box<dyn Write> = if let Some(outfile) = args.out {
        goal = outfile.to_string_lossy().into_owned();
        Box::new(BufWriter::new(std::fs::File::create(outfile)?))
    } else {
        Box::new(BufWriter::new(std::io::stdout()))
    };
    match args.out_format {
        Format::Dtb => {
            let tree = odt::eval::eval(tree, node_labels).map_err(|e| loader.infer_path(e))?;
            let dtb = odt::flat::serialize(&tree);
            writer.write_all(&dtb)?;
        }
        Format::Dts => {
            // TODO:  How much should we process the input before printing here?
            //   1. paste together included files
            //   2. tree operations
            //   3. assign phandles
            //   4. evaluate expressions
            //   5. discard labels
            // Currently this prints after step 2.  Step 4 discards type information,
            // so the output would be significantly worse than that of `dtc -O dts`.
            let source = format!("/dts-v1/;\n\n{}/ {};", tree.labels_as_display(), tree);
            // Reparse and pretty-print the output.
            let tree = odt::parse::parse_untyped(&source).unwrap();
            let output = odt::print::format(tree);
            write!(writer, "{output}")?;
        }
    }
    if let Some(depfile) = args.out_dependency {
        let content = loader.write_depfile(&goal);
        std::fs::write(depfile, content)?;
    }
    Ok(())
}
