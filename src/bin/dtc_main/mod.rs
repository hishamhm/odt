// This is in a mod.rs so cargo's "autobins" feature doesn't find it.

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
    /// devicetree blob
    Dtb,
    /// devicetree source
    Dts,
    /// fully-evaluated devicetree source
    Dtv,
}

pub fn dtc_main(
    args: impl IntoIterator<Item = std::ffi::OsString>,
) -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse_from(args);

    match args.in_format {
        Format::Dtb => dtb_input(args),
        Format::Dts | Format::Dtv => dts_input(args),
    }
}

fn dtb_input(args: Args) -> Result<(), Box<dyn std::error::Error>> {
    use odt::fs::{Loader, LocalFileLoader};
    let loader = LocalFileLoader::new(args.include);
    let input = args.input_path.unwrap_or(LocalFileLoader::STDIN.into());
    let Some((_path, data)) = loader.read(input.clone()) else {
        panic!("can't read {input:?}");
    };
    let tree = odt::flat::deserialize(data)?;
    let (goal, mut writer) = open_output(args.out)?;
    match args.out_format {
        Format::Dtb => {
            let dtb = odt::flat::serialize(&tree);
            writer.write_all(&dtb)?;
        }
        Format::Dts | Format::Dtv => {
            let source = format!("/dts-v1/;/{tree};");
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

fn dts_input(args: Args) -> Result<(), Box<dyn std::error::Error>> {
    use odt::fs::{Loader, LocalFileLoader};
    let loader = LocalFileLoader::new(args.include);
    let input = args.input_path.unwrap_or(LocalFileLoader::STDIN.into());
    let arena = odt::Arena::new();
    let (goal, mut writer) = open_output(args.out)?;
    match args.out_format {
        Format::Dtb => {
            let tree = odt::compile(&loader, &arena, &[&input])?;
            let dtb = odt::flat::serialize(&tree);
            writer.write_all(&dtb)?;
        }
        Format::Dtv => {
            // Lower all the way to binary node values, then convert back into source.
            // Types are lost in this process.
            let tree = odt::compile(&loader, &arena, &[&input])?;
            let source = format!("/dts-v1/;/{tree};");
            // Reparse and pretty-print the output.
            let tree = odt::parse::parse_untyped(&source).unwrap();
            let output = odt::print::format(tree);
            write!(writer, "{output}")?;
        }
        Format::Dts => {
            // This shows the tree after /include/ directives and merge operations,
            // but before assigning phandles or evaluating expressions.
            let tree = odt::merge(&loader, &arena, &[&input])?;
            let source = format!("/dts-v1/;{}/{tree};", tree.labels_as_display());
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

fn open_output(
    out: Option<PathBuf>,
) -> Result<(String, Box<dyn Write>), Box<dyn std::error::Error>> {
    Ok(match out {
        Some(path) => (
            (path.to_string_lossy().into_owned()),
            Box::new(BufWriter::new(std::fs::File::create(path)?)),
        ),
        None => ("-".into(), Box::new(BufWriter::new(std::io::stdout()))),
    })
}
