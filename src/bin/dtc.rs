use std::io::Write;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // TODO: use clap here
    let mut include_path = vec![];
    let mut args = std::env::args().skip(1).peekable();
    while args.peek().map(|s| s == "-i" || s == "--include") == Some(true) {
        args.next();
        include_path.push(std::path::PathBuf::from(args.next().unwrap()));
    }
    let filename = args.next().unwrap();
    let loader = dtsp::fs::IncludeLoader::new(include_path);
    let path = std::path::PathBuf::from(filename);
    let ast = dtsp::parse::parse_with_includes(&loader, &path).map_err(|e| loader.infer_path(e))?;
    let root = dtsp::eval::eval(ast).map_err(|e| loader.infer_path(e))?;
    let dtb = dtsp::flat::serialize(&root);
    std::io::stdout().write_all(&dtb)?;
    Ok(())
}
