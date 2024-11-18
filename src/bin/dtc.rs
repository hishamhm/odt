use std::io::Write;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = std::env::args().nth(1).unwrap();
    let source = std::fs::read_to_string(filename)?;
    let ast = dtsp::parse::parse(&source);
    let root = dtsp::eval::eval(ast);
    let dtb = dtsp::flat::serialize(&root);
    std::io::stdout().write_all(&dtb)?;
    Ok(())
}
