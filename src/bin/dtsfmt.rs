fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = std::env::args().nth(1).unwrap();
    let source = std::fs::read_to_string(filename)?;
    let tree = dtsp::tree::parse(&source);
    print!("{}", dtsp::print::format(tree));
    Ok(())
}
