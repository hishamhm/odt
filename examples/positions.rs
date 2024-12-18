use clap::Parser as _;
use odt::fs::Loader;
use odt::merge::merge;
use odt::node::Node;
use odt::parse::gen::TypedRule;
use odt::parse::parse_with_includes;
use odt::path::NodePath;
use std::path::PathBuf;

#[derive(clap::Parser)]
struct Args {
    /// Input file
    #[arg(value_name = "input_path")]
    input_path: Option<PathBuf>,

    /// Add a directory to the include search path
    #[arg(short = 'i', long, value_name = "path")]
    include: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let loader = Loader::new(args.include);
    let input = args.input_path.unwrap_or(Loader::STDIN.into());
    let dts = parse_with_includes(&loader, &input).map_err(|e| loader.with_path(e))?;
    println!("source files:");
    for path in loader.positive_deps() {
        println!("  {}", path.display());
    }
    println!();
    let (tree, node_labels) = merge(&dts).map_err(|e| loader.with_path(e))?;
    for path in paths(&tree) {
        println!("node {path}:");
        let node = tree.walk(path.segments()).unwrap();
        for (name, prop) in node.properties() {
            // Find the edit point to modify the value of this property.
            let span = match prop.prop_value {
                None => prop.semicolon.span(),
                Some(propvalue) => propvalue.labeled_value[0].value.span(),
            };
            let buffer = span.get_input().as_bytes().as_ptr_range();
            let file = loader.path_of_buffer(buffer).unwrap();
            // XXX BEWARE XXX
            // This query is linear in the file size, so quadratic in a loop!
            // If multiple queries are needed, add a cache of line starts to loader.
            let (line, col) = span.start_pos().line_col();
            // The column value is in codepoints, not bytes.
            println!("  {name} defined at {}:{line}:{col}", file.display());
        }
        // TODO:  Would also be nice to have the insertion point for new properties.
        // `fill_source_node()` could keep a map from NodePath to last definition.
        println!();
    }
    Ok(())
}

fn paths<P>(root: &Node<P>) -> Vec<NodePath> {
    let mut out = vec![];
    _paths(root, NodePath::root(), &mut out);
    out
}

fn _paths<P>(node: &Node<P>, path: NodePath, out: &mut Vec<NodePath>) {
    out.push(path.clone());
    for (name, child) in node.children() {
        _paths(child, path.join(name), out);
    }
}
