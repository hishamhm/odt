pub mod error;
pub mod eval;
pub mod flat;
pub mod fs;
pub mod label;
pub mod merge;
pub mod node;
pub mod parse;
pub mod path;
pub mod print;

pub type Arena = bumpalo::Bump;
pub type SourceNode<'i> = node::Node<&'i parse::rules::Prop<'i>>;
pub type BinaryNode = node::Node<Vec<u8>>;

pub fn compile(
    loader: &impl fs::Loader,
    arena: &Arena,
    dts_paths: &[&std::path::Path],
) -> Result<BinaryNode, error::SourceError> {
    let annotate = |e| loader.annotate_error(e);
    let inner = || {
        let dts = parse::parse_concat_with_includes(loader, arena, dts_paths)?;
        let (tree, node_labels, _) = merge::merge(&dts)?;
        let tree = eval::resolve_incbin_paths(loader, arena, tree)?;
        eval::eval(tree, node_labels, loader)
    };
    inner().map_err(annotate)
}

pub fn merge<'a>(
    loader: &'a impl fs::Loader,
    arena: &'a Arena,
    dts_paths: &[&std::path::Path],
) -> Result<SourceNode<'a>, error::SourceError> {
    let annotate = |e| loader.annotate_error(e);
    let inner = || {
        let dts = parse::parse_concat_with_includes(loader, arena, dts_paths)?;
        let tree = merge::merge(&dts)?.0;
        let tree = eval::resolve_incbin_paths(loader, arena, tree)?;
        Ok(tree)
    };
    inner().map_err(annotate)
}
