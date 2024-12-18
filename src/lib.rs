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

type SourceNode<'i> = node::Node<&'i parse::gen::Prop<'i>>;
type BinaryNode = node::Node<Vec<u8>>;
