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
pub type SourceNode<'i> = node::Node<&'i parse::gen::Prop<'i>>;
pub type BinaryNode = node::Node<Vec<u8>>;
