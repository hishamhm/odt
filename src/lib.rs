pub mod error;
pub mod eval;
pub mod flat;
pub mod fs;
pub mod node;
pub mod parse;
pub mod print;

// TODO: Return compiled output as these types, or as `node::Node<Vec<u8>>`?

#[derive(Debug, Default)]
pub struct Node {
    pub name: String,
    pub properties: Vec<Property>,
    pub children: Vec<Node>,
}

#[derive(Debug, Default)]
pub struct Property {
    pub name: String,
    pub value: Vec<u8>,
}
