pub mod eval;
pub mod flat;
pub mod parse;
pub mod print;

// TODO:  should this represent labels?  aliases?  phandles?

pub struct Node {
    pub name: String,
    pub properties: Vec<Property>,
    pub children: Vec<Node>,
}

pub struct Property {
    pub name: String,
    pub value: Vec<u8>,
}
