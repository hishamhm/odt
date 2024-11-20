//! Facilities for converting a parsed Devicetree Source file into a tree of nodes.

use crate::{Node, Property};

// TODO: apply deletes and patches
// TODO: evaluate arithmetic expressions
// TODO: assign phandles

pub fn eval() -> Node {
    Node {
        name: "".into(),
        properties: vec![Property {
            name: "prop".into(),
            value: b"propval\0".into(),
        }],
        children: vec![],
    }
}
