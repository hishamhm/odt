//! Facilities for converting a parsed Devicetree Source file into a tree of nodes.

use crate::parse::Tree;
use crate::{Node, Property};

pub fn eval(_dts: Tree) -> Node {
    // TODO: use dts
    Node {
        name: "".into(),
        properties: vec![Property {
            name: "prop".into(),
            value: b"propval\0".into(),
        }],
        children: vec![],
    }
}
