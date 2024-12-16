//! Facilities for converting a parsed Devicetree Source file into a tree of nodes.

use crate::label::{LabelMap, LabelResolver};
use crate::error::SourceError;
use crate::node::Node;
use crate::nodepath::NodePath;
use crate::parse::gen::*;
use crate::parse::TypedRuleExt;

type TempNode<'i> = Node<TempValue<'i>>;

#[derive(Clone)]
pub enum TempValue<'a> {
    Ast(&'a PropValue<'a>),
    Bytes(Vec<u8>),
}

/// Transform the AST into a single tree of TempNodes.  This handles deletions of nodes and
/// properties, property overrides, and label assignments.  Note that we may delete invalid
/// constructs without evaluating them.  For example, we accept
///   / { x = <(0 / 0)>; };
///   / { /delete-property/ x; };
/// while `dtc` does not.
pub fn merge<'i>(dts: &Dts<'i>) -> Result<(TempNode<'i>, LabelMap), SourceError> {
    let mut root = TempNode::default();
    let mut node_labels = LabelMap::new();
    if let Some(memres) = dts.memreserve.first() {
        unimplemented!("{}", memres.err("unimplemented"));
    }
    for top_def in dts.top_def {
        match top_def {
            TopDef::TopNode(topnode) => {
                let path = match topnode.top_node_name {
                    TopNodeName::NodeReference(noderef) => {
                        LabelResolver(&node_labels, &root).resolve(noderef)?
                    }
                    TopNodeName::RootNodeName(_) => NodePath::root(),
                };
                let node = root.walk_mut(path.segments()).unwrap();
                for label in topnode.label {
                    add_label(&mut node_labels, label, node, &path)?;
                }
                let contents = topnode.node_body.node_contents;
                fill_temp_node(&mut node_labels, node, &path, contents)?;
            }
            TopDef::TopDelNode(topdelnode) => {
                let noderef = topdelnode.node_reference;
                let target = LabelResolver(&node_labels, &root).resolve(noderef)?;
                delete_node(&mut node_labels, &mut root, &target);
            }
            TopDef::TopOmitNode(_) => (), // ignored
        }
    }
    Ok((root, node_labels))
}

impl Default for TempValue<'_> {
    fn default() -> Self {
        Self::Bytes(vec![])
    }
}

impl<'a> core::fmt::Display for TempValue<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            TempValue::Ast(ast) => {
                // TODO:  This retains comments and whitespace from the input.
                // May want the output a bit more normalized.
                write!(f, "{}", ast.str())
            }
            TempValue::Bytes(bytes) => {
                // TODO: C string detection
                if bytes.len() % 4 == 0 {
                    write!(f, "<")?;
                    let mut first = true;
                    for w in bytes.chunks_exact(4) {
                        if first {
                            first = false;
                        } else {
                            write!(f, " ")?;
                        }
                        // TODO: stabilization of slice::array_chunks would simplify this
                        let n = u32::from_be_bytes(w.try_into().unwrap());
                        write!(f, "{n:#x}")?;
                    }
                    write!(f, ">")
                } else {
                    write!(f, "[")?;
                    let mut first = true;
                    for b in bytes {
                        if first {
                            first = false;
                        } else {
                            write!(f, " ")?;
                        }
                        write!(f, "{b:02x}")?;
                    }
                    write!(f, "]")
                }
            }
        }
    }
}

fn delete_node(node_labels: &mut LabelMap, root: &mut TempNode, path: &NodePath) {
    if root.walk(path.segments()).is_none() {
        panic!("deleting nonexistent node {path:?}");
    }
    if path.is_root() {
        // delete everything
        node_labels.clear();
        *root = TempNode::default();
        return;
    }
    node_labels.retain(|_, p| !p.starts_with(path));
    let (parent, child) = (path.parent(), path.leaf());
    let parent = root.walk_mut(parent.segments()).unwrap();
    parent.remove_child(child);
}

fn fill_temp_node<'a, 'b: 'a>(
    node_labels: &mut LabelMap,
    node: &mut TempNode<'a>,
    path: &NodePath,
    contents: &NodeContents<'b>,
) -> Result<(), SourceError> {
    for child_def in contents.child_def {
        match child_def {
            ChildDef::ChildNode(childnode) => {
                let name = childnode.node_name.unescape_name();
                let child_path = path.join(name);
                let child = node.add_child(name);
                for child_node_prefix in childnode.child_node_prefix {
                    if let ChildNodePrefix::Label(label) = child_node_prefix {
                        add_label(node_labels, label, child, &child_path)?;
                    }
                }
                let contents = childnode.node_body.node_contents;
                fill_temp_node(node_labels, child, &child_path, contents)?;
            }
            ChildDef::DelNode(delnode) => {
                let name = delnode.node_name.unescape_name();
                node.remove_child(name);
                let childpath = path.join(name);
                // TODO:  This is potentially quadratic.  Could use the labels in the removed node.
                node_labels.retain(|_, p| !p.starts_with(&childpath));
            }
        }
    }
    let mut names_used = std::collections::HashSet::new();
    for prop_def in contents.prop_def {
        match prop_def {
            PropDef::Prop(prop) => {
                let name = prop.prop_name.unescape_name();
                if !names_used.insert(name) {
                    // dtc rejects this only during the first definition of a node.
                    // However, it seems sensible to reopen nodes at non-top level,
                    // but likely mistaken to redefine a property within a scope.
                    return Err(prop.prop_name.err("duplicate property"));
                }
                let value = match prop.prop_value {
                    Some(propvalue) => TempValue::Ast(propvalue),
                    None => TempValue::Bytes(vec![]),
                };
                node.set_property(name, value);
            }
            PropDef::DelProp(delprop) => {
                let name = delprop.prop_name.unescape_name();
                names_used.remove(name);
                node.remove_property(name);
            }
        }
    }
    Ok(())
}

trait UnescapeName<'a> {
    fn unescape_name(&self) -> &'a str;
}

impl<'a> UnescapeName<'a> for NodeName<'a> {
    fn unescape_name(&self) -> &'a str {
        let s = self.str();
        s.strip_prefix('\\').unwrap_or(s)
    }
}

impl<'a> UnescapeName<'a> for PropName<'a> {
    fn unescape_name(&self) -> &'a str {
        let s = self.str();
        s.strip_prefix('\\').unwrap_or(s)
    }
}

fn add_label(
    node_labels: &mut LabelMap,
    label: &Label,
    node: &mut TempNode,
    path: &NodePath,
) -> Result<(), SourceError> {
    let s = label.str().strip_suffix(':').unwrap();
    if let Some(old) = node_labels.insert(s.into(), path.clone()) {
        // dtc permits duplicate labels during evaluation, as long as only one survives.
        // This is accepted:
        //   / {
        //     x: a { };
        //     x: b { };
        //   };
        //   /delete-node/ &x;
        // Unclear if we need to emulate this.
        if old != *path {
            return Err(label.err(format!("Duplicate label also on {old:?}")));
        }
    }
    node.add_label(s);
    Ok(())
}

#[test]
fn test_duplicate_property() {
    let source = include_str!("testdata/duplicate_property.dts");
    let arena = bumpalo::Bump::new();
    let dts = crate::parse::parse_typed(source, &arena).unwrap();
    let err = merge(dts).map(|_| ()).expect_err("should fail");
    let message = format!("{err}");
    assert!(
        message.contains("duplicate property") && message.contains("this time it's an error"),
        "unexpected error:\n{message}"
    );
}
