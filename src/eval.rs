//! Facilities for converting a parsed Devicetree Source file into a tree of nodes.

use crate::parse::rules::NodeContents;
use crate::parse::Dts;
use crate::{Node, Property};
use std::collections::HashMap;

// TODO: evaluate arithmetic expressions
// TODO: assign phandles

#[derive(Debug)]
pub struct EvalError(String);

impl core::fmt::Display for EvalError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "EvalError({:?})", self.0)
    }
}

impl std::error::Error for EvalError {}

// TODO: would really like typed_derive to produce enums from simple sum rules.
// it doesn't appear to do that even with a trivial grammar; I guess the `span` field would have to
// be duplicated into each variant.
// https://github.com/pest-parser/pest/issues/882 doesn't mention enums.

pub fn eval(dts: Dts) -> Result<Node, EvalError> {
    let mut t = TempTree::default();
    for topdef in dts.TopDef() {
        if let Some(topnode) = topdef.TopNode() {
            let path = match topnode.TopNodeName().NodeReference() {
                Some(noderef) => t.resolve_noderef(noderef.span.as_str())?,
                None => "".to_string(),
            };
            let labels = topnode
                .Label()
                .into_iter()
                .map(|n| n.span.as_str().trim_end_matches(':'));
            let contents = topnode.NodeBody().NodeContents();
            t.process_node(&path, contents)?;
            // TODO: this adds labels in a different order than they appear in the source code.
            // might make error messages worse, or hide errors if a child both adds and deletes
            // a conflicting label.
            for label in labels {
                t.add_label(label, &path)?;
            }
        } else if let Some(topdelnode) = topdef.TopDelNode() {
            // TODO: extension trait for this based on Spanned?
            let noderef = topdelnode.NodeReference().span.as_str();
            let target = t.resolve_noderef(noderef)?;
            t.delete_node(&target);
        }
    }
    Ok(t.root)
}

#[derive(Default)]
struct TempTree {
    // TODO: all kinds of labels share one namespace!
    node_labels: HashMap<String, String>,
    root: Node,
}

impl TempTree {
    fn resolve_noderef(&self, noderef: &str) -> Result<String, EvalError> {
        self._resolve_noderef(noderef)
            .ok_or_else(|| EvalError(format!("can't resolve node reference {noderef:?}")))
    }

    fn _resolve_noderef(&self, noderef: &str) -> Option<String> {
        let path = noderef.trim_matches(&['&', '{', '}']);
        let mut segments = path.split('/');
        let first = segments.next().unwrap();
        let (root, mut result) = if first.is_empty() {
            (&self.root, String::new())
        } else {
            // If the node reference does not begin with '/', the first segment is a label name.
            let target = self.node_labels.get(first)?;
            (self.root.walk(target.split('/'))?, target.to_string())
        };
        // Check that the path exists.
        root.walk(segments.clone())?;
        for s in segments {
            result.push('/');
            result.push_str(s);
        }
        Some(result)
    }

    fn delete_node(&mut self, path: &str) {
        if path.is_empty() {
            // delete everything
            *self = Self::default();
            return;
        }
        if self.root.walk(path.split('/')).is_none() {
            panic!("deleting nonexistent node {path:?}");
        }
        let path_slash = String::from(path) + "/";
        self.node_labels
            .retain(|_, p| p != path && !p.starts_with(&path_slash));
        // TODO: delete property labels too
        let (parent, child) = path.rsplit_once('/').unwrap_or(("", path));
        let parent = self.root.walk_mut(parent.split('/')).unwrap();
        parent.children.retain(|node| node.name != child);
    }

    fn process_node(&mut self, path: &str, contents: &NodeContents) -> Result<(), EvalError> {
        let node = self.root.walk_mut(path.split('/')).unwrap();
        Self::_process_node(&mut self.node_labels, node, path, contents)
    }

    fn _process_node(
        node_labels: &mut HashMap<String, String>,
        node: &mut Node,
        path: &str,
        contents: &NodeContents,
    ) -> Result<(), EvalError> {
        for def in contents.ChildDef() {
            if let Some(childnode) = def.ChildNode() {
                let name = childnode.NodeName().span.as_str();
                let child = node.add_child(name);
                let labels = childnode
                    .Label()
                    .into_iter()
                    .flatten()
                    .map(|n| n.span.as_str().trim_end_matches(':'));
                let contents = childnode.NodeBody().NodeContents();
                let path = String::from(path) + "/" + name;
                Self::_process_node(node_labels, child, &path, contents)?;
                // TODO: this adds labels in a different order than they appear in the source code.
                // might make error messages worse, or hide errors if a child both adds and deletes
                // a conflicting label.
                for label in labels {
                    Self::_add_label(node_labels, label, &path)?;
                }
            }
            if let Some(delnode) = def.DelNode() {
                let name = delnode.NodeName().span.as_str();
                node.children.retain(|node| node.name != name);
                // TODO: delete any labels
            }
        }
        for def in contents.PropDef() {
            if let Some(prop) = def.Prop() {
                let name = prop.PropName().span.as_str();
                // TODO: evaluate value
                // TODO: replace if exists
                node.properties.push(Property {
                    name: name.to_owned(),
                    value: b"todo".into(),
                });
            }
            if let Some(delprop) = def.DelProp() {
                let name = delprop.PropName().span.as_str();
                node.properties.retain(|prop| prop.name != name);
            }
        }
        Ok(())
    }

    fn add_label(&mut self, label: &str, path: &str) -> Result<(), EvalError> {
        if let Some(old) = self.node_labels.insert(label.to_owned(), path.into()) {
            return Err(EvalError(format!("label {label:?} already set to {old:?}")));
        }
        Ok(())
    }

    fn _add_label(
        node_labels: &mut HashMap<String, String>,
        label: &str,
        path: &str,
    ) -> Result<(), EvalError> {
        if let Some(old) = node_labels.insert(label.to_owned(), path.into()) {
            return Err(EvalError(format!("label {label:?} already set to {old:?}")));
        }
        Ok(())
    }
}
