//! Facilities for converting a parsed Devicetree Source file into a tree of nodes.

use crate::parse::rules::{NodeContents, PropValue};
use crate::parse::Dts;
use crate::Node;
use hashlink::LinkedHashMap;

#[derive(Debug)]
pub struct EvalError(String);
// TODO: need a Span in this error

impl core::fmt::Display for EvalError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "EvalError({:?})", self.0)
    }
}

impl std::error::Error for EvalError {}

// TODO: would really like typed_derive to produce enums from simple sum rules.
// it doesn't appear to do that even with a trivial grammar; I guess the `span` field would have to
// be duplicated into each variant.
// Ah, the `content` field is a a typedef enum with variants _0, _1, _2... ugly.
// See what pest3 looks like.

pub fn eval(dts: Dts) -> Result<Node, EvalError> {
    // First pass transforms the AST into a `TempTree`.
    // Tree operations (merges and deletes) are performed, and labels are assigned.
    let mut t = TempTree::default();
    // TODO: split out label map into their own mut local
    for _memres in dts.Memreserve() {
        unimplemented!("memreserve");
    }
    for topdef in dts.TopDef() {
        if let Some(topnode) = topdef.TopNode() {
            let path = match topnode.TopNodeName().NodeReference() {
                Some(noderef) => t.resolve_noderef(noderef.span.as_str())?,
                None => NodePath::root(),
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

    // TODO: Second pass evaluates expressions and assigns phandles.

    // TODO: Third pass constructs a `Node`-based tree.

    Ok(t.root)
}

/// A portable subset of PathBuf needed for working with &{...} DTS path references.  The embedded
/// string is normalized and always ends with a slash, simplifying `starts_with()` queries.
#[derive(Clone, PartialEq)]
struct NodePath(String);

impl NodePath {
    fn is_root(&self) -> bool {
        self.0.len() == 1
    }

    fn join(&self, suffix: &str) -> Self {
        let mut r = self.clone();
        r.push(suffix);
        r
    }

    fn leaf(&self) -> &str {
        self.0.rsplit_terminator('/').next().unwrap()
    }

    fn parent(&self) -> Self {
        if self.is_root() {
            return self.clone();
        }
        Self(self.0[..self.0.len() - 1 - self.leaf().len()].into())
    }

    fn push(&mut self, suffix: &str) {
        for segment in suffix.split('/').filter(|s| !s.is_empty()) {
            self.0.push_str(segment);
            self.0.push('/');
        }
    }

    fn root() -> Self {
        Self("/".into())
    }

    fn segments(&self) -> impl Iterator<Item = &str> {
        self.0.split_terminator('/').filter(|s| !s.is_empty())
    }

    fn starts_with(&self, prefix: &Self) -> bool {
        self.0.starts_with(&prefix.0)
    }
}

impl std::fmt::Debug for NodePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::fmt::Display for NodePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Default)]
struct TempTree {
    // TODO: split up this struct
    node_labels: LabelMap,
    // TODO: use something more efficient so construction is not quadratic
    root: Node,
}

type LabelMap = LinkedHashMap<String, NodePath>;

#[derive(Default)]
struct TempNode<'a> {
    properties: LinkedHashMap<String, TempValue<'a>>,
    children: LinkedHashMap<String, TempNode<'a>>,
}

enum TempValue<'a> {
    Ast(PropValue<'a>),
    Bytes(Vec<u8>),
}

impl Default for TempValue<'_> {
    fn default() -> Self {
        Self::Bytes(vec![])
    }
}

impl TempTree {
    fn resolve_noderef(&self, noderef: &str) -> Result<NodePath, EvalError> {
        self._resolve_noderef(noderef)
            .ok_or_else(|| EvalError(format!("can't resolve node reference {noderef:?}")))
    }

    fn _resolve_noderef(&self, noderef: &str) -> Option<NodePath> {
        let path = noderef.trim_matches(&['&', '{', '}']);
        let mut segments = path.split('/');
        let first = segments.next().unwrap();
        let segments = segments.filter(|s| !s.is_empty());
        let (root, mut result) = if first.is_empty() {
            // The node reference is absolute.
            (&self.root, NodePath::root())
        } else {
            // The first segment is a label name.
            let target = self.node_labels.get(first)?;
            (self.root.walk(target.segments())?, target.clone())
        };
        // Check that the path exists.
        root.walk(segments.clone())?;
        for s in segments {
            result.push(s);
        }
        Some(result)
    }

    fn delete_node(&mut self, path: &NodePath) {
        if path.is_root() {
            // delete everything
            *self = Self::default();
            return;
        }
        if self.root.walk(path.segments()).is_none() {
            panic!("deleting nonexistent node {path:?}");
        }
        self.node_labels.retain(|_, p| !p.starts_with(&path));
        let (parent, child) = (path.parent(), path.leaf());
        let parent = self.root.walk_mut(parent.segments()).unwrap();
        parent.children.retain(|node| node.name != child);
    }

    fn process_node(&mut self, path: &NodePath, contents: &NodeContents) -> Result<(), EvalError> {
        let node = self.root.walk_mut(path.segments()).unwrap();
        Self::_process_node(&mut self.node_labels, node, path, contents)
    }

    fn _process_node(
        node_labels: &mut LabelMap,
        node: &mut Node,
        path: &NodePath,
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
                let child_path = path.join(name);
                Self::_process_node(node_labels, child, &child_path, contents)?;
                for label in labels {
                    Self::_add_label(node_labels, label, &child_path)?;
                }
            }
            if let Some(delnode) = def.DelNode() {
                let name = delnode.NodeName().span.as_str();
                node.children.retain(|node| node.name != name);
                let childpath = path.join(name);
                node_labels.retain(|_, p| !p.starts_with(&childpath));
            }
        }
        for def in contents.PropDef() {
            if let Some(prop) = def.Prop() {
                let name = prop.PropName().span.as_str();
                let value = node.add_property(name);
                *value = b"todo".into();
            }
            if let Some(delprop) = def.DelProp() {
                let name = delprop.PropName().span.as_str();
                node.properties.retain(|prop| prop.name != name);
            }
        }
        Ok(())
    }

    fn add_label(&mut self, label: &str, path: &NodePath) -> Result<(), EvalError> {
        Self::_add_label(&mut self.node_labels, label, path)
    }

    fn _add_label(
        node_labels: &mut LabelMap,
        label: &str,
        path: &NodePath,
    ) -> Result<(), EvalError> {
        if let Some(old) = node_labels.insert(label.to_owned(), path.clone()) {
            // dtc permits duplicate labels during evaluation, as long as only one survives.
            // This is accepted:
            //   / {
            //     x: a { };
            //     x: b { };
            //   };
            //   /delete-node/ &x;
            // Unclear if we need to emulate this.
            if old != *path {
                // TODO: get a span into this error
                return Err(EvalError(format!(
                    "Duplicate label: {label:?} already set to {old:?}"
                )));
            }
        }
        Ok(())
    }
}
