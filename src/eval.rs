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
    // Transform the AST into a tree of TempNodes.
    // Tree operations (merges and deletes) are performed, and labels are assigned.
    let (mut root, node_labels) = eval_phase1(dts)?;

    // Evaluate expressions and assign phandles.
    eval_phase2(&mut root, node_labels)?;

    // Convert to a `Node`-based tree.
    Ok(eval_phase3(root))
}

fn eval_phase1(dts: Dts) -> Result<(TempNode, LabelMap), EvalError> {
    let mut root = TempNode::default();
    let mut node_labels = LabelMap::new();
    for _memres in dts.Memreserve() {
        unimplemented!("memreserve");
    }
    for topdef in dts.TopDef() {
        if let Some(topnode) = topdef.TopNode() {
            let path = match topnode.TopNodeName().NodeReference() {
                Some(noderef) => resolve_noderef(&root, &mut node_labels, noderef.span.as_str())?,
                None => NodePath::root(),
            };
            let labels = topnode
                .Label()
                .into_iter()
                .map(|n| n.span.as_str().trim_end_matches(':'));
            let contents = topnode.NodeBody().NodeContents();
            let node = root.walk_mut(path.segments()).unwrap();
            process_node(&mut node_labels, node, &path, contents)?;
            // TODO: this adds labels in a different order than they appear in the source code.
            // might make error messages worse, or hide errors if a child both adds and deletes
            // a conflicting label.
            for label in labels {
                add_label(&mut node_labels, label, &path)?;
            }
        } else if let Some(topdelnode) = topdef.TopDelNode() {
            // TODO: extension trait for this based on Spanned?
            let noderef = topdelnode.NodeReference().span.as_str();
            let target = resolve_noderef(&root, &mut node_labels, noderef)?;
            delete_node(&mut node_labels, &mut root, &target);
        }
    }
    Ok((root, node_labels))
}

fn eval_phase2(_root: &mut TempNode, _node_labels: LabelMap) -> Result<(), EvalError> {
    // TODO
    Ok(())
}

fn eval_phase3(root: TempNode) -> Node {
    let TempNode {
        properties,
        children,
    } = root;
    let mut r = Node::default();
    for (k, v) in properties {
        match v {
            TempValue::Bytes(b) => *r.add_property(&k) = b,
            TempValue::Ast(_) => panic!("unevaluated property"),
        }
    }
    for (k, v) in children {
        let slot = r.add_child(&k);
        let mut n = eval_phase3(v);
        n.name = k;
        *slot = n;
    }
    r
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

type LabelMap = LinkedHashMap<String, NodePath>;

enum TempValue<'a> {
    Ast(PropValue<'a>),
    Bytes(Vec<u8>),
}

impl Default for TempValue<'_> {
    fn default() -> Self {
        Self::Bytes(vec![])
    }
}

#[derive(Default)]
struct TempNode<'a> {
    properties: LinkedHashMap<String, TempValue<'a>>,
    children: LinkedHashMap<String, TempNode<'a>>,
}

impl<'a> TempNode<'a> {
    fn walk<'b>(&'a self, path: impl IntoIterator<Item = &'b str>) -> Option<&'a TempNode<'a>> {
        let mut path = path.into_iter();
        match path.next() {
            None | Some("") => Some(self),
            Some(segment) => self.children.get(segment)?.walk(path),
        }
    }

    fn walk_mut<'b>(
        &mut self,
        path: impl IntoIterator<Item = &'b str>,
    ) -> Option<&mut TempNode<'a>> {
        let mut path = path.into_iter();
        match path.next() {
            None | Some("") => Some(self),
            Some(segment) => self.children.get_mut(segment)?.walk_mut(path),
        }
    }

    fn add_child(&mut self, name: &str) -> &mut TempNode<'a> {
        // TODO: hashlink reorders on hit, probably don't want that
        self.children
            .entry(name.into())
            .or_insert_with(|| Default::default())
    }

    fn add_property(&mut self, name: &str) -> &mut TempValue<'a> {
        // TODO: hashlink reorders on hit, probably don't want that
        self.properties
            .entry(name.into())
            .or_insert_with(|| Default::default())
    }

    fn remove_child(&mut self, name: &str) {
        self.children.remove(name);
    }

    fn remove_property(&mut self, name: &str) {
        self.properties.remove(name);
    }
}

fn resolve_noderef(
    root: &TempNode,
    node_labels: &LabelMap,
    noderef: &str,
) -> Result<NodePath, EvalError> {
    _resolve_noderef(root, node_labels, noderef)
        .ok_or_else(|| EvalError(format!("can't resolve node reference {noderef:?}")))
}

fn _resolve_noderef(root: &TempNode, node_labels: &LabelMap, noderef: &str) -> Option<NodePath> {
    let path = noderef.trim_matches(&['&', '{', '}']);
    let mut segments = path.split('/');
    let first = segments.next().unwrap();
    let segments = segments.filter(|s| !s.is_empty());
    let (root, mut result) = if first.is_empty() {
        // The node reference is absolute.
        (root, NodePath::root())
    } else {
        // The first segment is a label name.
        let target = node_labels.get(first)?;
        (root.walk(target.segments())?, target.clone())
    };
    // Check that the path exists.
    // TODO: hoist this into a helper and make this function pure path manipulation
    root.walk(segments.clone())?;
    for s in segments {
        result.push(s);
    }
    Some(result)
}

fn delete_node(node_labels: &mut LabelMap, root: &mut TempNode, path: &NodePath) {
    if path.is_root() {
        // delete everything
        *root = Default::default();
        *node_labels = Default::default();
        return;
    }
    if root.walk(path.segments()).is_none() {
        panic!("deleting nonexistent node {path:?}");
    }
    node_labels.retain(|_, p| !p.starts_with(&path));
    let (parent, child) = (path.parent(), path.leaf());
    let parent = root.walk_mut(parent.segments()).unwrap();
    parent.remove_child(child);
}

fn process_node(
    node_labels: &mut LabelMap,
    node: &mut TempNode,
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
            process_node(node_labels, child, &child_path, contents)?;
            for label in labels {
                add_label(node_labels, label, &child_path)?;
            }
        }
        if let Some(delnode) = def.DelNode() {
            let name = delnode.NodeName().span.as_str();
            node.remove_child(name);
            let childpath = path.join(name);
            node_labels.retain(|_, p| !p.starts_with(&childpath));
        }
    }
    for def in contents.PropDef() {
        if let Some(prop) = def.Prop() {
            let name = prop.PropName().span.as_str();
            let value = node.add_property(name);
            *value = TempValue::Bytes(b"todo".into());
        }
        if let Some(delprop) = def.DelProp() {
            let name = delprop.PropName().span.as_str();
            node.remove_property(name);
        }
    }
    Ok(())
}

fn add_label(node_labels: &mut LabelMap, label: &str, path: &NodePath) -> Result<(), EvalError> {
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
