//! Facilities for converting a parsed Devicetree Source file into a tree of nodes.

// XXX
#![allow(unused_variables)]

use crate::parse::rules::{IntLiteral, NodeContents, PropValue};
use crate::parse::Dts;
use crate::{Node, Property};
use hashlink::{LinkedHashMap, LinkedHashSet};
use std::str::FromStr;

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
    // Transform the AST into a single tree of TempNodes.  This phase handles deletions of nodes
    // and properties, property overrides, and label assignments.
    let (mut root, node_labels) = build_temp_tree(dts)?;

    // Assign phandles.
    assign_phandles(&mut root, &node_labels)?;

    // Evaluate expressions.
    evaluate_expressions(&mut root, &node_labels)?;

    // Convert to a `Node`-based tree.
    Ok(from_temp_tree(root))
}

fn build_temp_tree(dts: Dts) -> Result<(TempNode, LabelMap), EvalError> {
    let mut root = TempNode::default();
    let mut node_labels = LabelMap::new();
    for _memres in dts.Memreserve() {
        unimplemented!("memreserve");
    }
    for topdef in dts.TopDef() {
        if let Some(topnode) = topdef.TopNode() {
            let path = match topnode.TopNodeName().NodeReference() {
                Some(noderef) => LabelResolver(&node_labels, &root).resolve(noderef.str())?,
                None => NodePath::root(),
            };
            let labels = topnode
                .Label()
                .into_iter()
                .map(|n| n.str().trim_end_matches(':'));
            let contents = topnode.NodeBody().NodeContents();
            let node = root.walk_mut(path.segments()).unwrap();
            fill_temp_node(&mut node_labels, node, &path, contents)?;
            // TODO: this adds labels in a different order than they appear in the source code.
            // might make error messages worse, or hide errors if a child both adds and deletes
            // a conflicting label.
            for label in labels {
                add_label(&mut node_labels, label, &path)?;
            }
        }
        if let Some(topdelnode) = topdef.TopDelNode() {
            let noderef = topdelnode.NodeReference();
            let target = LabelResolver(&node_labels, &root).resolve(noderef.str())?;
            delete_node(&mut node_labels, &mut root, &target);
        }
    }
    Ok((root, node_labels))
}

fn assign_phandles(root: &mut TempNode, node_labels: &LabelMap) -> Result<(), EvalError> {
    let mut need_phandles = LinkedHashSet::<NodePath>::new();
    visit_phandle_references(
        LabelResolver(node_labels, root),
        root,
        &NodePath::root(),
        &mut need_phandles,
    )?;

    let mut phandle_counter = 0u32;
    for path in need_phandles {
        let node = root.walk_mut(path.segments()).unwrap();
        let phandle = node.get_property_or_insert_with("phandle", || {
            phandle_counter += 1;
            TempValue::Bytes(phandle_counter.to_be_bytes().into())
        });
        match phandle {
            TempValue::Bytes(bytes) => {
                let len = bytes.len();
                if len != 4 {
                    return Err(EvalError(format!(
                        "phandle for {path} has invalid length {len}"
                    )));
                }
                let phv = u32::from_be_bytes(bytes[..].try_into().unwrap());
                if phv == 0 || phv == !0u32 {
                    return Err(EvalError(format!(
                        "phandle for {path} has invalid value {phv}"
                    )));
                }
            }
            TempValue::Ast(_ast) => {
                // the expression here must have length 4, and may contain zero phandle references,
                // or one, pointing to itself.
                todo!();
                // TODO: also repeat the byte checks above once it is evaluated.
                // maybe move all this validation to a later pass.
                // but do have to detect self-reference now to number correctly.
            }
        }
    }

    // TODO
    Ok(())
}

fn visit_phandle_references(
    labels: LabelResolver,
    node: &TempNode,
    path: &NodePath,
    need_phandles: &mut LinkedHashSet<NodePath>,
) -> Result<(), EvalError> {
    for (_, tempvalue) in &node.properties {
        if let TempValue::Ast(propvalue) = tempvalue {
            for noderef in extract_phandle_references(propvalue) {
                let target = labels.resolve(noderef)?;
                need_phandles.insert(target);
            }
        }
    }
    for (name, tempnode) in &node.children {
        let child_path = path.join(name);
        visit_phandle_references(labels, &tempnode, &child_path, need_phandles)?;
    }
    Ok(())
}

fn extract_phandle_references<'a>(propvalue: &PropValue<'a>) -> Vec<&'a str> {
    let mut r = vec![];
    let values = propvalue.Value();
    for value in [values.0].into_iter().chain(values.1) {
        if let Some(cells) = value.Cells() {
            for cell in cells.Cell().into_iter().flatten() {
                if let Some(phandle) = cell.Phandle() {
                    r.push(phandle.str());
                }
            }
        }
    }
    r
}

fn evaluate_expressions(root: &mut TempNode, node_labels: &LabelMap) -> Result<(), EvalError> {
    // XXX here we have an ownership problem, because we need the shape of the tree to evaluate
    // label and phandle references, but we also need to mutate the propvalues.
    // could maybe use Cell to hold the temp values?
    // for now, just clone the tree.  will need to eagerly evaluate phandles.
    _evaluate_expressions(&root.clone(), node_labels, root)
}

fn _evaluate_expressions(
    root: &TempNode,
    node_labels: &LabelMap,
    node: &mut TempNode,
) -> Result<(), EvalError> {
    for (_, tempvalue) in &mut node.properties {
        if let TempValue::Bytes(_) = tempvalue {
            continue;
        }
        let TempValue::Ast(propvalue) = core::mem::take(tempvalue) else {
            unreachable!()
        };
        *tempvalue = TempValue::Bytes(evaluate_propvalue(root, node_labels, propvalue)?);
    }
    for (_, tempnode) in &mut node.children {
        _evaluate_expressions(root, node_labels, tempnode)?;
    }
    Ok(())
}

fn evaluate_propvalue(
    root: &TempNode,
    node_labels: &LabelMap,
    propvalue: PropValue,
) -> Result<Vec<u8>, EvalError> {
    let mut r = vec![];
    let values = propvalue.Value();
    for value in [values.0].into_iter().chain(values.1) {
        if let Some(cells) = value.Cells() {
            if let Some(bits) = cells.Bits() {
                todo!()
            }
            for cell in cells.Cell().into_iter().flatten() {
                if let Some(phandle) = cell.Phandle() {
                    let path = LabelResolver(node_labels, root).resolve(phandle.str())?;
                    let node = root.walk(path.segments()).unwrap();
                    let phandle = &node.properties["phandle"];
                    let TempValue::Bytes(bytes) = phandle else {
                        panic!("unevaluated phandle");
                    };
                    r.extend(bytes);
                }
                if let Some(expr) = cell.ParenExpr() {
                    todo!()
                }
                if let Some(lit) = cell.IntLiteral() {
                    let n = parse_literal(lit)?;
                    r.extend(n.to_be_bytes());
                }
            }
        }
        if let Some(quotedstring) = value.QuotedString() {
            // unescape string.  should grammar remove outer quotes?
            // append to vec, with null.
            let s = quotedstring.str();
            let s = s.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
            let s = unescape(s).unwrap(); // parser has already validated
            r.extend(s.as_bytes());
            r.push(0);
        }
        if let Some(noderef) = value.NodeReference() {
            let target = LabelResolver(node_labels, root).resolve(noderef.str())?;
            r.extend(target.display().as_bytes());
            r.push(0);
        }
        if let Some(bytestring) = value.ByteString() {
            for hexbyte in bytestring.HexByte().into_iter().flatten() {
                let s = hexbyte.str();
                let b = u8::from_str_radix(s, 16).unwrap(); // parser has already validated
                r.push(b);
            }
        }
        if let Some(incbin) = value.Incbin() {
            todo!()
        }
    }
    Ok(r)
}

fn unescape(s: &str) -> Option<String> {
    let mut r = String::new();
    let mut it = s.chars();
    while let Some(c) = it.next() {
        match c {
            '\\' => r.push(it.next()?),
            c => r.push(c),
        }
    }
    Some(r)
}

// TODO: needs a parameterized return type?
fn parse_literal(lit: &IntLiteral) -> Result<u32, EvalError> {
    let s = lit.str();
    if s.starts_with("'") {
        todo!();
    }
    let st = s.trim_end_matches(['U', 'L']); // dtc is case-sensitive here
    u32::from_str(st).map_err(|_| EvalError(format!("invalid numeric literal {s:?}")))
}

fn from_temp_tree(root: TempNode) -> Node {
    let TempNode {
        properties,
        children,
    } = root;
    let mut node = Node::default();
    for (name, tempvalue) in properties {
        let TempValue::Bytes(bytes) = tempvalue else {
            panic!("unevaluated property");
        };
        node.properties.push(Property { name, value: bytes });
    }
    for (name, tempnode) in children {
        let child = from_temp_tree(tempnode);
        node.children.push(Node { name, ..child });
    }
    node
}

/// A portable subset of PathBuf needed for working with &{...} DTS path references.  The embedded
/// string is normalized and always ends with a slash, simplifying `starts_with()` queries.
#[derive(Clone, Eq, Hash, PartialEq)]
struct NodePath(String);

impl NodePath {
    fn display(&self) -> &str {
        if self.is_root() {
            "/"
        } else {
            self.0.strip_suffix('/').unwrap()
        }
    }

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

#[derive(Clone)]
enum TempValue<'a> {
    Ast(PropValue<'a>),
    Bytes(Vec<u8>),
}

impl Default for TempValue<'_> {
    fn default() -> Self {
        Self::Bytes(vec![])
    }
}

#[derive(Clone, Default)]
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

    fn get_property_or_insert_with(
        &mut self,
        name: &str,
        default: impl FnOnce() -> TempValue<'a>,
    ) -> &mut TempValue<'a> {
        // TODO: hashlink reorders on hit, probably don't want that
        self.properties.entry(name.into()).or_insert_with(default)
    }

    fn set_property(&mut self, name: &str, value: TempValue<'a>) {
        self.properties.replace(name.into(), value);
    }

    fn remove_child(&mut self, name: &str) {
        self.children.remove(name);
    }

    fn remove_property(&mut self, name: &str) {
        self.properties.remove(name);
    }
}

#[derive(Clone, Copy)]
struct LabelResolver<'a>(&'a LabelMap, &'a TempNode<'a>);

impl LabelResolver<'_> {
    fn resolve(&self, noderef: &str) -> Result<NodePath, EvalError> {
        self._resolve(noderef)
            .ok_or_else(|| EvalError(format!("can't resolve node reference {noderef:?}")))
    }

    fn _resolve(&self, noderef: &str) -> Option<NodePath> {
        let path = noderef.trim_matches(&['&', '{', '}']);
        let mut segments = path.split('/');
        let first = segments.next().unwrap();
        let segments = segments.filter(|s| !s.is_empty());
        let (root, mut result) = if first.is_empty() {
            // The node reference is absolute.
            (self.1, NodePath::root())
        } else {
            // The first segment is a label name.
            let target = self.0.get(first)?;
            (self.1.walk(target.segments())?, target.clone())
        };
        // Check that the path exists.
        root.walk(segments.clone())?;
        for s in segments {
            result.push(s);
        }
        Some(result)
    }
}

fn delete_node(node_labels: &mut LabelMap, root: &mut TempNode, path: &NodePath) {
    if root.walk(path.segments()).is_none() {
        panic!("deleting nonexistent node {path:?}");
    }
    if path.is_root() {
        // delete everything
        node_labels.clear();
        *root = Default::default();
        return;
    }
    node_labels.retain(|_, p| !p.starts_with(&path));
    let (parent, child) = (path.parent(), path.leaf());
    let parent = root.walk_mut(parent.segments()).unwrap();
    parent.remove_child(child);
}

fn fill_temp_node<'a, 'b: 'a>(
    node_labels: &mut LabelMap,
    node: &mut TempNode<'a>,
    path: &NodePath,
    contents: &NodeContents<'b>,
) -> Result<(), EvalError> {
    for def in contents.ChildDef() {
        if let Some(childnode) = def.ChildNode() {
            let name = childnode.NodeName().str();
            let child = node.add_child(name);
            let labels = childnode
                .Label()
                .into_iter()
                .flatten()
                .map(|n| n.str().trim_end_matches(':'));
            let contents = childnode.NodeBody().NodeContents();
            let child_path = path.join(name);
            fill_temp_node(node_labels, child, &child_path, contents)?;
            for label in labels {
                add_label(node_labels, label, &child_path)?;
            }
        }
        if let Some(delnode) = def.DelNode() {
            let name = delnode.NodeName().str();
            node.remove_child(name);
            let childpath = path.join(name);
            node_labels.retain(|_, p| !p.starts_with(&childpath));
        }
    }
    for def in contents.PropDef() {
        if let Some(prop) = def.Prop() {
            let name = prop.PropName().str();
            let value = match prop.PropValue() {
                Some(propvalue) => TempValue::Ast(propvalue.clone()),
                None => TempValue::Bytes(vec![]),
            };
            node.set_property(name, value);
        }
        if let Some(delprop) = def.DelProp() {
            let name = delprop.PropName().str();
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

trait SpannedStr<'a, R: pest_typed::RuleType, T: pest_typed::Spanned<'a, R>> {
    fn str(&self) -> &'a str;
}

impl<'a, R: pest_typed::RuleType, T: pest_typed::Spanned<'a, R>> SpannedStr<'a, R, T> for T {
    fn str(&self) -> &'a str {
        self.span().as_str()
    }
}
