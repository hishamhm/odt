//! Facilities for converting a parsed Devicetree Source file into a tree of nodes.

// XXX
#![allow(unused_variables)]

use crate::error::SourceError;
use crate::parse::rules::{IntLiteral, Label, NodeContents, NodeReference, PropValue};
use crate::parse::{Dts, SpannedExt};
use crate::{Node, Property};
use hashlink::linked_hash_map::Entry;
use hashlink::{LinkedHashMap, LinkedHashSet};

pub fn eval(dts: Dts) -> Result<Node, SourceError> {
    // Transform the AST into a single tree of TempNodes.  This phase handles deletions of nodes
    // and properties, property overrides, and label assignments.  Note that we may delete invalid
    // constructs without evaluating them.  For example, we accept
    //   / { x = <(0 / 0)>; };
    //   / { /delete-property/ x; };
    // while `dtc` does not.
    let (mut root, node_labels) = build_temp_tree(dts)?;

    // Assign phandles.
    assign_phandles(&mut root, &node_labels)?;

    // Evaluate expressions.
    evaluate_expressions(&mut root, &node_labels)?;

    // Convert to a `Node`-based tree.
    Ok(from_temp_tree(root))
}

fn build_temp_tree(dts: Dts) -> Result<(TempNode, LabelMap), SourceError> {
    let mut root = TempNode::default();
    let mut node_labels = LabelMap::new();
    for _memres in dts.Memreserve() {
        unimplemented!("memreserve");
    }
    for topdef in dts.TopDef() {
        if let Some(topnode) = topdef.TopNode() {
            let path = match topnode.TopNodeName().NodeReference() {
                Some(noderef) => LabelResolver(&node_labels, &root).resolve(noderef)?,
                None => NodePath::root(),
            };
            let node = root.walk_mut(path.segments()).unwrap();
            for label in topnode.Label() {
                add_label(&mut node_labels, label, &path)?;
            }
            let contents = topnode.NodeBody().NodeContents();
            fill_temp_node(&mut node_labels, node, &path, contents)?;
        }
        if let Some(topdelnode) = topdef.TopDelNode() {
            let noderef = topdelnode.NodeReference();
            let target = LabelResolver(&node_labels, &root).resolve(noderef)?;
            delete_node(&mut node_labels, &mut root, &target);
        }
    }
    Ok((root, node_labels))
}

fn assign_phandles(root: &mut TempNode, node_labels: &LabelMap) -> Result<(), SourceError> {
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
                // This must be a phandle we previously assigned.
                // TODO: it could be an empty property.  how do we get a span to report the error
                // in that case?  maybe store the whole Prop node in TempValue?
                assert_eq!(bytes.len(), 4);
                let phv = u32::from_be_bytes(bytes[..].try_into().unwrap());
                assert_ne!(phv, 0);
                assert_ne!(phv, !0u32);
            }
            TempValue::Ast(ast) => {
                // the expression here must have length 4, and may contain zero phandle references,
                // or one, pointing to itself.
                todo!("{}", ast.err("unimplemented".into()));
                // TODO: also repeat the byte checks above once it is evaluated.
                // maybe move all this validation to a later pass.
                // but do have to detect self-reference now to number correctly.
            }
        }
    }

    Ok(())
}

fn visit_phandle_references(
    labels: LabelResolver,
    node: &TempNode,
    path: &NodePath,
    need_phandles: &mut LinkedHashSet<NodePath>,
) -> Result<(), SourceError> {
    for (_, tempvalue) in &node.properties {
        if let TempValue::Ast(propvalue) = tempvalue {
            let values = propvalue.Value();
            for value in [values.0].into_iter().chain(values.1) {
                if let Some(cells) = value.Cells() {
                    for cell in cells.Cell().into_iter().flatten() {
                        if let Some(phandle) = cell.NodeReference() {
                            let target = labels.resolve(&phandle)?;
                            need_phandles.replace(target);
                        }
                    }
                }
            }
        }
    }
    for (name, tempnode) in &node.children {
        let child_path = path.join(name);
        visit_phandle_references(labels, &tempnode, &child_path, need_phandles)?;
    }
    Ok(())
}

fn evaluate_expressions(root: &mut TempNode, node_labels: &LabelMap) -> Result<(), SourceError> {
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
) -> Result<(), SourceError> {
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
) -> Result<Vec<u8>, SourceError> {
    let mut r = vec![];
    let values = propvalue.Value();
    for value in [values.0].into_iter().chain(values.1) {
        if let Some(cells) = value.Cells() {
            if let Some(bits) = cells.Bits() {
                todo!()
            }
            for cell in cells.Cell().into_iter().flatten() {
                if let Some(phandle) = cell.NodeReference() {
                    let path = LabelResolver(node_labels, root).resolve(phandle)?;
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
            let target = LabelResolver(node_labels, root).resolve(noderef)?;
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

// TODO: needs a parameterized return type for /bits/?
fn parse_literal(lit: &IntLiteral) -> Result<u32, SourceError> {
    let s = lit.str();
    if s.starts_with("'") {
        todo!();
    }
    let s = s.trim_end_matches(['U', 'L']); // dtc is case-sensitive here
    let n = parse_int(s).ok_or_else(|| lit.err("invalid numeric literal".into()))?;
    // XXX dtc only requires that upper bits match; sign-extending a negative number is OK.
    // XXX i think it does this so intermediate arithmetic can be 64-bit.  lazy.
    u32::try_from(n).map_err(|_| lit.err("numeric literal exceeds 32 bits".into()))
}

fn parse_int(s: &str) -> Option<u64> {
    // TODO:  It would be nice to permit underscores or other digit separators.
    //        The grammar would need update too.
    if s == "0" {
        Some(0)
    } else if let Some(hex) = s.strip_prefix("0x").or(s.strip_prefix("0X")) {
        u64::from_str_radix(hex, 16).ok()
    } else if let Some(oct) = s.strip_prefix('0') {
        u64::from_str_radix(oct, 8).ok()
    } else {
        u64::from_str_radix(s, 10).ok()
    }
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

impl core::fmt::Debug for NodePath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.0.fmt(f)
    }
}

impl core::fmt::Display for NodePath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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

/// An intermediate representation used to gather deletes and overrides.  We used an
/// order-preserving container to stay closer to the input.  (This doesn't match `dtc` perfectly in
/// the presence of deletes, because it implements all deletes via tombstones.)
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
        // Avoid `Entry::or_insert_with()` because on LinkedHashMap that reorders existing entries.
        match self.children.entry(name.into()) {
            Entry::Vacant(entry) => entry.insert(TempNode::default()),
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }

    fn get_property_or_insert_with(
        &mut self,
        name: &str,
        default: impl FnOnce() -> TempValue<'a>,
    ) -> &mut TempValue<'a> {
        // Avoid `Entry::or_insert_with()` because on LinkedHashMap that reorders existing entries.
        match self.properties.entry(name.into()) {
            Entry::Vacant(entry) => entry.insert(default()),
            Entry::Occupied(entry) => entry.into_mut(),
        }
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
    fn resolve(&self, noderef: &NodeReference) -> Result<NodePath, SourceError> {
        self.resolve_str(noderef.str())
            .ok_or_else(|| noderef.err(format!("can't resolve node reference")))
    }

    fn resolve_str(&self, noderef: &str) -> Option<NodePath> {
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
        *root = TempNode::default();
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
) -> Result<(), SourceError> {
    for def in contents.ChildDef() {
        if let Some(childnode) = def.ChildNode() {
            let name = childnode.NodeName().str();
            let child_path = path.join(name);
            let child = node.add_child(name);
            for label in childnode.Label().into_iter().flatten() {
                add_label(node_labels, label, &child_path)?;
            }
            let contents = childnode.NodeBody().NodeContents();
            fill_temp_node(node_labels, child, &child_path, contents)?;
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

fn add_label(
    node_labels: &mut LabelMap,
    label: &Label,
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
    Ok(())
}
