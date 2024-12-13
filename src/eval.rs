//! Facilities for converting a parsed Devicetree Source file into a tree of nodes.

use crate::error::SourceError;
use crate::parse::gen::*;
use crate::parse::{SpanExt, TypedRuleExt};
use crate::{Node, Property};
use core::str::CharIndices;
use hashlink::linked_hash_map::Entry;
use hashlink::{LinkedHashMap, LinkedHashSet};
use std::borrow::Cow;

pub fn eval(dts: &Dts) -> Result<Node, SourceError> {
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

fn build_temp_tree<'i>(dts: &Dts<'i>) -> Result<(TempNode<'i>, LabelMap), SourceError> {
    let mut root = TempNode::default();
    let mut node_labels = LabelMap::new();
    if let Some(memres) = dts.memreserve.first() {
        unimplemented!("{}", memres.err("unimplemented"));
    }
    for topdef in dts.topdef {
        match topdef {
            TopDef::TopNode(topnode) => {
                let path = match topnode.topnodename {
                    TopNodeName::NodeReference(noderef) => {
                        LabelResolver(&node_labels, &root).resolve(noderef)?
                    }
                    TopNodeName::RootNodeName(_) => NodePath::root(),
                };
                let node = root.walk_mut(path.segments()).unwrap();
                for label in topnode.label {
                    add_label(&mut node_labels, label, &path)?;
                }
                let contents = topnode.nodebody.nodecontents;
                fill_temp_node(&mut node_labels, node, &path, contents)?;
            }
            TopDef::TopDelNode(topdelnode) => {
                let noderef = topdelnode.nodereference;
                let target = LabelResolver(&node_labels, &root).resolve(noderef)?;
                delete_node(&mut node_labels, &mut root, &target);
            }
            TopDef::TopOmitNode(_) => (), // ignored
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
                todo!("{}", ast.err("phandle self-reference unimplemented"));
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
            for labeledvalue in propvalue.labeledvalue {
                if let Value::Cells(cells) = labeledvalue.value {
                    for labelorcell in cells.labelorcell {
                        if let LabelOrCell::Cell(Cell::NodeReference(phandle)) = labelorcell {
                            let target = labels.resolve(phandle)?;
                            need_phandles.replace(target);
                        }
                    }
                }
            }
        }
    }
    for (name, tempnode) in &node.children {
        let child_path = path.join(name);
        visit_phandle_references(labels, tempnode, &child_path, need_phandles)?;
    }
    Ok(())
}

fn evaluate_expressions(root: &mut TempNode, node_labels: &LabelMap) -> Result<(), SourceError> {
    // TODO:  Ownership is a challenge here.  We need the shape of the tree to evaluate
    // label and phandle references, but we also need to mutate the propvalues.
    // Could put each property's TempValue into a Cell<>?
    // For now, just clone the tree.
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
    propvalue: &PropValue,
) -> Result<Vec<u8>, SourceError> {
    let mut r = vec![];
    for labeledvalue in propvalue.labeledvalue {
        match labeledvalue.value {
            Value::Cells(cells) => {
                let bits = match cells.bits {
                    None => 32,
                    Some(bits) => {
                        let n = bits.numericliteral.eval()?;
                        match n {
                            8 | 16 | 32 | 64 => n,
                            _ => return Err(bits.err("bad bit width: must be 8, 16, 32, or 64")),
                        }
                    }
                };
                for labelorcell in cells.labelorcell {
                    let LabelOrCell::Cell(cell) = labelorcell else {
                        continue;
                    };
                    let n = match cell {
                        Cell::NodeReference(noderef) => {
                            let path = LabelResolver(node_labels, root).resolve(noderef)?;
                            let node = root.walk(path.segments()).unwrap();
                            let phandle = &node.properties["phandle"];
                            let TempValue::Bytes(bytes) = phandle else {
                                panic!("unevaluated phandle");
                            };
                            if bits != 32 {
                                return Err(noderef.err("phandle references need /bits/ == 32"));
                            }
                            assert_eq!(bytes.len(), 4);
                            r.extend(bytes);
                            continue;
                        }
                        Cell::ParenExpr(expr) => expr.eval()?,
                        Cell::IntLiteral(lit) => lit.eval()?,
                    };
                    if bits < 64 {
                        // dtc warns if the lost bits are not all the same.
                        // We might also want to warn if they are ones but the value looks positive.
                        let sign_bits = (63 - bits) as u32;
                        let sign_extended = ((n as i64) << sign_bits >> sign_bits) as u64;
                        if n != sign_extended {
                            let err = cell.err(format!("value exceeds {bits} bits"));
                            let trunc = n & sign_extended;
                            let tchars = 2 + bits as usize / 4;
                            // TODO: Reporter interface for warnings.  Can't decorate span with file path
                            // here, and these are printed even if a more severe error occurs later.
                            eprintln!("Truncating value {n:#x} to {trunc:#0tchars$x}:\n{err}");
                        }
                    }
                    match bits {
                        8 => r.push(n as u8),
                        16 => r.extend((n as u16).to_be_bytes()),
                        32 => r.extend((n as u32).to_be_bytes()),
                        64 => r.extend((n as u64).to_be_bytes()),
                        _ => unreachable!(),
                    }
                }
            }
            Value::QuotedString(quotedstring) => {
                let bytes = quotedstring.unescape()?;
                r.extend(&*bytes);
                r.push(0);
            }
            Value::NodeReference(noderef) => {
                let target = LabelResolver(node_labels, root).resolve(noderef)?;
                r.extend(target.display().as_bytes());
                r.push(0);
            }
            Value::ByteString(bytestring) => {
                for labelorhexbyte in bytestring.labelorhexbyte {
                    if let LabelOrHexByte::HexByte(hexbyte) = labelorhexbyte {
                        let s = hexbyte.str();
                        let b = u8::from_str_radix(s, 16).unwrap(); // parser has already validated
                        r.push(b);
                    }
                }
            }
            Value::Incbin(incbin) => {
                unimplemented!("{}", incbin.err("/incbin/ unimplemented"));
            }
        }
    }
    Ok(r)
}

trait UnescapeExt<'a> {
    fn unescape(&self) -> Result<Cow<'a, [u8]>, SourceError>;
}

impl<'a> UnescapeExt<'a> for QuotedString<'a> {
    fn unescape(&self) -> Result<Cow<'a, [u8]>, SourceError> {
        self.trim_one().unescape()
    }
}

impl<'a> UnescapeExt<'a> for CharLiteral<'a> {
    fn unescape(&self) -> Result<Cow<'a, [u8]>, SourceError> {
        let r = self.trim_one().unescape()?;
        match r.len() {
            1 => Ok(r),
            n => Err(self.err(format!("char literal is {n} bytes, should be one byte"))),
        }
    }
}

impl<'a> UnescapeExt<'a> for pest::Span<'a> {
    fn unescape(&self) -> Result<Cow<'a, [u8]>, SourceError> {
        let s = self.as_str();
        if !s.contains('\\') {
            return Ok(Cow::Borrowed(s.as_bytes()));
        }
        fn push_char(r: &mut Vec<u8>, c: char) {
            match c.len_utf8() {
                1 => r.push(c as u8),
                _ => r.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes()),
            }
        }
        fn take_hex<'a>(it: &mut CharIndices<'a>) -> Result<u8, &'a str> {
            let n = it
                .clone()
                .take(2)
                .take_while(|(_, c)| c.is_ascii_hexdigit())
                .count();
            let s = &it.as_str()[..n];
            it.take(n).last();
            u8::from_str_radix(s, 16).or(Err(s))
        }
        fn take_oct<'a>(it: &mut CharIndices<'a>) -> Result<u8, &'a str> {
            let n = it
                .clone()
                .take(3)
                .take_while(|(_, c)| c.is_digit(8))
                .count();
            let s = &it.as_str()[..n];
            it.take(n).last();
            // `dtc` will accept and discard a ninth bit, e.g. '\501' is 'A'.
            // We reject escapes above '\377'.
            u8::from_str_radix(s, 8).or(Err(s))
        }
        let mut r = Vec::<u8>::new();
        let mut it = s.char_indices();
        while let Some((_, c)) = it.next() {
            if c != '\\' {
                push_char(&mut r, c);
                continue;
            }
            let it0 = it.clone();
            let Some((_, c)) = it.next() else {
                // This should be unreachable due to the grammar.
                return Err(self.err_at(it.as_str(), "unterminated escape sequence"));
            };
            let b: u8 = match c {
                'a' => b'\x07',
                'b' => b'\x08',
                'f' => b'\x0c',
                'n' => b'\n',
                'r' => b'\r',
                't' => b'\t',
                'v' => b'\x0b',
                'x' => take_hex(&mut it).map_err(|s| self.err_at(s, "bad hex escape sequence"))?,
                '0'..'8' => {
                    it = it0; // back up one character
                    take_oct(&mut it).map_err(|s| self.err_at(s, "bad octal escape sequence"))?
                }
                c => {
                    push_char(&mut r, c);
                    continue;
                }
            };
            r.push(b);
        }
        Ok(Cow::Owned(r))
    }
}

/// Evaluate an expression or parse a literal.
trait EvalExt {
    fn eval(&self) -> Result<u64, SourceError>;
}

impl EvalExt for IntLiteral<'_> {
    fn eval(&self) -> Result<u64, SourceError> {
        match self {
            IntLiteral::CharLiteral(c) => {
                let bytes = c.unescape()?;
                // This is a C 'char'; it has one byte.
                Ok(bytes[0].into())
            }
            IntLiteral::NumericLiteral(n) => n.eval(),
        }
    }
}

impl EvalExt for NumericLiteral<'_> {
    fn eval(&self) -> Result<u64, SourceError> {
        let s = self.str().trim_end_matches(['U', 'L']); // dtc is case-sensitive here
        parse_int(s).ok_or_else(|| self.err("bad numeric literal"))
    }
}

fn parse_int(s: &str) -> Option<u64> {
    if s == "0" {
        return Some(0);
    };
    let (digits, radix) = if let Some(hex) = s.strip_prefix("0x").or(s.strip_prefix("0X")) {
        (hex, 16)
    } else if let Some(oct) = s.strip_prefix('0') {
        (oct, 8)
    } else {
        (s, 10)
    };
    u64::from_str_radix(digits, radix).ok()
}

impl EvalExt for ParenExpr<'_> {
    fn eval(&self) -> Result<u64, SourceError> {
        self.expr.eval()
    }
}

impl EvalExt for Expr<'_> {
    fn eval(&self) -> Result<u64, SourceError> {
        self.ternaryprec.eval()
    }
}

impl EvalExt for UnaryExpr<'_> {
    fn eval(&self) -> Result<u64, SourceError> {
        let arg = self.unaryprec.eval()?;
        match self.unaryop {
            UnaryOp::LogicalNot(_) => Ok((arg == 0).into()),
            UnaryOp::BitwiseNot(_) => Ok(!arg),
            // Devicetree has only unsigned arithmetic, so negation is allowed to overflow.
            UnaryOp::Negate(_) => Ok(arg.wrapping_neg()),
        }
    }
}

impl EvalExt for TernaryPrec<'_> {
    fn eval(&self) -> Result<u64, SourceError> {
        let left = self.logicalorprec.eval()?;
        let [mid, right] = self.expr.as_slice() else {
            return Ok(left);
        };
        // Note that subexpression evaluation is lazy, unlike dtc.
        if left != 0 {
            mid.eval()
        } else {
            right.eval()
        }
    }
}

macro_rules! impl_binary_eval {
    ($rule:ident, $op:ident, $arg:ident) => {
        impl EvalExt for $rule<'_> {
            fn eval(&self) -> Result<u64, SourceError> {
                let mut left = self.$arg[0].eval();
                for (op, right) in core::iter::zip(self.$op, &self.$arg[1..]) {
                    let right = right.eval()?;
                    // It would be nice to match on the type of `op` rather than its text, but to
                    // get the compile-time safety of an exhaustive match, we'd need one match
                    // statement per precedence rule.
                    // TODO: could use UNTYPED_RULE?
                    left = eval_binary_op(left?, op.str(), right).map_err(|msg| self.err(msg));
                }
                left
            }
        }
    };
}

// TODO:  Should these short-circuit?
impl_binary_eval!(LogicalOrPrec, logicalor, logicalandprec);
impl_binary_eval!(LogicalAndPrec, logicaland, bitwiseorprec);

impl_binary_eval!(BitwiseOrPrec, bitwiseor, bitwisexorprec);
impl_binary_eval!(BitwiseXorPrec, bitwisexor, bitwiseandprec);
impl_binary_eval!(BitwiseAndPrec, bitwiseand, equalprec);
impl_binary_eval!(EqualPrec, equalprecop, compareprec);
impl_binary_eval!(ComparePrec, compareprecop, shiftprec);
impl_binary_eval!(ShiftPrec, shiftprecop, addprec);
impl_binary_eval!(AddPrec, addprecop, mulprec);
impl_binary_eval!(MulPrec, mulprecop, unaryprec);

impl EvalExt for UnaryPrec<'_> {
    fn eval(&self) -> Result<u64, SourceError> {
        match self {
            UnaryPrec::UnaryExpr(x) => x.eval(),
            UnaryPrec::ParenExpr(x) => x.eval(),
            UnaryPrec::IntLiteral(x) => x.eval(),
        }
    }
}

fn eval_binary_op(left: u64, op: &str, right: u64) -> Result<u64, &'static str> {
    fn check(checked_or_wrapping: Result<u64, u64>) -> Option<u64> {
        match checked_or_wrapping {
            Ok(checked) => Some(checked),
            Err(wrapping) => cfg!(feature = "wrapping-arithmetic").then_some(wrapping),
        }
    }
    fn add(a: u64, b: u64) -> Option<u64> {
        check(a.checked_add(b).ok_or(a.wrapping_add(b)))
    }
    fn sub(a: u64, b: u64) -> Option<u64> {
        check(a.checked_sub(b).ok_or(a.wrapping_sub(b)))
    }
    fn mul(a: u64, b: u64) -> Option<u64> {
        check(a.checked_mul(b).ok_or(a.wrapping_mul(b)))
    }
    fn shl(a: u64, b: u64) -> u64 {
        if b < 64 {
            a << b
        } else {
            0
        }
    }
    fn shr(a: u64, b: u64) -> u64 {
        if b < 64 {
            a >> b
        } else {
            0
        }
    }
    match op {
        "+" => add(left, right).ok_or("arithmetic overflow"),
        "-" => sub(left, right).ok_or("arithmetic overflow"),
        "*" => mul(left, right).ok_or("arithmetic overflow"),
        "<<" => Ok(shl(left, right)),
        ">>" => Ok(shr(left, right)),
        "/" => left.checked_div(right).ok_or("division by zero"),
        "%" => left.checked_rem(right).ok_or("division by zero"),
        "&" => Ok(left & right),
        "|" => Ok(left | right),
        "^" => Ok(left ^ right),
        "&&" => Ok((left != 0 && right != 0) as u64),
        "||" => Ok((left != 0 || right != 0) as u64),
        "<=" => Ok((left <= right) as u64),
        ">=" => Ok((left >= right) as u64),
        "<" => Ok((left < right) as u64),
        ">" => Ok((left > right) as u64),
        "==" => Ok((left == right) as u64),
        "!=" => Ok((left != right) as u64),
        _ => Err("unknown binary operator"),
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
    Ast(&'a PropValue<'a>),
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
            .ok_or_else(|| noderef.err("no such node"))
    }

    fn resolve_str(&self, noderef: &str) -> Option<NodePath> {
        let path = noderef.trim_matches(['&', '{', '}']);
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
    for childdef in contents.childdef {
        match childdef {
            ChildDef::ChildNode(childnode) => {
                let name = childnode.nodename.unescape_name();
                let child_path = path.join(name);
                let child = node.add_child(name);
                for childnodeprefix in childnode.childnodeprefix {
                    if let ChildNodePrefix::Label(label) = childnodeprefix {
                        add_label(node_labels, label, &child_path)?;
                    }
                }
                let contents = childnode.nodebody.nodecontents;
                fill_temp_node(node_labels, child, &child_path, contents)?;
            }
            ChildDef::DelNode(delnode) => {
                let name = delnode.nodename.unescape_name();
                node.remove_child(name);
                let childpath = path.join(name);
                node_labels.retain(|_, p| !p.starts_with(&childpath));
            }
        }
    }
    let mut names_used = std::collections::HashSet::new();
    for propdef in contents.propdef {
        match propdef {
            PropDef::Prop(prop) => {
                let name = prop.propname.unescape_name();
                if !names_used.insert(name) {
                    // dtc rejects this only during the first definition of a node.
                    // However, it seems sensible to reopen nodes at non-top level,
                    // but likely mistaken to redefine a property within a scope.
                    return Err(prop.propname.err("duplicate property"));
                }
                let value = match prop.propvalue {
                    Some(propvalue) => TempValue::Ast(propvalue),
                    None => TempValue::Bytes(vec![]),
                };
                node.set_property(name, value);
            }
            PropDef::DelProp(delprop) => {
                let name = delprop.propname.unescape_name();
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

#[test]
fn test_duplicate_property() {
    let source = include_str!("testdata/duplicate_property.dts");
    let arena = bumpalo::Bump::new();
    let dts = crate::parse::parse_typed(source, &arena).unwrap();
    let err = eval(dts).expect_err("evaluation should fail");
    let message = format!("{err}");
    assert!(
        message.contains("duplicate property") && message.contains("this time it's an error"),
        "unexpected error:\n{message}"
    );
}

#[test]
fn test_eval() {
    for source in [
        include_str!("testdata/charlit.dts"),
        include_str!("testdata/expr.dts"),
        #[cfg(feature = "wrapping-arithmetic")]
        include_str!("testdata/random_expressions.dts"),
    ] {
        let arena = bumpalo::Bump::new();
        let dts = crate::parse::parse_typed(source, &arena).unwrap();
        let tree = eval(dts).unwrap();
        for p in tree.properties {
            let name = &p.name;
            let v = &p.value;
            assert_eq!(
                v.len(),
                8,
                "property {name} has wrong shape; should be <expected computed>"
            );
            let left = u32::from_be_bytes(v[0..4].try_into().unwrap());
            let right = u32::from_be_bytes(v[4..8].try_into().unwrap());
            assert_eq!(
                left, right,
                "property {name:?} did not evaluate to two equal values"
            );
        }
    }
}
