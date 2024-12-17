//! Facilities for evaluating expressions and phandle references in a devicetree.

use crate::error::SourceError;
use crate::label::{LabelMap, LabelResolver};
use crate::nodepath::NodePath;
use crate::parse::gen::*;
use crate::parse::{SpanExt, TypedRuleExt};
use crate::{BinaryNode, SourceNode};
use core::str::CharIndices;
use hashlink::{LinkedHashMap, LinkedHashSet};
use std::borrow::Cow;

/// Assigns phandles and evaluates expressions.
/// Call with the output of `crate::merge::merge()`.
pub fn eval(tree: SourceNode, node_labels: LabelMap) -> Result<BinaryNode, SourceError> {
    let phandles = assign_phandles(&tree, &node_labels)?;
    let mut tree = evaluate_expressions(tree, &node_labels, &phandles)?;
    // poke assigned phandle values into the final tree
    for (nodepath, phandle) in phandles {
        tree.walk_mut(nodepath.segments())
            .unwrap()
            .set_property("phandle", phandle.to_be_bytes().into());
    }
    Ok(tree)
}

type PhandleMap = LinkedHashMap<NodePath, u32>;

fn assign_phandles(root: &SourceNode, node_labels: &LabelMap) -> Result<PhandleMap, SourceError> {
    let mut need_phandles = LinkedHashSet::<NodePath>::new();
    visit_phandle_references(
        &LabelResolver(node_labels, root),
        root,
        &NodePath::root(),
        &mut need_phandles,
    )?;
    // TODO:  Walk the tree to find existing phandle properties.
    // Each expression must have length 4, and may contain zero phandle references, or one,
    // pointing to itself.  Scalar values are skipped during assignment.
    let mut phandle_counter = 0u32;
    let mut phandles = PhandleMap::new();
    for path in need_phandles {
        phandle_counter += 1;
        phandles.insert(path, phandle_counter);
    }
    Ok(phandles)
}

fn visit_phandle_references<P>(
    labels: &LabelResolver<P>,
    node: &SourceNode,
    path: &NodePath,
    need_phandles: &mut LinkedHashSet<NodePath>,
) -> Result<(), SourceError> {
    for (_, prop) in node.properties() {
        if let Some(propvalue) = prop.prop_value {
            for labeled_value in propvalue.labeled_value {
                if let Value::Cells(cells) = labeled_value.value {
                    for label_or_cell in cells.label_or_cell {
                        if let LabelOrCell::Cell(Cell::NodeReference(phandle)) = label_or_cell {
                            let target = labels.resolve(phandle)?;
                            need_phandles.replace(target);
                        }
                    }
                }
            }
        }
    }
    for (name, tempnode) in node.children() {
        let child_path = path.join(name);
        visit_phandle_references(labels, tempnode, &child_path, need_phandles)?;
    }
    Ok(())
}

fn evaluate_expressions(
    root: SourceNode,
    node_labels: &LabelMap,
    phandles: &PhandleMap,
) -> Result<BinaryNode, SourceError> {
    let old = root.clone();
    let labels = LabelResolver(node_labels, &old);
    let eval = |value: &Prop| match value.prop_value {
        None => Ok(vec![]),
        Some(propvalue) => evaluate_propvalue(propvalue, &labels, phandles),
    };
    root.map_values(&eval)
}

fn evaluate_propvalue<P>(
    propvalue: &PropValue,
    labels: &LabelResolver<P>,
    phandles: &PhandleMap,
) -> Result<Vec<u8>, SourceError> {
    let mut r = vec![];
    for labeled_value in propvalue.labeled_value {
        match labeled_value.value {
            Value::Cells(cells) => {
                let bits = match cells.bits {
                    None => 32,
                    Some(bits) => {
                        let n = bits.numeric_literal.eval()?;
                        match n {
                            8 | 16 | 32 | 64 => n,
                            _ => return Err(bits.err("bad bit width: must be 8, 16, 32, or 64")),
                        }
                    }
                };
                for label_or_cell in cells.label_or_cell {
                    let LabelOrCell::Cell(cell) = label_or_cell else {
                        continue;
                    };
                    let n = match cell {
                        Cell::NodeReference(noderef) => {
                            // Any errors should have been reported during `assign_phandles()`.
                            let path = labels.resolve(noderef).unwrap();
                            let phandle: u32 = *phandles.get(&path).unwrap();
                            if bits != 32 {
                                return Err(noderef.err("phandle references need /bits/ == 32"));
                            }
                            phandle as u64
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
                        64 => r.extend(n.to_be_bytes()),
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
                let target = labels.resolve(noderef)?;
                r.extend(target.display().as_bytes());
                r.push(0);
            }
            Value::ByteString(bytestring) => {
                for label_or_hex_byte in bytestring.label_or_hex_byte {
                    if let LabelOrHexByte::HexByte(hex_byte) = label_or_hex_byte {
                        let s = hex_byte.str();
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
        self.ternary_prec.eval()
    }
}

impl EvalExt for UnaryExpr<'_> {
    fn eval(&self) -> Result<u64, SourceError> {
        let arg = self.unary_prec.eval()?;
        match self.unary_op {
            UnaryOp::LogicalNot(_) => Ok((arg == 0).into()),
            UnaryOp::BitwiseNot(_) => Ok(!arg),
            // Devicetree has only unsigned arithmetic, so negation is allowed to overflow.
            UnaryOp::Negate(_) => Ok(arg.wrapping_neg()),
        }
    }
}

impl EvalExt for TernaryPrec<'_> {
    fn eval(&self) -> Result<u64, SourceError> {
        let left = self.logical_or_prec.eval()?;
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
                    left = eval_binary_op(left?, op.str(), right).map_err(|msg| self.err(msg));
                }
                left
            }
        }
    };
}

// TODO:  Should these short-circuit?
impl_binary_eval!(LogicalOrPrec, logical_or, logical_and_prec);
impl_binary_eval!(LogicalAndPrec, logical_and, bitwise_or_prec);

impl_binary_eval!(BitwiseOrPrec, bitwise_or, bitwise_xor_prec);
impl_binary_eval!(BitwiseXorPrec, bitwise_xor, bitwise_and_prec);
impl_binary_eval!(BitwiseAndPrec, bitwise_and, equal_prec);
impl_binary_eval!(EqualPrec, equal_prec_op, compare_prec);
impl_binary_eval!(ComparePrec, compare_prec_op, shift_prec);
impl_binary_eval!(ShiftPrec, shift_prec_op, add_prec);
impl_binary_eval!(AddPrec, add_prec_op, mul_prec);
impl_binary_eval!(MulPrec, mul_prec_op, unary_prec);

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

#[test]
fn test_eval() {
    for source in [
        include_str!("testdata/charlit.dts"),
        include_str!("testdata/expr.dts"),
        include_str!("testdata/phandle.dts"),
        #[cfg(feature = "wrapping-arithmetic")]
        include_str!("testdata/random_expressions.dts"),
    ] {
        let arena = bumpalo::Bump::new();
        let dts = crate::parse::parse_typed(source, &arena).unwrap();
        let (tree, node_labels) = crate::merge::merge(&dts).unwrap();
        let tree = eval(tree, node_labels).unwrap();
        let check = tree.get_child("check").unwrap_or(&tree);
        for (name, value) in check.properties() {
            assert_eq!(
                value.len(),
                8,
                "property {name} has wrong shape; should be <expected computed>"
            );
            let left = u32::from_be_bytes(value[0..4].try_into().unwrap());
            let right = u32::from_be_bytes(value[4..8].try_into().unwrap());
            assert_eq!(
                left, right,
                "property {name} did not evaluate to two equal values"
            );
        }
    }
}
