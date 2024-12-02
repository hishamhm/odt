//! Generates random expressions to verify the implementation of operator precedence.
//!
//! Invoke with two arguments:
//! - the maximum number of tokens per expression
//! - the number of expressions to print (omit for all)
//!
//! Sequences with an ambiguous parse tree may be generated multiple times.  We could avoid that by
//! emitting tokens left to right while tracking the number of unclosed parentheses and ternary
//! operators.  But for randomized testing, it's fine if ambiguous expressions are overrepresented,
//! since those are the very cases where operator precedence matters.

use std::fmt::Write;

fn main() {
    let mut args = std::env::args().skip(1);
    let max_tokens = args
        .next()
        .map(|s| str::parse::<usize>(&s).unwrap())
        .unwrap_or(3);
    let max_examples = args
        .next()
        .map(|s| str::parse::<usize>(&s).unwrap())
        .unwrap_or(usize::MAX);

    let mut buf = String::new();

    if cfg!(test) {
        for tokens in 1..=max_tokens {
            let total = count(tokens);
            let mut seen = std::collections::HashSet::new();
            for i in 0..total {
                buf.clear();
                select(&mut buf, tokens, i);
                seen.insert(buf.clone());
            }
            let unique = seen.len();
            println!("for {tokens} tokens, {unique} unique of {total} total expressions");
        }
        return;
    }

    let total: usize = (1..=max_tokens).map(count).sum();
    let mut sampler = SampleState {
        to_consider: total,
        to_keep: max_examples,
    };

    println!("/dts-v1/;");
    println!();
    println!("/ {{");
    for tokens in 1..=max_tokens {
        let c = count(tokens);
        // Check that counter and generator agree on the boundary.
        assert!(select(&mut buf, tokens, c - 1));
        assert!(!select(&mut buf, tokens, c));
        print!("  expressions_with_{tokens}_tokens");
        let mut first = true;
        // This format is necessary for dts to process a large list quickly.
        // If we use separate properties, or comma-separated cell lists, it becomes quadratic.
        for i in 0..c {
            if sampler.kept() {
                buf.clear();
                assert!(select(&mut buf, tokens, i));
                if first {
                    print!(" = <");
                    first = false;
                }
                print!("\n    {i} ({buf})");
            }
        }
        if first {
            println!(";");
        } else {
            println!("\n  >;");
        }
    }
    println!("}};");
}

struct SampleState {
    to_consider: usize,
    to_keep: usize,
}

impl SampleState {
    fn kept(&mut self) -> bool {
        assert!(self.to_consider > 0);
        if rng() % self.to_consider < self.to_keep {
            self.to_consider -= 1;
            self.to_keep -= 1;
            true
        } else {
            self.to_consider -= 1;
            false
        }
    }
}

fn rng() -> usize {
    use std::hash::{BuildHasher, Hasher, RandomState};
    RandomState::new().build_hasher().finish() as usize
}

// TODO: pick a more interesting set of numbers here.
// or generate randomly at the leaves rather than enumerating these.
const LITERALS: &[&str] = &["0", "1"];
const UNARY_OPS: &[&str] = &["!", "~", "-"];
// TODO: add back division and modulus operators once we can filter out expressions that result in division by zero
const BINARY_OPS: &[&str] = &[
    "+", "-", "*", /*"/", "%",*/ "&&", "||", "&", "|", "^", "<<", ">>", "<=", ">=", "<", ">",
    "==", "!=",
];

fn count(tokens: usize) -> usize {
    if tokens == 0 {
        return 0;
    }
    if tokens == 1 {
        return LITERALS.len();
    }
    let mut r = 0;
    if tokens >= 2 {
        r += UNARY_OPS.len() * count(tokens - 1);
    }
    if tokens >= 3 {
        r += count(tokens - 2); // parenthesized expression
        for i in 1..tokens - 1 {
            r += count(i) * BINARY_OPS.len() * count(tokens - i - 1);
        }
    }
    if tokens >= 5 {
        for i in 1..tokens {
            for j in 1..tokens {
                if i + j + 2 < tokens {
                    r += count(i) * count(j) * count(tokens - i - j - 2); // ternary operator
                }
            }
        }
    }
    r
}

fn select(out: &mut dyn Write, tokens: usize, mut position: usize) -> bool {
    assert!(tokens > 0);
    if tokens == 1 {
        return position.within(LITERALS.len()) && emit(out, LITERALS, &mut position);
    }
    if tokens >= 2 {
        let n = count(tokens - 1);
        if position.within(UNARY_OPS.len() * n) {
            return emit(out, UNARY_OPS, &mut position) && select(out, tokens - 1, position);
        }
    }
    if tokens >= 3 {
        let n = count(tokens - 2);
        if position.within(n) {
            return emit_one(out, "(") && select(out, tokens - 2, position) && emit_one(out, ")");
        }
        for i in 1..tokens - 1 {
            let left = count(i);
            let right = count(tokens - i - 1);
            if position.within(left * BINARY_OPS.len() * right) {
                return select(out, i, position.divrem(left))
                    && emit(out, BINARY_OPS, &mut position)
                    && select(out, tokens - i - 1, position);
            }
        }
    }
    if tokens >= 5 {
        for i in 1..tokens {
            for j in 1..tokens {
                if i + j + 2 < tokens {
                    let left = count(i);
                    let mid = count(j);
                    let right = count(tokens - i - j - 2);
                    if position.within(left * mid * right) {
                        return select(out, i, position.divrem(left))
                            && emit_one(out, "?")
                            && select(out, j, position.divrem(mid))
                            && emit_one(out, ":")
                            && select(out, tokens - i - j - 2, position);
                    }
                }
            }
        }
    }
    false
}

fn emit(out: &mut dyn Write, choices: &[&str], position: &mut usize) -> bool {
    emit_one(out, choices[position.divrem(choices.len())])
}

fn emit_one(out: &mut dyn Write, s: &str) -> bool {
    out.write_str(s).is_ok()
}

trait NumberedPermutation {
    fn within(&mut self, count: Self) -> bool;
    fn divrem(&mut self, denom: Self) -> Self;
}

impl NumberedPermutation for usize {
    fn within(&mut self, count: usize) -> bool {
        if *self < count {
            return true;
        }
        *self -= count;
        false
    }
    fn divrem(&mut self, denom: usize) -> usize {
        let rem = *self % denom;
        *self /= denom;
        rem
    }
}
