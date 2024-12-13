//! Generates random expressions to verify the implementation of operator precedence.
//!
//! Invoke with two arguments:
//! - the maximum number of tokens per expression
//! - the number of expressions to print per token count (omit for all)
//!
//! Sequences with an ambiguous parse tree may be generated multiple times.  We could avoid that by
//! emitting tokens left to right while tracking the number of unclosed parentheses and ternary
//! operators.  But for randomized testing, it's fine if ambiguous expressions are overrepresented,
//! since those are the very cases where operator precedence matters.

use std::collections::HashSet;
use std::fmt::Write;

fn main() {
    let mut args = std::env::args().skip(1);
    let tokens = args
        .next()
        .map(|s| str::parse::<usize>(&s).unwrap())
        .unwrap_or(3);
    let max_examples = args
        .next()
        .map(|s| str::parse::<usize>(&s).unwrap())
        .unwrap_or(usize::MAX);

    let total = count(tokens, false);
    let mut buf = String::new();

    let mut cache = [[0usize; 2]; 100];
    for i in 0..=tokens {
        cache[i][0] = count(i, false);
        cache[i][1] = count(i, true);
    }
    let precount = &|tokens: usize, mask_unary: bool| {
        cache[tokens][mask_unary as usize]
    };

    if max_examples == 0 {
        let mut seen = HashSet::new();
        for i in 0..total {
            buf.clear();
            select(&mut buf, precount, tokens, false, i);
            if !seen.insert(buf.clone()) {
                println!("  duplicate: {buf}");
            }
        }
        let unique = seen.len();
        println!("for {tokens} tokens, {unique} unique of {total} total expressions");
        return;
    }

    // Check that counter and generator agree on the boundary.
    assert!(select(&mut buf, precount, tokens, false, total - 1));
    assert!(!select(&mut buf, precount, tokens, false, total));

    let mut sampler = SampleState {
        to_consider: total,
        to_keep: max_examples,
    };

    println!("/dts-v1/;");
    println!();
    println!("/ {{");
    if max_examples > 10000 {
        // This format is necessary for dtc to process a large list quickly.
        // If we use separate properties, or comma-separated cell lists, it becomes quadratic.
        print!("  expressions_with_{tokens}_tokens = <");
        for i in 0..total {
            if sampler.kept() {
                buf.clear();
                assert!(select(&mut buf, precount, tokens, false, i));
                print!("\n    {i} ({buf})");
            }
        }
        println!("\n  >;");
    } else {
        // This format is easy to pass through dtc in order to construct test input.
        let width = total.ilog10() as usize + 1;
        let mut sampled = HashSet::<usize>::new();
        while sampled.len() < max_examples.min(total) {
            sampled.insert(rng() % total);
        }
        let mut sampled = sampled.into_iter().collect::<Vec<usize>>();
        sampled.sort();
        for i in sampled {
            buf.clear();
            assert!(select(&mut buf, precount, tokens, false, i));
            println!("  tokens_{tokens}_expr_{i:0width$} = <({buf})>, \"({buf})\";");
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

    // TODO: skip ahead using hypergeometric distribution
}

fn rng() -> usize {
    use std::hash::{BuildHasher, Hasher, RandomState};
    RandomState::new().build_hasher().finish() as usize
}

const UNARY_OPS: &[&str] = &["!", "~", "-"];
// TODO: add back division and modulus operators once we can filter out expressions that result in division by zero
const BINARY_OPS: &[&str] = &[
    "+", "-", "*", /*"/", "%",*/ "&&", "||", "&", "|", "^", "<<", ">>",
    /*"<=", ">=",*/ "<", ">", "==", "!=",
];

// TODO: memoize here too
fn count(tokens: usize, mask_unary: bool) -> usize {
    if tokens == 0 {
        return 0;
    }
    if tokens == 1 {
        return 1;
    }
    let mut r = 0;
    if tokens >= 2 && !mask_unary {
        r += UNARY_OPS.len() * count(tokens - 1, true);
    }
    if tokens >= 3 {
        r += count(tokens - 2, false); // parenthesized expression
        for i in 1..tokens - 1 {
            r += count(i, false) * BINARY_OPS.len() * count(tokens - i - 1, false);
        }
    }
    if tokens >= 5 {
        for i in 1..tokens {
            for j in 1..tokens {
                if i + j + 2 < tokens {
                    r += count(i, false) * count(j, false) * count(tokens - i - j - 2, false); // ternary operator
                }
            }
        }
    }
    r
}

fn select(out: &mut dyn Write, count: &dyn Fn(usize, bool) -> usize, tokens: usize, mask_unary: bool, mut position: usize) -> bool {
    assert!(tokens > 0);
    if tokens == 1 {
        return position == 0 && emit_rand(out);
    }
    if tokens >= 2 && !mask_unary {
        let n = count(tokens - 1, true);
        if position.within(UNARY_OPS.len() * n) {
            return emit(out, UNARY_OPS, &mut position) && select(out, count, tokens - 1, true, position);
        }
    }
    if tokens >= 3 {
        let n = count(tokens - 2, false);
        if position.within(n) {
            return emit_one(out, "(") && select(out, count, tokens - 2, false, position) && emit_one(out, ")");
        }
        for i in 1..tokens - 1 {
            let left = count(i, false);
            let right = count(tokens - i - 1, false);
            if position.within(left * BINARY_OPS.len() * right) {
                return select(out, count, i, false, position.divrem(left))
                    && emit(out, BINARY_OPS, &mut position)
                    && select(out, count, tokens - i - 1, false, position);
            }
        }
    }
    if tokens >= 5 {
        for i in 1..tokens {
            for j in 1..tokens {
                if i + j + 2 < tokens {
                    let left = count(i, false);
                    let mid = count(j, false);
                    let right = count(tokens - i - j - 2, false);
                    if position.within(left * mid * right) {
                        return select(out, count, i, false, position.divrem(left))
                            && emit_one(out, "?")
                            && select(out, count, j, false, position.divrem(mid))
                            && emit_one(out, ":")
                            && select(out, count, tokens - i - j - 2, false, position);
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

fn emit_rand(out: &mut dyn Write) -> bool {
    write!(out, "{}", rng() % 10).is_ok()
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
