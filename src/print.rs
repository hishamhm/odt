use crate::parse_untyped::{Rule, Tree};
use std::fmt::Write;

pub fn format(dts: Tree) -> String {
    let mut pretty = PrettyPrinter::new();
    pretty.print(dts);
    pretty.out.buffer
}

#[derive(Default)]
struct IndentingWriter {
    buffer: String,
    indent: usize,
    // The number of newlines we should emit before the next non-comment token.
    pending_newlines: usize,
}

impl IndentingWriter {
    fn indent(&mut self, delta: isize) {
        let new = self.indent as isize + delta;
        assert!(new >= 0);
        self.indent = new as usize;
    }
    fn ensure_following_lines(&mut self, lines: usize) {
        self.pending_newlines = self.pending_newlines.max(lines);
    }
    fn ensure_lines(&mut self) {
        while self.pending_newlines > 0 {
            self.push('\n');
        }
    }
    fn ensure_space(&mut self) {
        if !self
            .buffer
            .chars()
            .rev()
            .next()
            .unwrap_or('\n')
            .is_ascii_whitespace()
        {
            self.push(' ');
        }
    }
    fn push(&mut self, c: char) {
        self.write_char(c).unwrap();
    }
}

impl Write for IndentingWriter {
    fn write_str(&mut self, input: &str) -> std::fmt::Result {
        let mut first = true;
        for line in input.split('\n') {
            if !first {
                self.buffer.write_char('\n')?;
                self.pending_newlines = self.pending_newlines.saturating_sub(1);
            }
            first = false;
            if line.is_empty() {
                continue;
            }
            if self.buffer.chars().rev().next().unwrap_or('\n') == '\n' {
                write!(self.buffer, "{:1$}", "", self.indent)?;
            }
            self.buffer.write_str(line)?;
            // We could discard any spaces we didn't produce at the start of each line.
            // That might be simpler than `ensure_space()`, but it would reformat inside comments.
        }
        Ok(())
    }
}

struct PrettyPrinter {
    tabstop: isize,
    out: IndentingWriter,
    last: Rule,
}

impl PrettyPrinter {
    fn new() -> Self {
        Self {
            tabstop: 4,
            out: IndentingWriter::default(),
            last: Rule::EOI,
        }
    }

    fn print(&mut self, p: Tree) {
        let rule = p.as_rule();
        let text = p.as_str();
        if rule == Rule::WHITESPACE {
            if text.contains('\n') {
                // TODO:  Rely less on the input formatting.
                // XXX
                // still seem to need two pieces of information:
                //   has the input had a line break since the last token
                //   does our model want to insert a line break
                // trying to fix topnode(;) -> \n \n \n
                //       and fix a;\n//comment -> a; //comment
                // need to write an actual test now.
                self.out.ensure_following_lines(1);
                self.out.ensure_lines();
            }
            // don't update self.last
            return;
        }
        let it = p.into_inner();
        if it.len() == 0 {
            let last = self.last;
            self.last = rule;
            let prepend_whitespace = match (last, rule) {
                (Rule::EOI, _) => false,
                (_, Rule::Semicolon) => false,
                (_, Rule::Comma) => false,
                (_, Rule::CloseCells) => false,
                (Rule::OpenCells, _) => false,
                _ => true,
            };
            // TODO:  Is there a better way to determine if we're in a COMMENT rule?
            let comment = match rule {
                Rule::BlockComment => true,
                Rule::LineComment => true,
                _ => false,
            };
            // Only a comment on the same line is allowed to linger when a line break is pending.
            // TODO:  "Same line" is implemented by WHITESPACE above.  Check more directly?
            if !comment {
                self.out.ensure_lines();
            }
            if prepend_whitespace {
                self.out.ensure_space();
            }
            write!(self.out, "{text}").unwrap();
            self.out.ensure_following_lines(match rule {
                Rule::OpenNode | Rule::Semicolon => 1,
                _ => 0,
            });
            return;
        }
        let indent = match rule {
            Rule::NodeContents => self.tabstop,
            Rule::PropValue => self.tabstop,
            _ => 0,
        };
        self.out.indent(indent);
        for p in it {
            self.print(p);
        }
        self.out.indent(-indent);
        self.out.ensure_following_lines(match rule {
            Rule::Version => 2,
            Rule::Include => 1,
            Rule::TopNode => 2,
            _ => 0,
        });
    }
}

#[cfg(test)]
fn split_testdata(text: &str) -> Vec<String> {
    text.split_inclusive('\n')
        .collect::<Vec<_>>()
        .split(|&s| s.starts_with("--"))
        .map(|v| v.concat())
        .filter(|v| !v.is_empty())
        .collect()
}

#[test]
fn test_format() {
    for (index, (input, expected)) in std::iter::zip(
        split_testdata(include_str!("testdata/format.in")),
        split_testdata(include_str!("testdata/format.out")),
    )
    .enumerate()
    {
        let ast = crate::parse_untyped::parse(&input);
        let formatted = format(ast);
        // to renumber input:
        // print!("-- {index}\n{input}");
        // to regenerate expected output:
        // print!("-- {index}\n{formatted}");
        pretty_assertions::assert_eq!(
            formatted,
            expected,
            "formatted output for testcase {index} differs from expected"
        );
    }
}
