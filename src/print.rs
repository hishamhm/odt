use crate::parse_untyped::{Rule, Tree};
use core::fmt::Write;

pub fn format(dts: Tree) -> String {
    let mut pretty = PrettyPrinter::new();
    pretty.print(dts, Rule::EOI, Rule::EOI);
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
    fn ensure_line(&mut self) {
        if self.buffer.chars().next_back().unwrap_or('\n') != '\n' {
            self.push('\n');
        }
    }
    fn ensure_space(&mut self) {
        if !self
            .buffer
            .chars()
            .next_back()
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
    fn write_str(&mut self, input: &str) -> core::fmt::Result {
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
            if self.buffer.chars().next_back().unwrap_or('\n') == '\n' {
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
    seen_lines: usize,
}

impl PrettyPrinter {
    fn new() -> Self {
        Self {
            tabstop: 4,
            out: IndentingWriter::default(),
            last: Rule::EOI,
            seen_lines: 0,
        }
    }

    fn print(&mut self, p: Tree, parent: Rule, next_sibling: Rule) {
        let last = self.last;
        let rule = p.as_rule();
        let text = p.as_str();

        if rule == Rule::EOI {
            self.out.ensure_line();
            return;
        }

        if rule == Rule::WHITESPACE {
            self.seen_lines += text.chars().filter(|&c| c == '\n').count();
            return;
        }

        // Adjust indentation.  (Adjusting as we enter and exit interior rules would be simpler,
        // but because the grammar accepts comments anywhere, that will not correctly reindent
        // comments.)
        let indent = match (parent, rule) {
            (_, Rule::OpenNode) => 1,
            (_, Rule::CloseNode) => -1,
            (Rule::Prop, Rule::PropName) => 1,
            (Rule::Prop, Rule::Semicolon) => -1,
            _ => 0,
        };

        // Unindent this token and following tokens.
        if indent < 0 {
            self.out.indent(indent * self.tabstop);
        }

        let children: Vec<Tree> = p.into_inner().collect();
        if !children.is_empty() {
            // This is an interior rule; visit each child in turn.
            let mut it = children.iter();
            while let Some(p) = it.next() {
                // Check if the next token (other than whitespace or comments) begins
                // a ChildNode or TopNode.
                let mut next = it
                    .clone()
                    .find(|p| !matches!(p.as_rule(), Rule::WHITESPACE | Rule::COMMENT))
                    .cloned();
                let mut next_rule = Rule::EOI;
                while let Some(p) = next {
                    next_rule = p.as_rule();
                    if matches!(next_rule, Rule::ChildNode | Rule::TopNode) {
                        break;
                    }
                    next = p.into_inner().next(); // left recurse
                }
                self.print(p.clone(), rule, next_rule);
            }
        } else {
            // This is a leaf rule with no children; print it.  (The grammar associates all text
            // with such rules, so this is enough to reproduce the input.)
            self.last = rule;

            // Preserve up to two newlines around comments.
            let keep_lines = match (last, rule) {
                (Rule::LineComment, _) => 1, // the line comment embeds one newline
                (_, Rule::LineComment) => 2,
                (Rule::BlockComment, _) => 2,
                (_, Rule::BlockComment) => 2,
                _ => 0,
            };
            let lines = self.seen_lines.min(keep_lines);
            self.seen_lines = 0;
            if lines > 0 {
                self.out.ensure_following_lines(lines);
                self.out.ensure_lines();
            };

            // Determine whether to insert horizontal space based on the last token printed.
            let prepend_space = match (last, rule) {
                (Rule::EOI, _) => false,
                (_, Rule::Semicolon) => false,
                (_, Rule::Comma) => false,
                (_, Rule::CloseCells) => false,
                (Rule::OpenCells, _) => false,
                (_, Rule::CloseParen) => false,
                (Rule::OpenParen, _) => false,
                _ => true,
            };

            // Only a comment on the same line is allowed to linger when a line break is pending.
            if !matches!(rule, Rule::BlockComment | Rule::LineComment) {
                self.out.ensure_lines();
            }
            if prepend_space {
                self.out.ensure_space();
            }

            // TODO:  Align "<" and "//" with previous output line.  Might require lookahead.

            // TODO:  This reindents the interior lines of a block comment incorrectly.

            write!(self.out, "{text}").unwrap();
        }

        // Indent following tokens but not this token.
        if indent > 0 {
            self.out.indent(indent * self.tabstop);
        }

        // Insert vertical whitespace between some pairings of adjacent rules.
        let lines = match (rule, next_sibling) {
            (Rule::Version, _) => 2,
            (Rule::OpenNode, _) => 1,
            (Rule::COMMENT, Rule::ChildNode) => 1,
            (_, Rule::ChildNode) => 2,
            (Rule::COMMENT, Rule::TopNode) => 1,
            (_, Rule::TopNode) => 2,
            (Rule::TopDef, _) => 2,
            (Rule::Include, _) => 1,
            (Rule::Semicolon, _) => 1,
            _ => 0,
        };
        self.out.ensure_following_lines(lines);
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
    for (index, (input, expected)) in core::iter::zip(
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
