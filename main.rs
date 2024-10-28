use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::fmt::Write;

#[derive(Parser)]
#[grammar = "dts.pest"]
struct DtsParser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = std::env::args().nth(1).unwrap();
    let source = std::fs::read_to_string(filename)?;
    let dts = match DtsParser::parse(Rule::dtsfile, &source) {
        Ok(mut dts) => dts.next().unwrap(),
        Err(e) => panic!(
            "parsing failed:\n{}",
            e.renamed_rules(|rule| format!("{:?}", rule))
        ),
    };
    // println!("{dts:#?}");
    let mut pretty = PrettyPrinter::new();
    pretty.print(dts);
    print!("{}", pretty.out.buffer);
    Ok(())
}

#[derive(Default)]
struct IndentingWriter {
    buffer: String,
    indent: usize,
}

impl IndentingWriter {
    fn indent(&mut self, delta: isize) {
        let new = self.indent as isize + delta;
        assert!(new >= 0);
        self.indent = new as usize;
    }
}

impl Write for IndentingWriter {
    fn write_str(&mut self, input: &str) -> std::fmt::Result {
        let mut first = true;
        for line in input.split('\n') {
            if !first {
                self.buffer.write_char('\n')?;
            }
            first = false;
            if line.is_empty() {
                continue;
            }
            if self.buffer.chars().rev().next().unwrap_or('\n') == '\n' {
                write!(self.buffer, "{:1$}", "", self.indent)?;
            }
            self.buffer.write_str(line)?;
            // TODO: trim left spaces of each input line?
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

    // TODO: extra blank line after last include
    // TODO: need to capture all newlines in the input
    //         otherwise don't know where to attach comments
    //         could infer them from Positions?
    // TODO: defer printing newlines until comments have been processed

    fn print(&mut self, p: Pair<Rule>) {
        let rule = p.as_rule();
        let token = p.as_str();
        let it = p.into_inner();
        if it.len() == 0 {
            let last = self.last;
            self.last = rule;
            let separator = match (last, rule) {
                (Rule::EOI, _) => "",
                (_, Rule::semicolon) => "",
                (_, Rule::comma) => "",
                (_, Rule::close_cells) => "",
                (Rule::open_cells, _) => "",
                (Rule::line_comment, _) => "\n",
                // (_, Rule::line_comment) => " /**/ ",  // XXX
                (Rule::open_node, _) => "\n",
                (Rule::semicolon, _) => "\n",
                (Rule::comma, _) => "\n",
                _ => " ",
            };
            write!(self.out, "{separator}{token}").unwrap();
            return;
        }
        let indent = match rule {
            Rule::nodecontents => self.tabstop,
            Rule::propvalue => self.tabstop,
            _ => 0,
        };
        self.out.indent(indent);
        for p in it {
            self.print(p);
        }
        self.out.indent(-indent);
        match rule {
            Rule::version | Rule::include | Rule::topnode => write!(self.out, "\n").unwrap(),
            _ => (),
        }
    }
}
