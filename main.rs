use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::error::Error;

#[derive(Parser)]
#[grammar = "dts.pest"]
struct DtsParser;

fn main() -> Result<(), Box<dyn Error>> {
    let filename = std::env::args().nth(1).unwrap();
    let source = std::fs::read_to_string(filename)?;
    let dts = match DtsParser::parse(Rule::dtsfile, &source) {
        Ok(mut dts) => dts.next().unwrap(),
        Err(e) => panic!("parsing failed:\n{}", e.renamed_rules(|rule| format!("{:?}", rule))),
    };
    // println!("{dts:#?}");
    prettyprint(dts, 0);
    Ok(())
}

// TODO:  looks like we have to name every string literal to get them back?

fn prettyprint(p: Pair<Rule>, indent: usize) {
    // let r = format!("{:?}", p.as_rule());
    let rule = p.as_rule();
    let s = p.as_str();
    let mut it = p.into_inner();
    if !s.contains('\n') {
        let rule = format!("{rule:?}");
        println!("/* {rule:-12} */  {:indent$} {s}", "");
        return;
    };
    // if it.len() == 0 { println!("{:indent$}{rule:?} {s}", ""); }
    let indent = indent + match rule {
        Rule::nodebody => 4,
        _ => 0,
    };
    for p in it {
        prettyprint(p, indent);
    }
    // for t in p.tokens() { println!("{t:?}"); }
}
