//! Facilities for parsing DTS files.
//!
//! This module exposes pest's untyped grammar, which is more convenient for
//! clients wishing to visit every character of the input.

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "dts.pest"]
pub struct DtsParser;

pub type Tree<'a> = Pair<'a, Rule>;

pub fn parse(source: &str) -> Result<Tree, pest::error::Error<Rule>> {
    let mut dtsfile = DtsParser::parse(Rule::DtsFile, source)?;
    Ok(dtsfile.next().unwrap())
}
