//! Facilities for parsing DTS files and expanding "/include/" directives.

use crate::error::SourceError;
use crate::fs::Loader;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use core::ops::Range;
use gen::{Dts, DtsFile};
use pest::iterators::Pair;
use pest::{Parser, Span};
use std::path::Path;

#[derive(pest_derive::Parser, pestle::TypedRules)]
#[grammar = "src/dts.pest"]
#[typed_mod = "gen"]
pub struct DtsParser;

/// This is pest's untyped grammar, which is convenient for clients that
/// wish to visit every character of the input.
pub type Tree<'a> = Pair<'a, Rule>;

pub fn parse_untyped(source: &str) -> Result<Tree, SourceError> {
    let mut it = DtsParser::parse(Rule::DtsFile, source)?;
    let dtsfile = it.next().unwrap();
    assert_eq!(dtsfile.as_rule(), Rule::DtsFile);
    assert_eq!(it.next(), None);
    Ok(dtsfile)
}

pub fn parse_typed<'i>(source: &'i str, arena: &'i Bump) -> Result<&'i Dts<'i>, SourceError> {
    let tree = parse_untyped(source)?;
    let dtsfile = DtsFile::build(tree, arena);
    Ok(dtsfile.dts)
}

pub fn parse_with_includes<'a>(loader: &'a Loader, path: &'_ Path) -> Result<Dts<'a>, SourceError> {
    let Some((_, src)) = loader.read_utf8(path.into()) else {
        // TODO:  presumably there is some kind of filesystem error we could propagate
        return Err(SourceError::new_unattributed(format!(
            "can't load file {path:?}"
        )));
    };
    let dts = parse_typed(src, &loader.arena).map_err(|e| e.with_path(path))?;
    // make an empty container to receive the merged tree
    let mut top_def = Vec::new_in(&loader.arena);
    visit_includes(loader, path, dts, &mut top_def)?;
    let out = Dts {
        top_def: loader.arena.alloc(top_def),
        ..*dts
    };
    Ok(out)
}

fn visit_includes<'a>(
    loader: &'a Loader,
    path: &'_ Path,
    dts: &'_ Dts<'a>,
    out: &'_ mut Vec<&'a gen::TopDef<'a>>,
) -> Result<(), SourceError> {
    let dir = Some(path.parent().unwrap());
    for include in dts.include {
        let pathspan = include.quoted_string.trim_one();
        // The path is not unescaped in any way before use.
        let Some((ipath, src)) = loader.find_utf8(dir, Path::new(pathspan.as_str())) else {
            return Err(pathspan.err("can't find include file on search path"));
        };
        let dts = parse_typed(src, &loader.arena)?;
        visit_includes(loader, ipath, dts, out)?;
    }
    if let Some(memres) = dts.memreserve.first() {
        unimplemented!("{}", memres.err("unimplemented"));
    }
    out.extend(dts.top_def);
    Ok(())
}

// Implements the unstable method `str::substr_range()`.
fn substr_range(outer: &str, inner: &str) -> Option<Range<usize>> {
    let outer = outer.as_bytes().as_ptr_range();
    let outer = outer.start as usize..outer.end as usize;
    let inner = inner.as_bytes().as_ptr_range();
    let inner = inner.start as usize..inner.end as usize;
    if outer.start <= inner.start && outer.end >= inner.end {
        Some(inner.start - outer.start..inner.end - outer.start)
    } else {
        None
    }
}

pub trait SpanExt {
    fn err(&self, message: impl Into<String>) -> SourceError {
        SourceError::new(message.into(), self.span())
    }
    fn err_at(&self, substr: &str, message: impl Into<String>) -> SourceError {
        let span = self.span();
        let range = substr_range(span.as_str(), substr);
        let span = range.map(|r| span.get(r).unwrap()).unwrap_or(span);
        SourceError::new(message.into(), span)
    }
    fn span(&self) -> Span;
}

impl SpanExt for Span<'_> {
    fn span(&self) -> Span {
        *self
    }
}

pub trait TypedRuleExt<'a> {
    fn err(&self, message: impl Into<String>) -> SourceError;
    fn str(&self) -> &'a str;
    fn trim_one(&self) -> Span<'a>;
}

impl<'a, T: gen::TypedRule<'a>> TypedRuleExt<'a> for T {
    fn err(&self, message: impl Into<String>) -> SourceError {
        self.span().err(message.into())
    }
    fn str(&self) -> &'a str {
        self.span().as_str()
    }
    fn trim_one(&self) -> Span<'a> {
        let span = self.span();
        let n = span.as_str().len();
        assert!(n >= 2, "{}", self.err("no end chars to trim"));
        span.get(1..n - 1).unwrap()
    }
}
