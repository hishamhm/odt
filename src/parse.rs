//! Facilities for parsing DTS files and expanding "/include/" directives.

use crate::error::SourceError;
use crate::fs::Loader;
use core::ops::Range;
use pest_typed::TypedParser;
use pest_typed_derive::TypedParser;
use std::path::Path;

#[derive(TypedParser)]
#[grammar = "dts.pest"]
#[emit_rule_reference]
#[box_only_if_needed]
struct DtsParser;

// TODO:  The pest_typed_derive representation of choices is not great.
// You would expect a Rust enum, but the only enums are generic with variants named _0, _1, _2....
// See if pest3 does better.

pub use crate::parse::rules::Dts;
use crate::parse::rules::*;

pub fn parse(source: &str) -> Result<Dts, SourceError> {
    let dtsfile = DtsParser::try_parse::<DtsFile>(source)?;
    Ok(dtsfile.Dts().clone())
}

pub fn parse_with_includes<'a>(loader: &'a Loader, path: &'_ Path) -> Result<Dts<'a>, SourceError> {
    let Some((_, src)) = loader.read_utf8(path.into()) else {
        // TODO:  presumably there is some kind of filesystem error we could propagate
        return Err(SourceError::new_unattributed(format!(
            "can't load file {path:?}"
        )));
    };
    let dts = parse(src).map_err(|e| e.with_path(path))?;
    // make an empty container to receive the merged tree
    let mut out = dts.clone();
    out.content.0.matched.content.clear();
    out.content.1.matched.content.clear();
    out.content.2.matched.content.clear();
    out.content.3.matched.content.clear();
    visit_includes(loader, path, dts, &mut out)?;
    Ok(out)
}

fn visit_includes<'a>(
    loader: &'a Loader,
    path: &'_ Path,
    mut dts: Dts<'a>,
    out: &'_ mut Dts<'a>,
) -> Result<(), SourceError> {
    let dir = Some(path.parent().unwrap());
    for include in dts.Include() {
        let pathspan = include.QuotedString().trim_one();
        // The path is not unescaped in any way before use.
        let Some((ipath, src)) = loader.find_utf8(dir, Path::new(pathspan.as_str())) else {
            return Err(pathspan.err("can't find include file on search path"));
        };
        let dts = parse(src)?;
        visit_includes(loader, ipath, dts, out)?;
    }
    // accumulate fields into the output, other than Includes (tuple element 1)
    let it = dts.content.0.matched.content.drain(..);
    out.content.0.matched.content.extend(it);
    let it = dts.content.2.matched.content.drain(..);
    out.content.2.matched.content.extend(it);
    let it = dts.content.3.matched.content.drain(..);
    out.content.3.matched.content.extend(it);
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
        SourceError::new(message.into(), self.to_untyped_span())
    }
    fn err_at(&self, substr: &str, message: impl Into<String>) -> SourceError {
        let span = self.to_untyped_span();
        let range = substr_range(span.as_str(), substr);
        let span = range.map(|r| span.get(r).unwrap()).unwrap_or(span);
        SourceError::new(message.into(), span)
    }
    fn to_untyped_span(&self) -> pest::Span;
}

impl SpanExt for pest::Span<'_> {
    fn to_untyped_span(&self) -> pest::Span {
        self.clone()
    }
}

impl SpanExt for pest_typed::Span<'_> {
    fn to_untyped_span(&self) -> pest::Span {
        pest::Span::new(self.get_input(), self.start(), self.end()).unwrap()
    }
}

pub trait SpannedExt<'a, R: pest_typed::RuleType, T: pest_typed::Spanned<'a, R>> {
    fn err(&self, message: impl Into<String>) -> SourceError;
    fn str(&self) -> &'a str;
    fn trim_one(&self) -> pest_typed::Span<'a>;
}

impl<'a, R: pest_typed::RuleType, T: pest_typed::Spanned<'a, R>> SpannedExt<'a, R, T> for T {
    fn err(&self, message: impl Into<String>) -> SourceError {
        self.span().err(message.into())
    }
    fn str(&self) -> &'a str {
        self.span().as_str()
    }
    fn trim_one(&self) -> pest_typed::Span<'a> {
        let span = self.span();
        let n = span.as_str().len();
        assert!(n >= 2, "{}", self.err("no end chars to trim"));
        span.get(1..n - 1).unwrap()
    }
}
