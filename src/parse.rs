//! Facilities for parsing DTS files and expanding "/include/" directives.

use pest_typed::TypedParser;
use pest_typed_derive::TypedParser;

#[derive(TypedParser)]
#[grammar = "dts.pest"]
#[emit_rule_reference]
#[box_only_if_needed]
struct DtsParser;

pub use crate::parse::rules::Dts;
use crate::parse::rules::*;

// TODO: expose errors
pub fn parse(source: &str) -> Dts {
    match DtsParser::try_parse::<DtsFile>(&source) {
        Ok(dtsfile) => dtsfile.Dts().clone(),
        // TODO: evaluate quality of error messages from this
        Err(err) => panic!(
            "parsing failed:\n{}",
            err.renamed_rules(|rule| format!("{:?}", rule))
        ),
    }
}

use crate::fs::IncludeLoader;
use std::path::Path;

#[derive(Debug)]
pub struct ParseError(String);
// TODO: need a Span in this error

impl core::fmt::Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "ParseError({:?})", self.0)
    }
}

impl std::error::Error for ParseError {}

pub fn parse_with_includes<'a>(
    loader: &'a IncludeLoader,
    path: &'_ Path,
) -> Result<Dts<'a>, ParseError> {
    let Some((_, src)) = loader.read_utf8(path.to_owned()) else {
        return Err(ParseError(format!("can't read {path:?}")));
    };
    let dts = parse(src);
    // make an empty container to receive the merged tree
    let mut out = dts.clone();
    out.content.0.matched.content.clear();
    out.content.1.matched.content.clear();
    out.content.2.matched.content.clear();
    out.content.3.matched.content.clear();
    _visit_includes(loader, path, dts, &mut out)?;
    Ok(out)
}

fn _visit_includes<'a>(
    loader: &'a IncludeLoader,
    path: &'_ Path,
    mut dts: Dts<'a>,
    out: &'_ mut Dts<'a>,
) -> Result<(), ParseError> {
    let dir = Some(path.parent().unwrap());
    for include in dts.Include() {
        let ipath = include.IncludePath().span.as_str().trim_matches('"');
        let Some((ipath, src)) = loader.find_utf8(dir, &Path::new(ipath)) else {
            // convert pest_typed::Span to pest::Span
            let s = include.span;
            let span = pest::Span::new(s.get_input(), s.start(), s.end()).unwrap();
            let error = pest::error::Error::<()>::new_from_span(
                pest::error::ErrorVariant::CustomError {
                    message: format!("can't find include {ipath:?}"),
                },
                span,
            );
            // TODO: the formatter for `pest::Error` is nice, but lacks the source path.
            panic!("in {path:?}:\n{error}");
            // TODO: return Err(ParseError(format!("can't find include {ipath:?}")));
        };
        let dts = parse(src);
        _visit_includes(loader, ipath, dts, out)?;
    }
    // TODO: copy headers and memreserves as well.  let parser deal with them.
    // accumulate fields into the output, other than Includes (tuple element 1)
    let it = dts.content.0.matched.content.drain(..);
    out.content.0.matched.content.extend(it);
    let it = dts.content.2.matched.content.drain(..);
    out.content.2.matched.content.extend(it);
    let it = dts.content.3.matched.content.drain(..);
    out.content.3.matched.content.extend(it);
    Ok(())
}

// TODO:  alternatively, we could have a design where we intermix loading the sources, evaluating
// expressions, and tree-merge operations into a single traversal.  will that be too messy?
// seems like it might be worse for error reporting.  i don't want to hear about an invalid
// expression if my includes don't work.
//
// Arguably
//
// / { x = <(0 / 0)>; };
// / { /delete-property/ x; };
//
// should be accepted, although `dtc` rejects this.
