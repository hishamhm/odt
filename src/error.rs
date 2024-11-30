use crate::parse::Rule;
use core::fmt::{Debug, Display, Formatter, Result};
use core::ops::Range;
use pest::error::{Error, ErrorVariant};
use std::path::Path;

/// An error annotated with a span of source code.
pub struct SourceError {
    /// The wrapped error.
    /// TODO:  is this dependency OK?  what about parse_untyped?  move error?
    pub pest_error: Error<Rule>,
    /// The source's address range; effectively, a '&static str which can only be compared by
    /// identity.  This is used to reconstruct the source path during error reporting.
    pub buffer: Range<*const u8>,
}

impl SourceError {
    pub fn new(message: String, span: pest::Span) -> Self {
        let message = ErrorVariant::CustomError { message };
        let buffer = span.get_input().as_bytes().as_ptr_range();
        let pest_error = Error::new_from_span(message, span);
        Self { pest_error, buffer }
    }

    // TODO: better to do this with an enum field.  the formatting looks strange this way.
    pub fn new_unattributed(message: String) -> Self {
        let message = ErrorVariant::CustomError { message };
        let src = "";
        let buffer = src.as_bytes().as_ptr_range();
        let pos = pest::Position::from_start(src);
        let pest_error = Error::new_from_pos(message, pos);
        Self { pest_error, buffer }
    }

    pub fn path(&self) -> Option<&str> {
        self.pest_error.path()
    }

    pub fn with_path(mut self, path: &Path) -> Self {
        self.pest_error = self.pest_error.with_path(&path.to_string_lossy());
        self
    }
}

impl From<Box<Error<Rule>>> for SourceError {
    fn from(pest_error: Box<Error<Rule>>) -> Self {
        let pest_error = *pest_error;
        let src = "";
        let buffer = src.as_bytes().as_ptr_range();
        SourceError { pest_error, buffer }
    }
}

impl Debug for SourceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(&self.pest_error, f)
    }
}

impl Display for SourceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(&self.pest_error, f)
    }
}

impl core::error::Error for SourceError {}
