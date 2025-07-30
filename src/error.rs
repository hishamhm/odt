use crate::parse::Rule;
use core::fmt::{Debug, Display, Formatter};
use core::ops::Range;
use pest::error::{Error, ErrorVariant};
use std::path::Path;

/// An error annotated with a span of source code.
pub struct SourceError {
    /// The wrapped error.
    pub pest_error: Box<Error<Rule>>,
    /// The source's address range; effectively, a &'static str which can only be compared by
    /// identity.  This is used to reconstruct the source path during error reporting.
    pub buffer: Range<usize>,
}

impl SourceError {
    pub fn new(message: String, span: pest::Span) -> Self {
        let message = ErrorVariant::CustomError { message };
        let buffer = span.get_input().as_bytes().as_ptr_range();
        let buffer = buffer.start as usize..buffer.end as usize;
        let pest_error = Box::new(Error::new_from_span(message, span));
        Self { pest_error, buffer }
    }

    // TODO: better to do this with an enum field.  the formatting looks strange this way.
    pub fn new_unattributed(message: String) -> Self {
        let message = ErrorVariant::CustomError { message };
        let src = "";
        let buffer = src.as_bytes().as_ptr_range();
        let buffer = buffer.start as usize..buffer.end as usize;
        let pos = pest::Position::from_start(src);
        let pest_error = Box::new(Error::new_from_pos(message, pos));
        Self { pest_error, buffer }
    }

    pub fn path(&self) -> Option<&str> {
        self.pest_error.path()
    }

    pub fn with_path(mut self, path: &Path) -> Self {
        *self.pest_error = self.pest_error.with_path(&path.to_string_lossy());
        self
    }

    pub fn buffer(&self) -> Range<*const u8> {
        self.buffer.start as *const u8..self.buffer.end as *const u8
    }
}

impl From<Error<Rule>> for SourceError {
    fn from(pest_error: Error<Rule>) -> Self {
        let pest_error = Box::new(pest_error);
        let buffer = "".as_bytes().as_ptr_range();
        let buffer = buffer.start as usize..buffer.end as usize;
        SourceError { pest_error, buffer }
    }
}

impl Debug for SourceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        Display::fmt(&self.pest_error, f)
    }
}

impl Display for SourceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        Display::fmt(&self.pest_error, f)
    }
}

impl core::error::Error for SourceError {}
