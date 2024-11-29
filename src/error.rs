use core::fmt::{Debug, Display, Formatter, Result};
use core::ops::Range;
use pest::error::{Error, ErrorVariant};

/// An error annotated with a span of source code.
pub struct SourceError {
    /// The wrapped error.
    pub pest_error: Error<()>,
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
