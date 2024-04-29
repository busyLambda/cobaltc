use std::ops::Range;

#[derive(Debug)]
pub struct PropagatorError {
    message: String,
    location: Location,
}

impl PropagatorError {
    pub fn new(message: &str, location: Location) -> Self {
        Self {
            message: message.to_string(),
            location,
        }
    }
}

#[derive(Debug)]
pub struct StructureError {
    message: String,
    propagator_location: Location,
    location: Location,
}

impl StructureError {
    pub fn from_propagator(value: PropagatorError, location: Location) -> Self {
        Self {
            message: value.message,
            propagator_location: value.location,
            location,
        }
    }

    pub fn new(message: &str, propagator_location: Location, location: Location) -> Self {
        Self {
            message: message.to_string(),
            propagator_location,
            location,
        }
    }
}

pub type PropagatorResult<T> = Result<T, (T, PropagatorError)>;
#[derive(Debug)]
pub struct ParseError {
    message: String,
    location: ErrorLocation,
}

impl ParseError {
    pub fn new(message: String, location: ErrorLocation) -> ParseError {
        ParseError { message, location }
    }
}

#[derive(Debug)]
pub struct ErrorLocation {
    structure: Location,
    // Structures don't return errors while propagator parsers do,
    // These errors travel up the chain until the structure reports it.
    propagator: Location,
}

impl ErrorLocation {
    pub fn new() -> ErrorLocation {
        ErrorLocation {
            structure: Location { span: 0..0 },
            propagator: Location { span: 0..0 },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Location {
    span: Range<usize>,
}

impl Location {
    pub fn new(span: Range<usize>) -> Self {
        Self { span }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct SyncEOF;