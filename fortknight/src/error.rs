use crate::span::Span;
use arrayvec::ArrayVec;

#[derive(Debug)]
pub enum AnalysisErrorKind {
    // Lexer + Parser Errors: E0000-E0999
    Parser(ParserErrorCode),

    // System Errors: E9000+
    Io(std::io::Error),
}

impl AnalysisErrorKind {
    pub fn code(&self) -> u16 {
        match self {
            AnalysisErrorKind::Parser(err) => 9000 + err.code(),
            AnalysisErrorKind::Io(_) => 9000,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ParserErrorCode {
    UnrecognizedToken,
    UnterminatedStringLiteral,
    UnterminatedOperator,
    UnterminatedContinuationLine,
    InvalidCarriageReturn,
    UnexpectedToken,
    MissingExponent,
    DiscontinuedCharacterContext,
}

impl ParserErrorCode {
    pub fn code(self) -> u16 {
        use ParserErrorCode::*;

        let code = match self {
            UnrecognizedToken => 0,
            UnterminatedStringLiteral => 1,
            UnterminatedOperator => 2,
            UnterminatedContinuationLine => 3,
            InvalidCarriageReturn => 4,
            UnexpectedToken => 5,
            MissingExponent => 6,
            DiscontinuedCharacterContext => 7,
        };
        assert!(code < 1000);
        code
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OneError {
    pub span: Span,
    pub code: ParserErrorCode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error(pub ArrayVec<[OneError; 2]>);

impl From<OneError> for Error {
    fn from(err: OneError) -> Self {
        use std::iter::FromIterator;

        Error(ArrayVec::from_iter(std::iter::once(err)))
    }
}
