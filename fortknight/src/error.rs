use std::io::Write;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use crate::data::FileData;
use crate::span::Span;

/// This structure is a target for emitting diagnostics (warnings, errors, etc.). Any component with
/// access to a `DiagnosticSink` can emit a diagnostic without exclusive mutable access.
pub enum DiagnosticSink {
    Terminal(StandardStream),
    Raw(Box<(dyn Write + Send)>),
}

impl DiagnosticSink {
    pub fn from_stderr() -> Self {
        DiagnosticSink::Terminal(StandardStream::stderr(ColorChoice::Always))
    }

    pub fn set_color(&mut self, spec: &mut ColorSpec) -> std::io::Result<()> {
        match *self {
            DiagnosticSink::Terminal(ref mut stream) => stream.set_color(spec),
            _ => Ok(()),
        }
    }

    pub fn reset(&mut self) -> std::io::Result<()> {
        match *self {
            DiagnosticSink::Terminal(ref mut stream) => stream.reset(),
            _ => Ok(()),
        }
    }
}

impl Write for DiagnosticSink {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        match *self {
            DiagnosticSink::Terminal(ref mut t) => t.write(bytes),
            DiagnosticSink::Raw(ref mut t) => t.write(bytes),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match *self {
            DiagnosticSink::Terminal(ref mut t) => t.flush(),
            DiagnosticSink::Raw(ref mut t) => t.flush(),
        }
    }
}

impl DiagnosticSink {
    pub fn emit_error_from_contents(
        &mut self,
        _contents: &str,
        kind: AnalysisErrorKind,
        _span: Span,
        msg: &str,
    ) {
        (move || -> Result<(), Box<dyn std::error::Error>> {
            self.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_intense(true))?;
            write!(self, "error [E{:04}]", kind.code())?;
            self.set_color(
                ColorSpec::new()
                    .set_fg(Some(Color::White))
                    .set_intense(true),
            )?;

            writeln!(self, ": {}", msg)?;

            // TODO: Output the offending text

            self.reset()?;

            Ok(())
        })()
        .unwrap()
    }

    pub fn emit_error(
        &mut self,
        file_data: &FileData,
        kind: AnalysisErrorKind,
        span: Span,
        msg: &str,
    ) {
        self.emit_error_from_contents(
            &file_data.contents[span.file_id.0 as usize],
            kind,
            span,
            msg,
        )
    }
}

#[derive(Debug)]
pub enum AnalysisErrorKind {
    /// Lexer + Parser Errors: E0000-E0999
    Parser(ParserErrorCode),

    /// Semantic Errors: E1000-E1999
    Semantic(SemanticErrorCode),

    /// System Errors: E9000+
    Io(std::io::Error),
}

impl AnalysisErrorKind {
    pub fn code(&self) -> u16 {
        match self {
            AnalysisErrorKind::Parser(err) => 0000 + err.code(),
            AnalysisErrorKind::Semantic(err) => 1000 + err.code(),
            AnalysisErrorKind::Io(_) => 9000,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ParserErrorCode {
    UnrecognizedToken,
    UnterminatedStringLiteral,
    UnterminatedOperator,
    LonelyContinuation,
    InvalidCarriageReturn,
    UnexpectedToken,
    MissingExponent,
    DiscontinuedCharacterContext,
    UnexpectedPostContinuationCharacter,
    UnterminatedCBlockComment,
    ExpectedEOS,
    ExpectedToken,
    ExpectedBinaryConstant,
    ExpectedOctalConstant,
    ExpectedHexConstant,
    TooLongLabel,
    UnlabeledFormatStatement,
    LabeledEmptyStatement,
}

impl ParserErrorCode {
    pub fn code(self) -> u16 {
        use ParserErrorCode::*;

        let code = match self {
            UnrecognizedToken => 0,
            UnterminatedStringLiteral => 1,
            UnterminatedOperator => 2,
            LonelyContinuation => 3,
            InvalidCarriageReturn => 4,
            UnexpectedToken => 5,
            MissingExponent => 6,
            DiscontinuedCharacterContext => 7,
            UnexpectedPostContinuationCharacter => 8,
            UnterminatedCBlockComment => 9,
            ExpectedEOS => 10,
            ExpectedToken => 11,
            ExpectedBinaryConstant => 12,
            ExpectedOctalConstant => 13,
            ExpectedHexConstant => 14,
            TooLongLabel => 15,
            UnlabeledFormatStatement => 16,
            LabeledEmptyStatement => 17,
        };
        debug_assert!(code < 1000);
        code
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SemanticErrorCode {
    ImplicitNoneDuplicateSpec,
}

impl SemanticErrorCode {
    pub fn code(self) -> u16 {
        use SemanticErrorCode::*;

        let code = match self {
            ImplicitNoneDuplicateSpec => 0,
        };
        debug_assert!(code < 1000);
        code
    }
}
