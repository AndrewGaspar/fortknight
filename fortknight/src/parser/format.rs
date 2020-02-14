//! Implements a lexer for format-specifications. Sufficiently different from the standard lexer to
//! merit its own implementation.

use crate::error::ParserErrorCode::UnterminatedStringLiteral;
use crate::{
    error::{
        AnalysisErrorKind, DiagnosticSink,
        ParserErrorCode::{self, UnrecognizedToken},
    },
    index::FileId,
    parser::preprocessor::FortranPreprocessor,
    span::Span,
    string::ContinuationStr,
};
use core::cell::RefCell;

#[cfg(test)]
mod tests;
mod token;

pub use token::{FormatToken, FormatTokenKind};

#[derive(Clone)]
pub struct FormatLexer<'input> {
    chars: FortranPreprocessor<'input>,
}

impl<'input> FormatLexer<'input> {
    pub fn new(
        file_id: FileId,
        text: &'input str,
        diagnostics: &'input RefCell<DiagnosticSink>,
    ) -> Self {
        let mut chars = FortranPreprocessor::new(false, file_id, text, diagnostics);
        chars.insignificant_whitespace(true);
        Self { chars }
    }

    fn emit_error_span(&mut self, err: ParserErrorCode, span: Span, msg: &str) {
        self.chars
            .diagnostics
            .borrow_mut()
            .emit_error_from_contents(&self.chars.text, AnalysisErrorKind::Parser(err), span, msg)
    }

    fn emit_error(&mut self, err: ParserErrorCode, start: u32, end: u32, msg: &str) {
        self.emit_error_span(
            err,
            Span {
                file_id: self.chars.file_id,
                start,
                end,
            },
            msg,
        )
    }

    fn text_span(&self, start: u32, end: u32) -> &str {
        &self.chars.text[start as usize..end as usize]
    }

    fn text_len(&self) -> u32 {
        self.chars.text.len() as u32
    }

    fn token(&self, kind: FormatTokenKind, start: u32, end: u32) -> FormatToken {
        FormatToken {
            kind,
            span: Span {
                file_id: self.chars.file_id,
                start,
                end,
            },
        }
    }

    fn bump(&mut self) -> Option<(u32, char)> {
        self.chars.next().map(|(i, c)| (i, c.to_ascii_uppercase()))
    }

    fn peek_char_next(&self) -> Option<(u32, char)> {
        self.chars
            .clone()
            .next()
            .map(|(i, c)| (i, c.to_ascii_uppercase()))
    }

    fn unrecognized_token(&mut self, idx0: u32) -> FormatToken {
        let idx1 = loop {
            if let Some((idx, c)) = self.peek_char_next() {
                if is_recognized_character(c) {
                    break idx;
                } else {
                    self.bump();
                }
            } else {
                break self.text_len();
            }
        };

        self.emit_error(
            UnrecognizedToken,
            idx0,
            idx1,
            &format!(
                "`{}` is not a valid lexical token in Fortran",
                ContinuationStr::new(&self.text_span(idx0, idx1))
            ),
        );

        self.token(FormatTokenKind::Unknown, idx0, idx1)
    }

    fn string_literal(&mut self, idx0: u32, quote: char) -> FormatToken {
        let mut saw_closing_quote = false;

        // TODO: Add optional, non-standard support for escape sequences. See gfortran
        // -fbackslash option
        // See: https://gcc.gnu.org/onlinedocs/gfortran/Fortran-Dialect-Options.html
        let maybe_idx1: Result<u32, u32> = loop {
            if let Some((idx, c)) = self.peek_char_next() {
                if saw_closing_quote {
                    if c == quote {
                        saw_closing_quote = false;
                        self.bump();
                    } else {
                        break Ok(idx);
                    }
                } else {
                    if c == quote {
                        saw_closing_quote = true;
                        self.bump();
                    } else if c == '\r' || c == '\n' {
                        break Err(idx);
                    } else {
                        self.bump();
                    }
                }
            } else {
                break Err(self.text_len());
            }
        };

        let idx1 = match maybe_idx1 {
            Ok(idx1) => idx1,
            Err(idx1) => {
                self.emit_error(
                    UnterminatedStringLiteral,
                    idx0,
                    idx1,
                    "Missing closing quote for string literal",
                );
                return self.token(FormatTokenKind::Unknown, idx0, idx1);
            }
        };

        self.token(FormatTokenKind::CharLiteralConstant, idx0, idx1)
    }

    fn digit_string(&mut self, idx0: u32) -> FormatToken {
        let mut last_idx = idx0;
        while let Some((idx, c)) = self.peek_char_next() {
            if is_digit(c) {
                self.bump();
                last_idx = idx;
            } else {
                break;
            }
        }

        self.token(FormatTokenKind::DigitString, idx0, last_idx + 1)
    }

    fn internal_next(&mut self) -> Option<FormatToken> {
        Some(match self.bump()? {
            (idx0, c) if c == '\'' || c == '"' => self.string_literal(idx0, c),
            (idx0, c) if is_digit(c) => self.digit_string(idx0),
            (idx0, '+') => self.token(FormatTokenKind::Plus, idx0, idx0 + 1),
            (idx0, '-') => self.token(FormatTokenKind::Minus, idx0, idx0 + 1),
            (idx0, '/') => self.token(FormatTokenKind::Slash, idx0, idx0 + 1),
            (idx0, '*') => self.token(FormatTokenKind::Asterisk, idx0, idx0 + 1),
            (idx0, ':') => self.token(FormatTokenKind::Colon, idx0, idx0 + 1),
            (idx0, '.') => self.token(FormatTokenKind::Dot, idx0, idx0 + 1),
            (idx0, ',') => self.token(FormatTokenKind::Comma, idx0, idx0 + 1),
            (idx0, '(') => self.token(FormatTokenKind::LeftParen, idx0, idx0 + 1),
            (idx0, ')') => self.token(FormatTokenKind::RightParen, idx0, idx0 + 1),
            (idx0, 'A') => self.token(FormatTokenKind::A, idx0, idx0 + 1),
            (idx0, 'B') => match self.peek_char_next() {
                Some((_, 'N')) => {
                    self.bump();
                    self.token(FormatTokenKind::BN, idx0, idx0 + 2)
                }
                Some((_, 'Z')) => {
                    self.bump();
                    self.token(FormatTokenKind::BZ, idx0, idx0 + 2)
                }
                _ => self.token(FormatTokenKind::B, idx0, idx0 + 1),
            },
            (idx0, 'D') => match self.peek_char_next() {
                Some((_, 'C')) => {
                    self.bump();
                    self.token(FormatTokenKind::DC, idx0, idx0 + 2)
                }
                Some((_, 'P')) => {
                    self.bump();
                    self.token(FormatTokenKind::DP, idx0, idx0 + 2)
                }
                Some((_, 'T')) => {
                    self.bump();
                    self.token(FormatTokenKind::DT, idx0, idx0 + 2)
                }
                _ => self.token(FormatTokenKind::D, idx0, idx0 + 1),
            },
            (idx0, 'E') => match self.peek_char_next() {
                Some((_, 'N')) => {
                    self.bump();
                    self.token(FormatTokenKind::EN, idx0, idx0 + 2)
                }
                Some((_, 'S')) => {
                    self.bump();
                    self.token(FormatTokenKind::ES, idx0, idx0 + 2)
                }
                Some((_, 'X')) => {
                    self.bump();
                    self.token(FormatTokenKind::EX, idx0, idx0 + 2)
                }
                _ => self.token(FormatTokenKind::E, idx0, idx0 + 1),
            },
            (idx0, 'F') => self.token(FormatTokenKind::F, idx0, idx0 + 1),
            (idx0, 'G') => self.token(FormatTokenKind::G, idx0, idx0 + 1),
            (idx0, 'I') => self.token(FormatTokenKind::I, idx0, idx0 + 1),
            (idx0, 'L') => self.token(FormatTokenKind::L, idx0, idx0 + 1),
            (idx0, 'O') => self.token(FormatTokenKind::O, idx0, idx0 + 1),
            (idx0, 'P') => self.token(FormatTokenKind::P, idx0, idx0 + 1),
            (idx0, 'R') => match self.peek_char_next() {
                Some((_, 'C')) => {
                    self.bump();
                    self.token(FormatTokenKind::RC, idx0, idx0 + 2)
                }
                Some((_, 'D')) => {
                    self.bump();
                    self.token(FormatTokenKind::RD, idx0, idx0 + 2)
                }
                Some((_, 'N')) => {
                    self.bump();
                    self.token(FormatTokenKind::RN, idx0, idx0 + 2)
                }
                Some((_, 'P')) => {
                    self.bump();
                    self.token(FormatTokenKind::RP, idx0, idx0 + 2)
                }
                Some((_, 'U')) => {
                    self.bump();
                    self.token(FormatTokenKind::RU, idx0, idx0 + 2)
                }
                Some((_, 'Z')) => {
                    self.bump();
                    self.token(FormatTokenKind::RZ, idx0, idx0 + 2)
                }
                _ => self.unrecognized_token(idx0),
            },
            (idx0, 'S') => match self.peek_char_next() {
                Some((_, 'P')) => {
                    self.bump();
                    self.token(FormatTokenKind::SP, idx0, idx0 + 2)
                }
                Some((_, 'S')) => {
                    self.bump();
                    self.token(FormatTokenKind::SS, idx0, idx0 + 2)
                }
                _ => self.token(FormatTokenKind::S, idx0, idx0 + 1),
            },
            (idx0, 'T') => match self.peek_char_next() {
                Some((_, 'L')) => {
                    self.bump();
                    self.token(FormatTokenKind::TL, idx0, idx0 + 2)
                }
                Some((_, 'R')) => {
                    self.bump();
                    self.token(FormatTokenKind::TR, idx0, idx0 + 2)
                }
                _ => self.token(FormatTokenKind::T, idx0, idx0 + 1),
            },
            (idx0, 'X') => self.token(FormatTokenKind::X, idx0, idx0 + 1),
            (idx0, 'Z') => self.token(FormatTokenKind::Z, idx0, idx0 + 1),
            (idx0, _) => self.unrecognized_token(idx0),
        })
    }
}

impl<'input> Iterator for FormatLexer<'input> {
    type Item = FormatToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.internal_next()
    }
}

fn is_letter(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_recognized_character(c: char) -> bool {
    is_letter(c)
        || is_digit(c)
        || match c {
            '+' | '-' | '*' | '/' | '(' | ')' | ',' | '.' | ':' | '\'' | '"' => true,
            _ => false,
        }
}
