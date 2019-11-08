//! Implements a "low-level" lexer which merely resolves continuations

use std::cell::RefCell;
use std::iter::Iterator;
use std::str::CharIndices;

use crate::error::{AnalysisErrorKind, DiagnosticSink, ParserErrorCode};
use crate::index::FileId;
use crate::span::Span;

#[cfg(test)]
mod tests;
#[derive(Copy, Clone, PartialEq, Eq)]
enum InCComment {
    No,
    InOpening,
    Yes,
    InClosing,
}

/// Merely massaging the underlying character stream to make it easier for the higher level lexer to
/// to lex tokens. It does this by:
/// - resolving continuations
/// - mapping down newlines to LF
#[derive(Clone)]
pub struct FortranPreprocessor<'input> {
    pub(crate) file_id: FileId,
    pub(crate) text: &'input str,
    pub(crate) diagnostics: &'input RefCell<DiagnosticSink>,
    chars: CharIndices<'input>,
    tokenize_preprocessor: bool,
    fresh_new_line: bool,
    opening_quote: Option<char>,
    in_c_comment: InCComment,
    insignificant_whitespace: bool,
}

impl<'input> FortranPreprocessor<'input> {
    pub fn new(
        tokenize_preprocessor: bool,
        file_id: FileId,
        text: &'input str,
        diagnostics: &'input RefCell<DiagnosticSink>,
    ) -> Self {
        assert!(
            text.len() <= u32::max_value() as usize,
            "Fortknight only supports a maximum of 4GB files."
        );

        FortranPreprocessor {
            file_id,
            text,
            diagnostics,
            chars: text.char_indices(),
            tokenize_preprocessor,
            fresh_new_line: true,
            opening_quote: None,
            in_c_comment: InCComment::No,
            insignificant_whitespace: false,
        }
    }

    pub fn insignificant_whitespace(&mut self, b: bool) {
        self.insignificant_whitespace = b;
    }

    fn emit_error_span(&mut self, err: ParserErrorCode, span: Span, msg: &str) {
        self.diagnostics.borrow_mut().emit_error_from_contents(
            &self.text,
            AnalysisErrorKind::Parser(err),
            span,
            msg,
        )
    }

    fn emit_error(&mut self, err: ParserErrorCode, start: u32, end: u32, msg: &str) {
        self.emit_error_span(
            err,
            Span {
                file_id: self.file_id,
                start,
                end,
            },
            msg,
        )
    }

    fn text_len(&self) -> u32 {
        self.text.len() as u32
    }

    fn bump(&mut self) -> Option<(u32, char)> {
        self.chars.next().map(|(i, c)| (i as u32, c))
    }

    fn bump_nth(&mut self, n: u32) -> Option<(u32, char)> {
        self.chars.nth(n as usize).map(|(i, c)| (i as u32, c))
    }

    fn peek_char(&mut self) -> Option<(u32, char)> {
        self.peek_nth_char(0)
    }

    fn peek_nth_char(&mut self, n: u32) -> Option<(u32, char)> {
        self.chars
            .clone()
            .nth(n as usize)
            .map(|(i, c)| (i as u32, c))
    }

    // call when you want to look past a new-line token.
    fn peek_past_new_line(&mut self, chars: &mut CharIndices<'input>) -> u32 {
        match chars.next().map(|(i, c)| (i as u32, c)) {
            Some((_, '\n')) => 1,
            Some((idx0, '\r')) => {
                match chars.next() {
                    Some((_, '\n')) => 2,
                    // CR is not a supported line ending
                    _ => {
                        self.emit_error(
                            ParserErrorCode::InvalidCarriageReturn,
                            idx0,
                            idx0 + 1,
                            "CR is not a valid line ending. Only CRLF and LF are supported.",
                        );
                        // But still act like it is so we can at least progress.
                        1
                    }
                }
            }
            _ => panic!("Internal compiler error: self.peek_char() must match a new line start"),
        }
    }

    // call when you want to consume a new-line token.
    fn consume_new_line(&mut self) -> Option<(u32, char)> {
        match self.bump() {
            x @ Some((_, '\n')) => x,
            Some((idx0, '\r')) => {
                match self.peek_char() {
                    Some((_, '\n')) => {
                        self.bump();
                        Some((idx0, '\n'))
                    }
                    // CR is not a supported line ending
                    _ => {
                        self.emit_error(
                            ParserErrorCode::InvalidCarriageReturn,
                            idx0,
                            idx0 + 1,
                            "CR is not a valid line ending. Only CRLF and LF are supported.",
                        );
                        // But still act like it is so we can at least progress.
                        Some((idx0, '\n'))
                    }
                }
            }
            _ => panic!("Internal compiler error: self.peek_char() must match a new line start"),
        }
    }

    /// Called upon seeing a '!' but not wanting to consume it yet. Returns the next position after
    /// the newline closing the commentary
    fn peek_past_commentary(&mut self, chars: &mut CharIndices<'input>) -> u32 {
        loop {
            match chars.clone().next() {
                Some((_, '\r')) | Some((_, '\n')) | None => break self.peek_past_new_line(chars),
                _ => {
                    chars.next();
                    continue;
                }
            }
        }
    }

    /// Called upon seeing a '!'
    fn skip_commentary(&mut self) {
        loop {
            match self.peek_char() {
                Some((_, '\r')) | Some((_, '\n')) | None => break,
                _ => {
                    self.bump();
                    continue;
                }
            }
        }
    }

    /// Called after consuming the starting '&'
    fn resolve_continuation(&mut self, idx0: u32) -> Option<(u32, char)> {
        // Resolve first line
        loop {
            match self.peek_char() {
                // Commentary is ignored inside a string literal - exclamation points are
                // significant
                Some((_, '!')) if self.opening_quote.is_none() => {
                    self.skip_commentary();
                    self.consume_new_line();
                    break;
                }
                Some((_, '\r')) | Some((_, '\n')) => {
                    self.consume_new_line();
                    break;
                }
                Some((_, c)) if c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Some(_) => {
                    let idx1 = loop {
                        match self.bump() {
                            Some((idx1, '!')) => break idx1,
                            Some((idx1, '\r')) | Some((idx1, '\n')) => break idx1,
                            None => break self.text_len(),
                            _ => continue,
                        }
                    };

                    self.emit_error(
                        ParserErrorCode::UnexpectedPostContinuationCharacter, idx0, idx1,
                        "Nothing may follow a line continuation besides commentary, whitespace, or \
                        a newline");
                    continue;
                }
                None => {
                    self.bump();
                    return None;
                }
            };
        }

        // We're at the next line in the continuation

        // We need to lookahead to see what we're dealing with before consuming any of the
        // characters
        let mut chars = self.chars.clone();
        let mut first_seen_whitepace = None;
        loop {
            match chars.clone().next().map(|(i, c)| (i as u32, c)) {
                Some((_, '!')) if self.opening_quote.is_none() => {
                    self.peek_past_commentary(&mut chars);
                    // this was a commentary line, so forget the whitespace we saw
                    first_seen_whitepace = None;
                }
                Some((_, '\r')) | Some((_, '\n')) | None => {
                    // skip all the characters we've advanced past and go back to letting the main
                    // loop steer.

                    // We don't want to consume the newline, so we merely consume up to the
                    // previous character
                    self.chars = chars;

                    return None;
                }
                Some((_, c)) if c.is_whitespace() => {
                    // Whitespace only later becomes significant if it turns out we're not in a
                    // token continuation
                    if first_seen_whitepace.is_none() {
                        first_seen_whitepace = Some(chars.clone());
                    }
                    chars.next();
                }
                Some((idx, '&')) => {
                    // We're in a token continuation - check to see if the continuation is lonely,
                    // then advance through the continuation end

                    // consume through the ampersand
                    chars.next().unwrap();

                    {
                        let mut chars = chars.clone();
                        loop {
                            match chars.next() {
                                Some((_, '\r')) | Some((_, '\n')) | None => {
                                    self.emit_error(
                                    ParserErrorCode::LonelyContinuation,
                                    idx,
                                    idx + 1,
                                    "A line continuation may not appear as the only token on a line",
                                );
                                    break;
                                }
                                Some((_, c)) if c.is_whitespace() => {}
                                Some(_) => {
                                    break;
                                }
                            };
                        }
                    }

                    self.chars = chars;

                    return None;
                }
                Some((idx, _)) => {
                    // When in a string literal, even though it's erroneous to not have a
                    // continuation, just continue from the first column, even if it's not
                    // whitespace
                    if self.opening_quote.is_some() {
                        self.emit_error(
                            ParserErrorCode::DiscontinuedCharacterContext,
                            idx0,
                            idx,
                            "Missing continuation ('&') of string literal",
                        );

                        return match first_seen_whitepace {
                            Some(chars) => {
                                self.chars = chars;
                                self.bump()
                            }
                            None => {
                                self.chars = chars;
                                self.bump()
                            }
                        };
                    }

                    // If we encounter a significant character, then we know we're merely continuing
                    // the statement, and not continuing the lexical token. Therefore we must emit
                    // at least one whitespace, even if none appear on this line. We emit the first
                    // seen whitespace on the line if there is one, otherwise we emit a space as if
                    // it was in the current column.
                    return match first_seen_whitepace {
                        Some(chars) => {
                            self.chars = chars;
                            self.bump()
                        }
                        None => Some((idx, ' ')),
                    };
                }
            };
        }
    }

    /// Only called when we're tokenizing the preprocessor, and we've detected that we're in a c
    /// block comment. Consider this a separate "mode".
    fn in_c_comment_next(&mut self) -> Option<(u32, char)> {
        let mut chars = self.chars.clone();

        match chars.next() {
            Some((_, '*')) if self.in_c_comment == InCComment::InOpening => {
                self.in_c_comment = InCComment::Yes;
                self.bump()
            }
            Some((_, '*')) if self.in_c_comment == InCComment::Yes => {
                if let Some((_, '/')) = chars.next() {
                    self.in_c_comment = InCComment::InClosing;
                }
                self.bump()
            }
            Some((_, '/')) if self.in_c_comment == InCComment::InClosing => {
                self.in_c_comment = InCComment::No;
                self.bump()
            }
            _ => self.bump(),
        }
    }

    fn char_next(&mut self) -> Option<(u32, char)> {
        if self.in_c_comment != InCComment::No {
            return self.in_c_comment_next();
        }

        loop {
            let mut chars = self.chars.clone();
            return match chars.next().map(|(i, c)| (i as u32, c)) {
                // '&' is significant in a C Block Comment
                Some((idx0, '&')) => {
                    if self.fresh_new_line {
                        self.emit_error(
                            ParserErrorCode::LonelyContinuation,
                            idx0,
                            idx0 + 1,
                            "A line continuation may not appear as the only token on a line",
                        );
                    }

                    self.bump();
                    match self.resolve_continuation(idx0) {
                        Some(x) => Some(x),
                        None => continue,
                    }
                }
                Some((_, '\r')) | Some((_, '\n')) => {
                    self.fresh_new_line = true;
                    self.consume_new_line()
                }
                Some((_, c)) if self.opening_quote.is_none() && (c == '\'' || c == '"') => {
                    self.opening_quote = Some(c);
                    self.bump()
                }
                Some((_, c)) if Some(c) == self.opening_quote => {
                    self.opening_quote = None;
                    self.bump()
                }
                Some((_, '/'))
                    if self.tokenize_preprocessor && self.in_c_comment == InCComment::No =>
                {
                    if let Some((_, '*')) = chars.next() {
                        self.in_c_comment = InCComment::InOpening;
                    };
                    self.bump()
                }
                Some((_, c)) if self.insignificant_whitespace && c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                _ => {
                    self.fresh_new_line = false;
                    self.bump()
                }
            };
        }
    }
}

impl<'input> Iterator for FortranPreprocessor<'input> {
    type Item = (u32, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.char_next()
    }
}
