//! Implements a "low-level" lexer which merely resolves continuations

use std::cell::RefCell;
use std::iter::Iterator;
use std::str::CharIndices;

use peek_nth::{IteratorExt, PeekableNth};

use crate::error::{AnalysisErrorKind, DiagnosticSink, ParserErrorCode};
use crate::index::FileId;
use crate::span::Span;

use super::TokenizerOptions;

/// Merely massaging the underlying character stream to make it easier for the higher level lexer to
/// to lex tokens. It does this by:
/// - resolving continuations
/// - mapping down newlines to LF
pub struct LowLevelLexer<'input> {
    file_id: FileId,
    text: &'input str,
    diagnostics: &'input RefCell<DiagnosticSink>,
    chars: PeekableNth<CharIndices<'input>>,
    tokenize_preprocessor: bool,
    fresh_new_line: bool,
    opening_quote: Option<char>,
}

impl<'input> LowLevelLexer<'input> {
    pub fn new(
        options: &TokenizerOptions,
        file_id: FileId,
        text: &'input str,
        diagnostics: &'input RefCell<DiagnosticSink>,
    ) -> Self {
        assert!(
            text.len() <= u32::max_value() as usize,
            "Fortknight only supports a maximum of 4GB files."
        );

        LowLevelLexer {
            file_id,
            text,
            diagnostics,
            chars: text.char_indices().peekable_nth(),
            tokenize_preprocessor: options.tokenize_preprocessor,
            fresh_new_line: true,
            opening_quote: None,
        }
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

    fn peek_nth(&mut self, n: usize) -> Option<(u32, char)> {
        self.chars.peek_nth(n).map(|(i, c)| (*i as u32, *c))
    }

    fn peek(&mut self) -> Option<(u32, char)> {
        self.peek_nth(0)
    }

    fn bump(&mut self) -> Option<(u32, char)> {
        self.chars.next().map(|(i, c)| (i as u32, c))
    }

    fn bump_nth(&mut self, n: usize) -> Option<(u32, char)> {
        self.chars.nth(n).map(|(i, c)| (i as u32, c))
    }

    // call when you want to look past a new-line token.
    fn peek_past_new_line(&mut self, start: usize) -> usize {
        match self.peek_nth(start) {
            Some((_, '\n')) => start + 1,
            Some((idx0, '\r')) => {
                match self.peek_nth(start + 1) {
                    Some((_, '\n')) => start + 2,
                    // CR is not a supported line ending
                    _ => {
                        self.emit_error(
                            ParserErrorCode::InvalidCarriageReturn,
                            idx0,
                            idx0 + 1,
                            "CR is not a valid line ending. Only CRLF and LF are supported.",
                        );
                        // But still act like it is so we can at least progress.
                        start + 1
                    }
                }
            }
            _ => panic!("Internal compiler error: self.peek() must match a new line start"),
        }
    }

    // call when you want to consume a new-line token.
    fn consume_new_line(&mut self) -> Option<(u32, char)> {
        match self.bump() {
            x @ Some((_, '\n')) => x,
            Some((idx0, '\r')) => {
                match self.peek() {
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
            _ => panic!("Internal compiler error: self.peek() must match a new line start"),
        }
    }

    /// Called upon seeing a '!' but not wanting to consume it yet. Returns the next position after
    /// the newline closing the commentary
    fn peek_past_commentary(&mut self, start_at: usize) -> usize {
        let mut advance = start_at;
        loop {
            match self.peek_nth(advance) {
                Some((_, '\r')) | Some((_, '\n')) | None => break self.peek_past_new_line(advance),
                _ => {
                    advance += 1;
                    continue;
                }
            }
        }
    }

    /// Called upon seeing a '!'
    fn skip_commentary(&mut self) {
        loop {
            match self.peek() {
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
            match self.peek() {
                // Commentary is ignored inside a string literal
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
        let mut advance_by = 0;
        let mut first_seen_whitepace = None;
        loop {
            match self.peek_nth(advance_by) {
                Some((_, '!')) if self.opening_quote.is_none() => {
                    advance_by = self.peek_past_commentary(advance_by);
                    // this was a commentary line, so forget the whitespace we saw
                    first_seen_whitepace = None;
                }
                // Whitespace, or lack of whitespace, is always significant in a string-literal
                Some((_, c)) if self.opening_quote.is_none() && c.is_whitespace() => {
                    // Whitespace only later becomes significant if it turns out we're not in a
                    // token continuation
                    if first_seen_whitepace.is_none() {
                        first_seen_whitepace = Some(advance_by);
                    }
                    advance_by += 1;
                }
                Some((_, '\r')) | Some((_, '\n')) | None => {
                    // skip all the characters we've advanced past and go back to letting the main
                    // loop steer.

                    // We don't want to consume the newline, so we merely consume up to the
                    // previous character
                    if advance_by > 0 {
                        self.bump_nth(advance_by - 1);
                    }

                    return None;
                }
                Some((idx, '&')) => {
                    // We're in a token continuation - check to see if the continuation is lonely,
                    // then advance through the continuation end

                    let mut advance_past = advance_by + 1;

                    loop {
                        match self.peek_nth(advance_past) {
                            Some((_, c)) if c.is_whitespace() => {}
                            Some((_, '\r')) | Some((_, '\n')) | None => {
                                self.emit_error(
                                    ParserErrorCode::LonelyContinuation,
                                    idx,
                                    idx + 1,
                                    "A line continuation may not appear as the only token on a line",
                                );
                                break;
                            }
                            Some(_) => {
                                break;
                            }
                        };

                        advance_past += 1;
                    }

                    // consume through the ampersand
                    self.bump_nth(advance_by);

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
                        return self.bump_nth(advance_by);
                    }

                    // If we encounter a significant character, then we know we're merely continuing
                    // the statement, and not continuing the lexical token. Therefore we must emit
                    // at least one whitespace, even if none appear on this line. We emit the first
                    // seen whitespace on the line if there is one, otherwise we emit a space as if
                    // it was in the current column.
                    return match first_seen_whitepace {
                        Some(x) => self.bump_nth(x),
                        None => Some((idx, ' ')),
                    };
                }
            };
        }
    }
}

impl<'input> Iterator for LowLevelLexer<'input> {
    type Item = (u32, char);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            return match self.peek() {
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
                _ => {
                    self.fresh_new_line = false;
                    self.bump()
                }
            };
        }
    }
}
