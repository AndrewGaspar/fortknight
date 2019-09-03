use std::cell::RefCell;
use std::iter::Iterator;
use std::str::CharIndices;

use num_traits::FromPrimitive;
use peek_nth::{IteratorExt, PeekableNth};

use self::TakeUntil::*;

use crate::error::{
    AnalysisErrorKind, DiagnosticSink,
    ParserErrorCode::{self, *},
};
use crate::index::FileId;
use crate::span::Span;
use crate::string::{CaseInsensitiveContinuationStr, ContinuationStr};

#[cfg(test)]
mod tests;

mod low_level;
mod token;

use token::*;
pub use token::{KeywordTokenKind, Letter, Token, TokenKind};

#[derive(Clone, Copy, Default)]
pub struct TokenizerOptions {
    pub tokenize_preprocessor: bool,
}

pub struct Tokenizer<'input> {
    file_id: FileId,
    text: &'input str,
    diagnostics: &'input RefCell<DiagnosticSink>,
    chars: PeekableNth<CharIndices<'input>>,
    tokenize_preprocessor: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TakeUntil {
    Continue,
    Stop,
    Abort,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Lookahead {
    Character(u32, char),
    EOF,
}

impl<'input> Tokenizer<'input> {
    pub fn new(
        options: &TokenizerOptions,
        file_id: FileId,
        text: &'input str,
        diagnostics: &'input RefCell<DiagnosticSink>,
    ) -> Tokenizer<'input> {
        assert!(
            text.len() <= u32::max_value() as usize,
            "Fortknight only supports a maximum of 4GB files."
        );

        Tokenizer {
            file_id,
            text,
            diagnostics,
            chars: text.char_indices().peekable_nth(),
            tokenize_preprocessor: options.tokenize_preprocessor,
        }
    }

    fn lookahead_idx(&self, lookahead: Lookahead) -> u32 {
        match lookahead {
            Lookahead::Character(idx, _) => idx,
            Lookahead::EOF => self.text_len(),
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

    fn text_span(&self, start: u32, end: u32) -> &str {
        &self.text[start as usize..end as usize]
    }

    fn text_len(&self) -> u32 {
        self.text.len() as u32
    }

    fn token(&self, kind: TokenKind, start: u32, end: u32) -> Token {
        Token {
            kind,
            span: Span {
                file_id: self.file_id,
                start,
                end,
            },
        }
    }

    fn operator(&mut self, idx0: u32) -> Token {
        let maybe_idx1 = self.take_until(idx0, |lookahead: Lookahead| match lookahead {
            Lookahead::Character(_, c) if is_operator_continue(c) => Continue,
            Lookahead::Character(_, '.') => Stop,
            _ => Abort,
        });

        let idx1 = match maybe_idx1 {
            Ok(idx1) => idx1,
            Err(idx1) => {
                self.emit_error(
                    UnterminatedOperator,
                    idx0,
                    idx1,
                    &format!(
                        "Expected `{}` to be an operator.",
                        ContinuationStr::new(self.text_span(idx0, idx1))
                    ),
                );
                return self.token(TokenKind::Unknown, idx0, idx1);
            }
        };

        // consume .
        self.bump();

        // don't include . in operator name
        let operator = CaseInsensitiveContinuationStr::new(&self.text_span(idx0 + 1, idx1));

        let kind = INTRINSIC_OPERATORS
            .iter()
            .filter(|&&(w, _)| CaseInsensitiveContinuationStr::new(w) == operator)
            .map(|&(_, ref t)| t.clone())
            .next()
            .unwrap_or(TokenKind::DefinedOperator);

        self.token(kind, idx0, idx1 + 1)
    }

    fn identifierish(&mut self, idx0: u32) -> Token {
        let idx1 = self
            .take_until(idx0, |lookahead: Lookahead| match lookahead {
                Lookahead::Character(_, c) if is_identifier_continue(c) => Continue,
                _ => Stop,
            })
            .expect("Internal error: identifier tokens always terminate");

        let word = CaseInsensitiveContinuationStr::new(&self.text_span(idx0, idx1)).to_string();

        let kind = if word.len() == 1 {
            TokenKind::Letter(Letter::from_u8(word.as_bytes()[0] - b'a').unwrap())
        } else if let Some(kind) = KEYWORDS_TRIE.get(&word) {
            TokenKind::Keyword(*kind)
        } else {
            TokenKind::Name
        };

        self.token(kind, idx0, idx1)
    }

    fn unrecognized_token(&mut self, idx0: u32) -> Token {
        let tokenize_preprocessor = self.tokenize_preprocessor;

        let idx1 = self
            .take_until(idx0, |lookahead: Lookahead| match lookahead {
                Lookahead::Character(_, c) if is_recognized_character(c, tokenize_preprocessor) => {
                    Stop
                }
                Lookahead::EOF => Stop,
                _ => Continue,
            })
            .unwrap_or(self.text_len());

        self.emit_error(
            UnrecognizedToken,
            idx0,
            idx1,
            &format!(
                "`{}` is not a valid lexical token in Fortran",
                ContinuationStr::new(&self.text_span(idx0, idx1))
            ),
        );

        self.token(TokenKind::Unknown, idx0, idx1)
    }

    fn string_literal(&mut self, idx0: u32, quote: char) -> Token {
        let mut saw_closing_quote = false;

        // TODO: Add optional, non-standard support for escape sequences. See gfortran
        // -fbackslash option
        // See: https://gcc.gnu.org/onlinedocs/gfortran/Fortran-Dialect-Options.html
        let maybe_idx1 = self.take_until_terminate(idx0, true, |lookahead: Lookahead| {
            if saw_closing_quote {
                match lookahead {
                    Lookahead::Character(_, c) if c == quote => {
                        saw_closing_quote = false;
                        Continue
                    }
                    Lookahead::Character(_, _) | Lookahead::EOF => Stop,
                }
            } else {
                match lookahead {
                    Lookahead::Character(_, c) if c == quote => {
                        saw_closing_quote = true;
                        Continue
                    }
                    Lookahead::Character(_, c) if is_new_line_start(c) => Abort,
                    Lookahead::Character(_, _) => Continue,
                    Lookahead::EOF => Abort,
                }
            }
        });

        let idx1 = match maybe_idx1 {
            Ok(idx1) => idx1,
            Err(idx1) => {
                self.emit_error(
                    UnterminatedStringLiteral,
                    idx0,
                    idx1,
                    "Missing closing quote for string literal",
                );
                return self.token(TokenKind::Unknown, idx0, idx1);
            }
        };

        self.token(TokenKind::CharLiteralConstant, idx0, idx1)
    }

    fn take_digit_string(&mut self, idx0: u32) -> u32 {
        self.take_until(idx0, |lookahead: Lookahead| match lookahead {
            Lookahead::Character(_, c) if is_digit(c) => Continue,
            _ => Stop,
        })
        .expect("Internal compiler error: Did not expect abort when taking a digit string")
    }

    fn finish_exponent(&mut self, idx0: u32) -> Token {
        if let Lookahead::Character(idx1, '+') | Lookahead::Character(idx1, '-') = self.peek() {
            self.bump();
            if !self.skip_continuation() {
                self.emit_error(
                    MissingExponent,
                    idx0,
                    idx1,
                    &format!("Real literal constant has incomplete exponent part"),
                );
                return self.token(TokenKind::Unknown, idx0, idx1);
            }
        }

        match self.peek() {
            Lookahead::Character(_, c) if is_digit(c) => {
                let idx1 = self.take_digit_string(idx0);
                self.token(TokenKind::RealLiteralConstant, idx0, idx1)
            }
            lookahead => {
                self.emit_error(
                    MissingExponent,
                    idx0,
                    self.lookahead_idx(lookahead),
                    "Real literal constant has an exponent letter but no exponent",
                );
                self.token(TokenKind::Unknown, idx0, self.lookahead_idx(lookahead))
            }
        }
    }

    fn finish_real_literal_constant(&mut self, idx0: u32) -> Token {
        match self.peek() {
            Lookahead::Character(idx1, c) if is_exponent_letter(c) => {
                self.bump();
                if !self.skip_continuation() {
                    self.emit_error(
                        MissingExponent,
                        idx0,
                        idx1,
                        "Real literal constant has incomplete exponent part",
                    );
                    self.token(TokenKind::Unknown, idx0, idx1)
                } else {
                    self.finish_exponent(idx0)
                }
            }
            lookahead => self.token(
                TokenKind::RealLiteralConstant,
                idx0,
                self.lookahead_idx(lookahead),
            ),
        }
    }

    fn decimal(&mut self, idx0: u32) -> Token {
        debug_assert!(match self.peek() {
            Lookahead::Character(_, c) if is_digit(c) => true,
            _ => false,
        });

        let idx1 = self.take_digit_string(idx0);
        debug_assert_ne!(idx1, idx0);

        self.finish_real_literal_constant(idx0)
    }

    fn numberish(&mut self, idx0: u32) -> Token {
        let idx1 = self.take_digit_string(idx0);

        match self.peek() {
            Lookahead::Character(idx1, '.') => {
                self.bump();
                if !self.skip_continuation() {
                    self.token(TokenKind::RealLiteralConstant, idx0, idx1)
                } else {
                    match self.peek() {
                        Lookahead::Character(_, c) if is_digit(c) => self.decimal(idx0),
                        Lookahead::Character(_, c) if is_exponent_letter(c) => {
                            self.finish_real_literal_constant(idx0)
                        }
                        Lookahead::Character(idx1, _) => {
                            self.token(TokenKind::RealLiteralConstant, idx0, idx1)
                        }
                        Lookahead::EOF => {
                            self.token(TokenKind::RealLiteralConstant, idx0, self.text_len())
                        }
                    }
                }
            }
            Lookahead::Character(idx1, c) if is_exponent_letter(c) => {
                self.bump();
                if !self.skip_continuation() {
                    self.emit_error(
                        MissingExponent,
                        idx0,
                        idx1,
                        "Real literal constant has incomplete exponent part",
                    );
                    self.token(TokenKind::Unknown, idx0, idx1)
                } else {
                    self.finish_exponent(idx0)
                }
            }
            _ => self.token(TokenKind::DigitString, idx0, idx1),
        }
    }

    // expected that last seen character was '!'
    // returns nothing - merely advances to end of comment.
    fn commentary(&mut self, idx0: u32) -> Token {
        let idx1 = self
            .take_until(idx0, |lookahead: Lookahead| match lookahead {
                Lookahead::Character(_, c) if is_new_line_start(c) => Stop,
                Lookahead::EOF => Stop,
                _ => Continue,
            })
            .expect("Internal compiler error: Commentary never fails to terminate");

        // ignore the newline for commentary - commentary can be considered EOL
        match self.peek() {
            Lookahead::Character(_, c) if is_new_line_start(c) => {
                self.consume_new_line();
            }
            _ => {}
        }

        self.token(TokenKind::Commentary, idx0, idx1)
    }

    /// Parses a C style block comment
    fn c_block_commentary(&mut self, idx0: u32) -> Token {
        let mut maybe_end = false;
        let idx1 = self.take_until_ignore_continuation(|lookahead: Lookahead| match lookahead {
            Lookahead::Character(_, '*') => {
                maybe_end = true;
                Continue
            }
            Lookahead::Character(_, '/') if maybe_end => Stop,
            Lookahead::Character(_, _) => {
                maybe_end = false;
                Continue
            }
            Lookahead::EOF => Abort,
        });

        match idx1 {
            Ok(idx1) => {
                self.bump(); // consume '/'
                self.token(TokenKind::CBlockCommentary, idx0, idx1)
            }
            Err(idx1) => {
                self.emit_error(
                    ParserErrorCode::UnterminatedCBlockComment,
                    idx0,
                    idx1,
                    "Unterminated block comment",
                );
                self.token(TokenKind::Unknown, idx0, idx1)
            }
        }
    }

    // call when you want to consume a new-line token. Test if at the start of
    // a new line with is_new_line_start.
    fn consume_new_line(&mut self) -> Token {
        loop {
            return match self.peek() {
                Lookahead::Character(idx0, '\n') => {
                    self.bump();
                    self.token(TokenKind::NewLine, idx0, idx0 + 1)
                }
                Lookahead::Character(idx0, '\r') => {
                    match self.bump() {
                        Lookahead::Character(_, '\n') => {
                            self.bump();
                            self.token(TokenKind::NewLine, idx0, idx0 + 2)
                        }
                        // CR is not a supported line ending
                        _ => {
                            self.emit_error(
                                InvalidCarriageReturn,
                                idx0,
                                idx0 + 1,
                                "CR is not a valid line ending. Only CRLF and LF are supported.",
                            );
                            // But still act like it is so we can at least progress.
                            self.token(TokenKind::NewLine, idx0, idx0 + 1)
                        }
                    }
                }
                _ => panic!("self.peek() must match a new line start"),
            };
        }
    }

    fn continuation(&mut self, idx0: u32) -> bool {
        let mut first_line = true;

        loop {
            return match self.peek() {
                Lookahead::Character(idx0, '!') => {
                    self.bump();
                    self.commentary(idx0);
                    continue;
                }
                Lookahead::Character(_, c) if is_new_line_start(c) => {
                    first_line = false;
                    self.consume_new_line();
                    continue;
                }
                Lookahead::Character(_, s) if s.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Lookahead::Character(idx0, _) if first_line => {
                    self.bump();
                    let idx1 = loop {
                        match self.bump() {
                            Lookahead::Character(idx1, '!') => break idx1,
                            Lookahead::Character(idx1, c) if is_new_line_start(c) => break idx1,
                            Lookahead::EOF => break self.text_len(),
                            _ => continue,
                        }
                    };

                    self.emit_error(
                        UnexpectedPostContinuationCharacter, idx0, idx1,
                        "Nothing may follow a line continuation besides commentary, whitespace, or \
                        a newline");
                    continue;
                }
                // If an & is encountered, we're done processing the
                // continuation. The caller should continue whatever
                // tokenization process it was previously performing.
                Lookahead::Character(_, '&') => {
                    self.bump();
                    true
                }
                // If a new token is encountered, then the continuation has
                // ended. The new character is a new token.
                Lookahead::Character(_, _) => false,
                // Treat an unterminated continuation as one that does not continue a token
                Lookahead::EOF => {
                    false
                }
            };
        }
    }

    /// Can be called at any time during tokenization. Should be called at every
    /// character when consuming a multi-character token.
    ///
    /// Returns None if continuation is skipped without issue. Return Some(err)
    /// if there was an issue in the continuation.
    fn skip_continuation(&mut self) -> bool {
        if let Lookahead::Character(idx0, '&') = self.peek() {
            self.bump();
            self.continuation(idx0)
        } else {
            true
        }
    }

    fn get_next(&mut self, lookahead: (u32, char)) -> Token {
        match lookahead {
            (idx0, '=') => {
                self.bump();

                if !self.skip_continuation() {
                    self.token(TokenKind::Equals, idx0, idx0 + 1)
                } else {
                    match self.peek() {
                        Lookahead::Character(idx1, '>') => {
                            self.bump();
                            self.token(TokenKind::Arrow, idx0, idx1 + 1)
                        }
                        Lookahead::Character(idx1, '=') => {
                            self.bump();
                            self.token(TokenKind::EqualsEquals, idx0, idx1 + 1)
                        }
                        _ => self.token(TokenKind::Equals, idx0, idx0 + 1),
                    }
                }
            }
            (idx0, '+') => {
                self.bump();
                self.token(TokenKind::Plus, idx0, idx0 + 1)
            }
            (idx0, '-') => {
                self.bump();
                self.token(TokenKind::Minus, idx0, idx0 + 1)
            }
            (idx0, '*') => {
                self.bump();
                if !self.skip_continuation() {
                    self.token(TokenKind::Star, idx0, idx0 + 1)
                } else {
                    match self.peek() {
                        Lookahead::Character(idx1, '*') => {
                            self.bump();
                            self.token(TokenKind::StarStar, idx0, idx1 + 1)
                        }
                        _ => self.token(TokenKind::Star, idx0, idx0 + 1),
                    }
                }
            }
            (idx0, '/') => {
                self.bump();
                if !self.skip_continuation() {
                    self.token(TokenKind::Slash, idx0, idx0 + 1)
                } else {
                    match self.peek() {
                        Lookahead::Character(idx1, '/') => {
                            self.bump();
                            self.token(TokenKind::SlashSlash, idx0, idx1 + 1)
                        }
                        Lookahead::Character(idx1, '=') => {
                            self.bump();
                            self.token(TokenKind::SlashEquals, idx0, idx1 + 1)
                        }
                        Lookahead::Character(_, '*') if self.tokenize_preprocessor => {
                            self.bump();
                            self.c_block_commentary(idx0)
                        }
                        _ => self.token(TokenKind::Slash, idx0, idx0 + 1),
                    }
                }
            }
            (idx0, '%') => {
                self.bump();
                (self.token(TokenKind::Percent, idx0, idx0 + 1))
            }
            (idx0, '(') => {
                self.bump();
                (self.token(TokenKind::LeftParen, idx0, idx0 + 1))
            }
            (idx0, ')') => {
                self.bump();
                (self.token(TokenKind::RightParen, idx0, idx0 + 1))
            }
            (idx0, '[') => {
                self.bump();
                (self.token(TokenKind::LeftBracket, idx0, idx0 + 1))
            }
            (idx0, ']') => {
                self.bump();
                (self.token(TokenKind::RightBracket, idx0, idx0 + 1))
            }
            (idx0, '<') => {
                self.bump();
                if !self.skip_continuation() {
                    (self.token(TokenKind::LeftAngle, idx0, idx0 + 1))
                } else {
                    match self.peek() {
                        Lookahead::Character(idx1, '=') => {
                            self.bump();
                            (self.token(TokenKind::LeftAngleEquals, idx0, idx1 + 1))
                        }
                        _ => (self.token(TokenKind::LeftAngle, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, '>') => {
                self.bump();
                if !self.skip_continuation() {
                    (self.token(TokenKind::RightAngle, idx0, idx0 + 1))
                } else {
                    match self.peek() {
                        Lookahead::Character(idx1, '=') => {
                            self.bump();
                            (self.token(TokenKind::RightAngleEquals, idx0, idx1 + 1))
                        }
                        _ => (self.token(TokenKind::RightAngle, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, '.') => {
                self.bump();
                if !self.skip_continuation() {
                    (self.token(TokenKind::Dot, idx0, idx0 + 1))
                } else {
                    match self.peek() {
                        // if followed by a letter, then this must be an operator
                        Lookahead::Character(_, c) if is_letter(c) => self.operator(idx0),
                        // If followed by a digit, then this must be an real literal constant
                        Lookahead::Character(_, c) if is_digit(c) => self.decimal(idx0),
                        // else just return the dot token
                        _ => (self.token(TokenKind::Dot, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, ',') => {
                self.bump();
                (self.token(TokenKind::Comma, idx0, idx0 + 1))
            }
            (idx0, ':') => {
                self.bump();
                if !self.skip_continuation() {
                    (self.token(TokenKind::Colon, idx0, idx0 + 1))
                } else {
                    match self.peek() {
                        Lookahead::Character(idx1, ':') => {
                            self.bump();
                            (self.token(TokenKind::ColonColon, idx0, idx1 + 1))
                        }
                        _ => (self.token(TokenKind::Colon, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, ';') => {
                self.bump();
                (self.token(TokenKind::SemiColon, idx0, idx0 + 1))
            }
            (idx0, c) if (c == '"' || c == '\'') => {
                self.bump();
                self.string_literal(idx0, c)
            }
            (idx0, c) if is_digit(c) => {
                self.bump();
                self.numberish(idx0)
            }
            (idx0, '!') => {
                self.bump();
                (self.commentary(idx0))
            }
            (idx0, c) if is_identifier_start(c) => {
                self.bump();
                (self.identifierish(idx0))
            }
            (idx0, '#') if self.tokenize_preprocessor => {
                self.bump();
                self.token(TokenKind::Pound, idx0, idx0 + 1)
            }
            (idx, _) => {
                self.bump();
                self.unrecognized_token(idx)
            }
        }
    }

    fn internal_next(&mut self) -> Option<Token> {
        loop {
            return match self.peek() {
                Lookahead::Character(_, '&') => {
                    self.skip_continuation();
                    continue;
                }
                Lookahead::Character(_, c) if is_new_line_start(c) => Some(self.consume_new_line()),
                Lookahead::Character(_, c) if c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Lookahead::Character(idx, c) => Some(self.get_next((idx, c))),
                Lookahead::EOF => None,
            };
        }
    }

    fn take_until_terminate<F>(
        &mut self,
        idx0: u32,
        require_continue_context: bool,
        mut terminate: F,
    ) -> Result<u32, u32>
    where
        F: FnMut(Lookahead) -> TakeUntil,
    {
        loop {
            return match self.peek() {
                Lookahead::Character(idx1, '&') => {
                    self.bump();
                    let continue_context = self.continuation(idx0);
                    if require_continue_context && !continue_context {
                        let lookahead = self.peek();
                        self.emit_error(
                            DiscontinuedCharacterContext,
                            idx1,
                            self.lookahead_idx(lookahead),
                            "Missing continuation ('&') of string literal",
                        );
                    }

                    continue;
                }
                Lookahead::EOF => match terminate(Lookahead::EOF) {
                    Continue => panic!("Internal error: Tried to tokenize past EOF!"),
                    Stop => Ok(self.text_len()),
                    Abort => Err(self.text_len()),
                },
                Lookahead::Character(idx1, c) => match terminate(Lookahead::Character(idx1, c)) {
                    Continue => {
                        self.bump();
                        continue;
                    }
                    Stop => Ok(idx1),
                    Abort => Err(idx1),
                },
            };
        }
    }

    fn take_until_ignore_continuation<F>(&mut self, mut terminate: F) -> Result<u32, u32>
    where
        F: FnMut(Lookahead) -> TakeUntil,
    {
        loop {
            return match self.peek() {
                Lookahead::EOF => match terminate(Lookahead::EOF) {
                    Continue => panic!("Internal error: Tried to tokenize past EOF!"),
                    Stop => Ok(self.text_len()),
                    Abort => Err(self.text_len()),
                },
                Lookahead::Character(idx1, c) => match terminate(Lookahead::Character(idx1, c)) {
                    Continue => {
                        self.bump();
                        continue;
                    }
                    Stop => Ok(idx1),
                    Abort => Err(idx1),
                },
            };
        }
    }

    fn take_until<F>(&mut self, idx0: u32, terminate: F) -> Result<u32, u32>
    where
        F: FnMut(Lookahead) -> TakeUntil,
    {
        self.take_until_terminate(idx0, false, terminate)
    }

    fn peek_nth(&mut self, n: usize) -> Lookahead {
        self.chars
            .peek_nth(n)
            .map(|(i, c)| Lookahead::Character(*i as u32, *c))
            .unwrap_or(Lookahead::EOF)
    }

    fn peek(&mut self) -> Lookahead {
        self.peek_nth(0)
    }

    fn bump(&mut self) -> Lookahead {
        self.chars
            .next()
            .map(|(i, c)| Lookahead::Character(i as u32, c))
            .unwrap_or(Lookahead::EOF)
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.internal_next()
    }
}

// assumed that starting . has been consumed
// the final character will need to be validated as a . and consumed
fn is_letter(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

fn is_exponent_letter(c: char) -> bool {
    c == 'e' || c == 'E' || c == 'd' || c == 'D'
}

fn is_operator_continue(c: char) -> bool {
    is_letter(c)
}

fn is_identifier_start(c: char) -> bool {
    is_letter(c)
}

fn is_identifier_continue(c: char) -> bool {
    is_letter(c) || is_digit(c) || (c == '_')
}

fn is_new_line_start(c: char) -> bool {
    c == '\r' || c == '\n'
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_recognized_character(c: char, tokenize_preprocessor: bool) -> bool {
    is_letter(c)
        || is_digit(c)
        || c.is_whitespace()
        || match c {
            '_' | '=' | '+' | '-' | '*' | '/' | '\\' | '(' | ')' | '[' | ']' | ',' | '.' | ':'
            | ';' | '!' | '"' | '%' | '&' | '<' | '>' => true,
            '#' => tokenize_preprocessor,
            _ => false,
        }
}
