use std::cell::RefCell;
use std::collections::VecDeque;
use std::iter::Iterator;

use num_traits::FromPrimitive;

use self::TakeUntil::*;

use super::preprocessor::FortranPreprocessor;

use crate::error::{
    AnalysisErrorKind, DiagnosticSink,
    ParserErrorCode::{self, *},
};
use crate::index::FileId;
use crate::span::Span;
use crate::string::{CaseInsensitiveContinuationStr, ContinuationStr};

#[cfg(test)]
mod tests;

mod token;

use token::*;
pub use token::{KeywordTokenKind, Letter, Token, TokenKind};

#[derive(Clone, Copy, Default)]
pub struct TokenizerOptions {
    pub tokenize_preprocessor: bool,
}

/// Used to control how the file is lexed. Can be switched into different modes based on feedback
/// from higher level parser.
#[derive(Copy, Clone)]
pub enum LexMode {
    Normal,
    Format,
}

#[derive(Clone)]
pub struct Tokenizer<'input> {
    chars: FortranPreprocessor<'input>,
    tokenize_preprocessor: bool,
    mode: LexMode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TakeUntil {
    Continue,
    Stop,
    Abort,
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
            chars: FortranPreprocessor::new(
                options.tokenize_preprocessor,
                file_id,
                text,
                diagnostics,
            ),
            tokenize_preprocessor: options.tokenize_preprocessor,
            mode: LexMode::Normal,
        }
    }

    /// Lexing behavior changes depending on whether we're in "normal" mode or "format" mode.
    pub fn set_lex_mode(&mut self, mode: LexMode) {
        self.mode = mode;
        match self.mode {
            LexMode::Format => {
                self.chars.insignificant_whitespace(true);
            }
            LexMode::Normal => {
                self.chars.insignificant_whitespace(false);
            }
        }
    }

    fn lookahead_idx(&self, lookahead: Option<(u32, char)>) -> u32 {
        match lookahead {
            Some((idx, _)) => idx,
            None => self.text_len(),
        }
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

    fn token(&self, kind: TokenKind, start: u32, end: u32) -> Token {
        Token {
            kind,
            span: Span {
                file_id: self.chars.file_id,
                start,
                end,
            },
        }
    }

    fn operator(&mut self, idx0: u32) -> Token {
        let maybe_idx1 = self.take_until(|lookahead| match lookahead {
            Some((_, c)) if is_operator_continue(c) => Continue,
            Some((_, '.')) => Stop,
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
            .take_until(|lookahead| match lookahead {
                Some((_, c)) if is_identifier_continue(c) => Continue,
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
            .take_until(|lookahead| match lookahead {
                Some((_, c)) if is_recognized_character(c, tokenize_preprocessor) => Stop,
                None => Stop,
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
        let maybe_idx1 = self.take_until(|lookahead| {
            if saw_closing_quote {
                match lookahead {
                    Some((_, c)) if c == quote => {
                        saw_closing_quote = false;
                        Continue
                    }
                    Some((_, _)) | None => Stop,
                }
            } else {
                match lookahead {
                    Some((_, c)) if c == quote => {
                        saw_closing_quote = true;
                        Continue
                    }
                    Some((_, c)) if is_new_line_start(c) => Abort,
                    Some((_, _)) => Continue,
                    None => Abort,
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

    fn take_digit_string(&mut self) -> u32 {
        self.take_until(|lookahead| match lookahead {
            Some((_, c)) if is_digit(c) => Continue,
            _ => Stop,
        })
        .expect("Internal compiler error: Did not expect abort when taking a digit string")
    }

    fn finish_exponent(&mut self, idx0: u32) -> Token {
        if let Some((_, '+')) | Some((_, '-')) = self.peek() {
            self.bump();
        }

        match self.peek() {
            Some((_, c)) if is_digit(c) => {
                let idx1 = self.take_digit_string();
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
            Some((_, c)) if is_exponent_letter(c) => {
                self.bump();
                self.finish_exponent(idx0)
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
            Some((_, c)) if is_digit(c) => true,
            _ => false,
        });

        let idx1 = self.take_digit_string();
        debug_assert_ne!(idx1, idx0);

        self.finish_real_literal_constant(idx0)
    }

    fn numberish(&mut self, idx0: u32) -> Token {
        let idx1 = self.take_digit_string();

        match self.peek() {
            Some((_, '.')) => {
                self.bump();
                match self.peek() {
                    Some((_, c)) if is_digit(c) => self.decimal(idx0),
                    Some((_, c)) if is_exponent_letter(c) => {
                        self.finish_real_literal_constant(idx0)
                    }
                    Some((idx1, _)) => self.token(TokenKind::RealLiteralConstant, idx0, idx1),
                    None => self.token(TokenKind::RealLiteralConstant, idx0, self.text_len()),
                }
            }
            Some((_, c)) if is_exponent_letter(c) => {
                self.bump();
                self.finish_exponent(idx0)
            }
            _ => self.token(TokenKind::DigitString, idx0, idx1),
        }
    }

    // expected that last seen character was '!'
    // returns nothing - merely advances to end of comment.
    fn commentary(&mut self, idx0: u32) -> Token {
        let idx1 = self
            .take_until(|lookahead| match lookahead {
                Some((_, c)) if is_new_line_start(c) => Stop,
                None => Stop,
                _ => Continue,
            })
            .expect("Internal compiler error: Commentary never fails to terminate");

        // ignore the newline for commentary - commentary can be considered EOL
        match self.peek() {
            Some((_, c)) if is_new_line_start(c) => {
                self.consume_new_line();
            }
            _ => {}
        }

        self.token(TokenKind::Commentary, idx0, idx1)
    }

    /// Parses a C style block comment
    fn c_block_commentary(&mut self, idx0: u32) -> Token {
        let mut maybe_end = false;
        let idx1 = self.take_until(|lookahead| match lookahead {
            Some((_, '*')) => {
                maybe_end = true;
                Continue
            }
            Some((_, '/')) if maybe_end => Stop,
            Some((_, _)) => {
                maybe_end = false;
                Continue
            }
            None => Abort,
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
                Some((idx0, '\n')) => {
                    self.bump();
                    self.token(TokenKind::NewLine, idx0, idx0 + 1)
                }
                _ => panic!("self.peek() must match a new line start"),
            };
        }
    }

    fn get_next(&mut self) -> Token {
        if self.at_binary_constant_start() {
            return self.take_binary_constant();
        }

        if self.at_octal_constant_start() {
            return self.take_octal_constant();
        }

        if self.at_hex_constant_start() {
            return self.take_hex_constant();
        }

        let peeked = match self.peek() {
            Some((i, c)) => (i, c),
            _ => panic!("Internal compiler error: We should be at EOF"),
        };

        match peeked {
            (idx0, '=') => {
                self.bump();

                match self.peek() {
                    Some((idx1, '>')) => {
                        self.bump();
                        self.token(TokenKind::Arrow, idx0, idx1 + 1)
                    }
                    Some((idx1, '=')) => {
                        self.bump();
                        self.token(TokenKind::EqualsEquals, idx0, idx1 + 1)
                    }
                    _ => self.token(TokenKind::Equals, idx0, idx0 + 1),
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
                match self.peek() {
                    Some((idx1, '*')) => {
                        self.bump();
                        self.token(TokenKind::StarStar, idx0, idx1 + 1)
                    }
                    _ => self.token(TokenKind::Star, idx0, idx0 + 1),
                }
            }
            (idx0, '/') => {
                self.bump();
                match self.peek() {
                    Some((idx1, '/')) => {
                        self.bump();
                        self.token(TokenKind::SlashSlash, idx0, idx1 + 1)
                    }
                    Some((idx1, '=')) => {
                        self.bump();
                        self.token(TokenKind::SlashEquals, idx0, idx1 + 1)
                    }
                    Some((idx1, ')')) => {
                        self.bump();
                        self.token(TokenKind::SlashRightParen, idx0, idx1 + 1)
                    }
                    Some((_, '*')) if self.tokenize_preprocessor => {
                        self.bump();
                        self.c_block_commentary(idx0)
                    }
                    _ => self.token(TokenKind::Slash, idx0, idx0 + 1),
                }
            }
            (idx0, '%') => {
                self.bump();
                self.token(TokenKind::Percent, idx0, idx0 + 1)
            }
            (idx0, '(') => {
                self.bump();
                match self.peek() {
                    Some((idx1, '/')) => {
                        self.bump();
                        self.token(TokenKind::LeftParenSlash, idx0, idx1 + 1)
                    }
                    _ => self.token(TokenKind::LeftParen, idx0, idx0 + 1),
                }
            }
            (idx0, ')') => {
                self.bump();
                self.token(TokenKind::RightParen, idx0, idx0 + 1)
            }
            (idx0, '[') => {
                self.bump();
                self.token(TokenKind::LeftBracket, idx0, idx0 + 1)
            }
            (idx0, ']') => {
                self.bump();
                self.token(TokenKind::RightBracket, idx0, idx0 + 1)
            }
            (idx0, '<') => {
                self.bump();
                match self.peek() {
                    Some((idx1, '=')) => {
                        self.bump();
                        self.token(TokenKind::LeftAngleEquals, idx0, idx1 + 1)
                    }
                    _ => self.token(TokenKind::LeftAngle, idx0, idx0 + 1),
                }
            }
            (idx0, '>') => {
                self.bump();
                match self.peek() {
                    Some((idx1, '=')) => {
                        self.bump();
                        self.token(TokenKind::RightAngleEquals, idx0, idx1 + 1)
                    }
                    _ => self.token(TokenKind::RightAngle, idx0, idx0 + 1),
                }
            }
            (idx0, '.') => {
                self.bump();
                match self.peek() {
                    // if followed by a letter, then this must be an operator
                    Some((_, c)) if is_letter(c) => self.operator(idx0),
                    // If followed by a digit, then this must be an real literal constant
                    Some((_, c)) if is_digit(c) => self.decimal(idx0),
                    // else just return the dot token
                    _ => self.token(TokenKind::Dot, idx0, idx0 + 1),
                }
            }
            (idx0, ',') => {
                self.bump();
                self.token(TokenKind::Comma, idx0, idx0 + 1)
            }
            (idx0, ':') => {
                self.bump();
                match self.peek() {
                    Some((idx1, ':')) => {
                        self.bump();
                        self.token(TokenKind::ColonColon, idx0, idx1 + 1)
                    }
                    _ => self.token(TokenKind::Colon, idx0, idx0 + 1),
                }
            }
            (idx0, ';') => {
                self.bump();
                self.token(TokenKind::SemiColon, idx0, idx0 + 1)
            }
            (idx0, '_') => {
                self.bump();
                self.token(TokenKind::Underscore, idx0, idx0 + 1)
            }
            (idx0, c) if (c == '"' || c == '\'') => {
                self.bump();
                self.string_literal(idx0, c)
            }
            (idx0, c) if c.is_ascii_digit() => {
                self.bump();
                self.numberish(idx0)
            }
            (idx0, '!') => {
                self.bump();
                self.commentary(idx0)
            }
            (idx0, c) if is_identifier_start(c) => {
                self.bump();
                self.identifierish(idx0)
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
            return match self.peek()?.1 {
                c if is_new_line_start(c) => Some(self.consume_new_line()),
                c if c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                _ => Some(self.get_next()),
            };
        }
    }

    fn take_until<F>(&mut self, mut terminate: F) -> Result<u32, u32>
    where
        F: FnMut(Option<(u32, char)>) -> TakeUntil,
    {
        loop {
            return match self.peek() {
                None => match terminate(None) {
                    Continue => panic!("Internal error: Tried to tokenize past EOF!"),
                    Stop => Ok(self.text_len()),
                    Abort => Err(self.text_len()),
                },
                Some((idx1, c)) => match terminate(Some((idx1, c))) {
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

    /// R765: binary-constant
    fn at_binary_constant_start(&mut self) -> bool {
        let mut chars = self.chars.clone().map(|(_, c)| c);

        match chars.next() {
            Some('b') | Some('B') => {}
            _ => return false,
        };

        match chars.next() {
            Some('\'') | Some('"') => true,
            _ => false,
        }
    }

    /// R765: binary-constant
    fn take_binary_constant(&mut self) -> Token {
        let idx0 = match self.peek().expect("ICE") {
            (idx, 'b') | (idx, 'B') => {
                self.bump();
                idx
            }
            _ => panic!("ICE"),
        };

        let quote = match self.peek().expect("ICE").1 {
            c if c == '\'' || c == '"' => {
                self.bump();
                c
            }
            _ => panic!("ICE"),
        };

        let result = match self.peek() {
            Some((_, c)) if is_binary_digit(c) => {
                self.bump();
                self.take_until(|lookahead| match lookahead {
                    Some((_, c)) if is_binary_digit(c) => TakeUntil::Continue,
                    Some((_, c)) if c == quote => TakeUntil::Stop,
                    _ => TakeUntil::Abort,
                })
            }
            Some((idx, _)) => Err(idx),
            None => Err(self.text_len()),
        };

        match result {
            Ok(idx1) => {
                self.bump();
                self.token(TokenKind::BinaryConstant, idx0, idx1 + 1)
            }
            Err(idx1) => {
                self.emit_error(
                    ExpectedBinaryConstant,
                    idx0,
                    idx1,
                    &format!(
                        "Expected `{}` to be a binary constant.",
                        ContinuationStr::new(self.text_span(idx0, idx1))
                    ),
                );
                self.token(TokenKind::Unknown, idx0, idx1)
            }
        }
    }

    /// R766: octal-constant
    fn at_octal_constant_start(&mut self) -> bool {
        let mut chars = self.chars.clone().map(|(_, c)| c);

        match chars.next() {
            Some('o') | Some('O') => {}
            _ => return false,
        };

        match chars.next() {
            Some('\'') | Some('"') => true,
            _ => false,
        }
    }

    /// R766: octal-constant
    fn take_octal_constant(&mut self) -> Token {
        let idx0 = match self.peek().expect("ICE") {
            (idx, 'o') | (idx, 'O') => {
                self.bump();
                idx
            }
            _ => panic!("ICE"),
        };

        let quote = match self.peek().expect("ICE").1 {
            c if c == '\'' || c == '"' => {
                self.bump();
                c
            }
            _ => panic!("ICE"),
        };

        let result = match self.peek() {
            Some((_, c)) if is_octal_digit(c) => {
                self.bump();
                self.take_until(|lookahead| match lookahead {
                    Some((_, c)) if is_octal_digit(c) => TakeUntil::Continue,
                    Some((_, c)) if c == quote => TakeUntil::Stop,
                    _ => TakeUntil::Abort,
                })
            }
            Some((idx, _)) => Err(idx),
            None => Err(self.text_len()),
        };

        match result {
            Ok(idx1) => {
                self.bump();
                self.token(TokenKind::OctalConstant, idx0, idx1 + 1)
            }
            Err(idx1) => {
                self.emit_error(
                    ExpectedOctalConstant,
                    idx0,
                    idx1,
                    &format!(
                        "Expected `{}` to be an octal constant.",
                        ContinuationStr::new(self.text_span(idx0, idx1))
                    ),
                );
                self.token(TokenKind::Unknown, idx0, idx1)
            }
        }
    }

    /// R767: hex-constant
    fn at_hex_constant_start(&mut self) -> bool {
        let mut chars = self.chars.clone().map(|(_, c)| c);

        match chars.next() {
            Some('z') | Some('Z') => {}
            _ => return false,
        };

        match chars.next() {
            Some('\'') | Some('"') => true,
            _ => false,
        }
    }

    /// R767: hex-constant
    fn take_hex_constant(&mut self) -> Token {
        let idx0 = match self.peek().expect("ICE") {
            (idx, 'z') | (idx, 'Z') => {
                self.bump();
                idx
            }
            _ => panic!("ICE"),
        };

        let quote = match self.peek().expect("ICE").1 {
            c if c == '\'' || c == '"' => {
                self.bump();
                c
            }
            _ => panic!("ICE"),
        };

        let result = match self.peek() {
            Some((_, c)) if is_hex_digit(c) => {
                self.bump();
                self.take_until(|lookahead| match lookahead {
                    Some((_, c)) if is_hex_digit(c) => TakeUntil::Continue,
                    Some((_, c)) if c == quote => TakeUntil::Stop,
                    _ => TakeUntil::Abort,
                })
            }
            Some((idx, _)) => Err(idx),
            None => Err(self.text_len()),
        };

        match result {
            Ok(idx1) => {
                self.bump();
                self.token(TokenKind::HexConstant, idx0, idx1 + 1)
            }
            Err(idx1) => {
                self.emit_error(
                    ExpectedHexConstant,
                    idx0,
                    idx1,
                    &format!(
                        "Expected `{}` to be a hex constant.",
                        ContinuationStr::new(self.text_span(idx0, idx1))
                    ),
                );
                self.token(TokenKind::Unknown, idx0, idx1)
            }
        }
    }

    fn peek(&mut self) -> Option<(u32, char)> {
        self.chars.clone().next().map(|(i, c)| (i as u32, c))
    }

    fn bump(&mut self) -> Option<(u32, char)> {
        self.chars.next().map(|(i, c)| (i as u32, c))
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
    // CRs are preprocessed out, so we don't have to check for them in the Tokenizer
    c == '\n'
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_binary_digit(c: char) -> bool {
    c == '0' || c == '1'
}

fn is_octal_digit(c: char) -> bool {
    c >= '0' && c <= '7'
}

fn is_hex_digit(c: char) -> bool {
    is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
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

/// A Tokenizer that can lookahead and can efficiently flush the peeked tokens when a "mode" change
/// occurs.
pub struct PeekableTokenizer<'input> {
    tokenizer: Tokenizer<'input>,
    peeking_tokenizer: Option<Tokenizer<'input>>,
    peeked: VecDeque<Token>,
    num_behind: usize,
}

impl<'input> PeekableTokenizer<'input> {
    pub fn new(
        options: &TokenizerOptions,
        file_id: FileId,
        text: &'input str,
        diagnostics: &'input RefCell<DiagnosticSink>,
    ) -> Self {
        Self {
            tokenizer: Tokenizer::new(options, file_id, text, diagnostics),
            peeking_tokenizer: None,
            peeked: VecDeque::new(),
            num_behind: 0,
        }
    }

    /// Get the tokenizer up-to-date with the peeking tokenizer and discard peeked state.
    fn reset(&mut self) {
        self.peeked.clear();
        self.peeking_tokenizer = None;

        // If the non-peeking tokenizer is behind, it needs to be caught up by manually advancing
        // it.
        while self.num_behind > 0 {
            self.next();
            self.num_behind -= 1;
        }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.peek_nth(0)
    }

    pub fn peek_nth(&mut self, n: usize) -> Option<&Token> {
        let tokenizer = &self.tokenizer;
        let peeking_tokenizer = &mut self.peeking_tokenizer;

        let tokenizer = peeking_tokenizer.get_or_insert_with(|| tokenizer.clone());

        while n + 1 > self.peeked.len() {
            if let Some(t) = tokenizer.next() {
                self.peeked.push_back(t);
            } else {
                if self.peeked.is_empty() {
                    self.peeking_tokenizer = None;
                }
                return None;
            }
        }

        Some(&self.peeked[n])
    }

    /// Lexing behavior changes depending on whether we're in "normal" mode or "format" mode.
    pub fn set_lex_mode(&mut self, mode: LexMode) {
        self.reset();
        self.tokenizer.set_lex_mode(mode);
    }
}

impl<'input> Iterator for PeekableTokenizer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // If some tokens have been peeked already
        if !self.peeked.is_empty() {
            // track that the tokenizer is behind, so will need to catch up if there's a mode
            // change.
            self.num_behind += 1;
            let result = self.peeked.pop_front();

            // If there are no more peeked tokens, reset the peeked state and fast-forward the
            // tokenizer.
            if self.peeked.is_empty() {
                debug_assert!(self.peeking_tokenizer.is_some());
                self.tokenizer = self.peeking_tokenizer.take().unwrap();

                self.num_behind = 0;
            }

            result
        } else {
            self.tokenizer.next()
        }
    }
}
