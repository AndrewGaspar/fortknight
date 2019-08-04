use std::iter::Iterator;
use std::str::CharIndices;

use self::ErrorCode::*;
use self::TakeUntil::*;

use crate::index::FileId;
use crate::span::Span;

#[cfg(test)]
mod tests;

mod token;

use token::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub span: Span,
    pub code: ErrorCode,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    UnrecognizedToken,
    UnterminatedStringLiteral,
    UnterminatedOperator,
    UnterminatedContinuationLine,
    InvalidCarriageReturn,
    UnexpectedToken,
    MissingExponent,
    DiscontinuedCharacterContext,
}

impl ErrorCode {
    pub fn code(self) -> u16 {
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
enum TakeUntil {
    Continue,
    Stop,
    Error(ErrorCode),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Lookahead {
    Character(char),
    EOF,
}

#[derive(Clone, Debug)]
pub struct UserStr<'input> {
    string: &'input str,
}

impl<'input> UserStr<'input> {
    pub fn new(string: &'input str) -> UserStr {
        debug_assert!(string.is_ascii());

        UserStr { string: string }
    }

    pub fn iter(&self) -> UserStrIterator<'input> {
        UserStrIterator::new(self.string.chars())
    }
}

impl<'input> PartialEq for UserStr<'input> {
    fn eq(&self, other: &UserStr) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<'input> Eq for UserStr<'input> {}

impl<'input> ToString for UserStr<'input> {
    fn to_string(&self) -> String {
        use std::iter::FromIterator;
        String::from_iter(self.iter())
    }
}

#[derive(Clone, Debug)]
pub struct UserStrIterator<'input> {
    str_iter: std::str::Chars<'input>,
}

impl<'input> UserStrIterator<'input> {
    fn new(str_iter: std::str::Chars<'input>) -> UserStrIterator<'input> {
        UserStrIterator { str_iter: str_iter }
    }
}

// Iterator over a FortranUserStr. Ignores continuation. This allows us to
// tokenize the FORTRAN program without allocating any memory.
impl<'input> Iterator for UserStrIterator<'input> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        // if we're here, we can assume that the string is already a
        // valid identifier, which means the continuation is properly
        // terminated. Just continue until we see a closing ampersand.
        loop {
            return match self.str_iter.next() {
                Some(amp) if amp == '&' => {
                    while '&' != self.str_iter.next().unwrap() {}
                    continue;
                }
                Some(x) => Some(x),
                None => None,
            };
        }
    }
}

#[derive(Clone, Debug)]
pub struct CaseInsensitiveUserStr<'input> {
    user_str: UserStr<'input>,
}

impl<'input> CaseInsensitiveUserStr<'input> {
    pub fn new(string: &'input str) -> CaseInsensitiveUserStr {
        CaseInsensitiveUserStr {
            user_str: UserStr::new(string),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = char> + 'input {
        self.user_str.iter().map(|c: char| c.to_ascii_lowercase())
    }
}

impl<'input> PartialEq for CaseInsensitiveUserStr<'input> {
    fn eq(&self, other: &CaseInsensitiveUserStr) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<'input> Eq for CaseInsensitiveUserStr<'input> {}

impl<'input> ToString for CaseInsensitiveUserStr<'input> {
    fn to_string(&self) -> String {
        use std::iter::FromIterator;
        String::from_iter(self.iter())
    }
}

pub struct Tokenizer<'input> {
    file_id: FileId,
    text: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(u32, char)>,
}

impl<'input> Tokenizer<'input> {
    pub fn new(file_id: FileId, text: &'input str) -> Tokenizer<'input> {
        assert!(
            text.len() <= u32::max_value() as usize,
            "Fortknight only supports a maximum of 4GB files."
        );

        let mut t = Tokenizer {
            file_id,
            text: text,
            chars: text.char_indices(),
            lookahead: None,
        };
        t.bump();
        t
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

    fn error<T>(&self, c: ErrorCode, start: u32, end: u32) -> Result<T, Error> {
        Err(Error {
            span: Span {
                file_id: self.file_id,
                start,
                end,
            },
            code: c,
        })
    }

    fn operator(&mut self, idx0: u32) -> Result<Token, Error> {
        let idx1 = self.take_until(idx0, |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if is_operator_continue(c) => Continue,
            Lookahead::Character('.') => Stop,
            _ => Error(UnterminatedOperator),
        })?;

        // consume .
        self.bump();

        // don't include . in operator name
        let operator = CaseInsensitiveUserStr::new(&self.text_span(idx0 + 1, idx1));

        let kind = INTRINSIC_OPERATORS
            .iter()
            .filter(|&&(w, _)| CaseInsensitiveUserStr::new(w) == operator)
            .map(|&(_, ref t)| t.clone())
            .next()
            .unwrap_or_else(|| TokenKind::DefinedOperator);

        Ok(self.token(kind, idx0, idx1 + 1))
    }

    fn identifierish(&mut self, idx0: u32) -> Result<Token, Error> {
        let idx1 = self.take_until(idx0, |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if is_identifier_continue(c) => Continue,
            _ => Stop,
        })?;

        let word = CaseInsensitiveUserStr::new(&self.text_span(idx0, idx1));

        let kind = if let Some(kind) = KEYWORDS_TRIE.get(&word.to_string()) {
            TokenKind::Keyword(*kind)
        } else {
            TokenKind::Name
        };

        Ok(self.token(kind, idx0, idx1))
    }

    fn unrecognized_token(&mut self, idx0: u32) -> Result<Token, Error> {
        let idx1 = self.take_until(idx0, |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if c.is_whitespace() => Stop,
            Lookahead::EOF => Stop,
            _ => Continue,
        })?;

        self.error(UnrecognizedToken, idx0, idx1)
    }

    fn string_literal(&mut self, idx0: u32, quote: char) -> Result<Token, Error> {
        let mut saw_closing_quote = false;

        // TODO: Add optional, non-standard support for escape sequences. See gfortran
        // -fbackslash option
        // See: https://gcc.gnu.org/onlinedocs/gfortran/Fortran-Dialect-Options.html
        let idx1 = self.take_until_terminate(idx0, true, |lookahead: Lookahead| {
            if saw_closing_quote {
                match lookahead {
                    Lookahead::Character(c) if c == quote => {
                        saw_closing_quote = false;
                        Continue
                    }
                    Lookahead::Character(_) | Lookahead::EOF => Stop,
                }
            } else {
                match lookahead {
                    Lookahead::Character(c) if c == quote => {
                        saw_closing_quote = true;
                        Continue
                    }
                    Lookahead::Character(c) if is_new_line_start(c) => {
                        Error(UnterminatedStringLiteral)
                    }
                    Lookahead::Character(_) => Continue,
                    Lookahead::EOF => Error(UnterminatedStringLiteral),
                }
            }
        })?;

        Ok(self.token(TokenKind::CharLiteralConstant, idx0, idx1))
    }

    fn take_digit_string(&mut self, idx0: u32) -> Result<u32, Error> {
        self.take_until(idx0, |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if is_digit(c) => Continue,
            _ => Stop,
        })
    }

    fn finish_exponent(&mut self, idx0: u32) -> Result<Token, Error> {
        if let Some((_, '+')) | Some((_, '-')) = self.lookahead {
            self.bump();
            self.expect_continuation(ErrorCode::MissingExponent, idx0)?;
        }

        match self.lookahead {
            Some((_, c)) if is_digit(c) => {
                let idx1 = self.take_digit_string(idx0)?;
                Ok(self.token(TokenKind::RealLiteralConstant, idx0, idx1))
            }
            Some((idx1, _)) => self.error(ErrorCode::MissingExponent, idx0, idx1),
            None => self.error(ErrorCode::MissingExponent, idx0, self.text_len()),
        }
    }

    fn finish_real_literal_constant(&mut self, idx0: u32) -> Result<Token, Error> {
        let idx1 = self.take_digit_string(idx0)?;

        match self.lookahead {
            Some((_, c)) if is_exponent_letter(c) => {
                self.bump();
                self.expect_continuation(ErrorCode::MissingExponent, idx0)?;

                self.finish_exponent(idx0)
            }
            _ => Ok(self.token(TokenKind::RealLiteralConstant, idx0, idx1)),
        }
    }

    fn numberish(&mut self, idx0: u32) -> Result<Token, Error> {
        let idx1 = self.take_digit_string(idx0)?;

        match self.lookahead {
            Some((_, '.')) => {
                self.bump();
                if !self.skip_continuation()? {
                    Ok(self.token(TokenKind::RealLiteralConstant, idx0, idx1))
                } else {
                    match self.lookahead {
                        Some((_, c)) if is_digit(c) || is_exponent_letter(c) => {
                            self.finish_real_literal_constant(idx0)
                        }
                        Some((idx1, _)) => {
                            Ok(self.token(TokenKind::RealLiteralConstant, idx0, idx1))
                        }
                        None => {
                            Ok(self.token(TokenKind::RealLiteralConstant, idx0, self.text_len()))
                        }
                    }
                }
            }
            Some((_, c)) if is_exponent_letter(c) => {
                self.bump();
                self.expect_continuation(ErrorCode::MissingExponent, idx0)?;

                self.finish_exponent(idx0)
            }
            _ => Ok(self.token(TokenKind::DigitString, idx0, idx1)),
        }
    }

    // expected that last seen character was '!'
    // returns nothing - merely advances to end of comment.
    fn commentary(&mut self, idx0: u32) -> Result<Token, Error> {
        let idx1 = self.take_until(idx0, |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if is_new_line_start(c) => Stop,
            Lookahead::EOF => Stop,
            _ => Continue,
        })?;

        // skip the newline for commentary - commentary can be considered EOL
        match self.lookahead {
            Some((_, c)) if is_new_line_start(c) => {
                self.consume_new_line().unwrap()?;
            }
            _ => {}
        }

        Ok(self.token(TokenKind::Commentary, idx0, idx1))
    }

    // call when you want to consume a new-line token. Test if at the start of
    // a new line with is_new_line_start.
    fn consume_new_line(&mut self) -> Option<Result<Token, Error>> {
        loop {
            return match self.lookahead {
                Some((idx0, '\n')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::NewLine, idx0, idx0 + 1)))
                }
                Some((idx0, '\r')) => {
                    match self.bump() {
                        Some((_, '\n')) => {
                            self.bump();
                            Some(Ok(self.token(TokenKind::NewLine, idx0, idx0 + 2)))
                        }
                        // CR is not a supported line ending
                        _ => Some(self.error(InvalidCarriageReturn, idx0, idx0 + 1)),
                    }
                }
                Some((_, _)) => panic!("self.lookahead must match a new line start"),
                None => None,
            };
        }
    }

    fn continuation(&mut self, idx0: u32) -> Result<bool, Error> {
        let mut first_line = true;

        loop {
            return match self.lookahead {
                Some((idx0, '!')) => {
                    self.bump();
                    self.commentary(idx0)
                        .expect("Internal error: Unexpected error parsing commentary");
                    continue;
                }
                Some((_, c)) if is_new_line_start(c) => {
                    first_line = false;
                    match self.consume_new_line() {
                        // propagate errors
                        Some(Err(err)) => Err(err),
                        // discard newline token inside of continuation.
                        // EOF ends continuation
                        Some(Ok(_)) | None => continue,
                    }
                }
                Some((_, s)) if s.is_whitespace() => {
                    self.bump();
                    continue;
                }
                // TODO: Read until end of token, log error, continue
                Some((idx1, _)) if first_line => self.error(UnexpectedToken, idx1, idx1 + 1),
                // If an & is encountered, we're done processing the
                // continuation. The caller should continue whatever
                // tokenization process it was previously performing.
                Some((_, '&')) => {
                    self.bump();
                    Ok(true)
                }
                // If a new token is encountered, then the continuation has
                // ended. The new character is a new token.
                Some(_) => Ok(false),
                None => self.error(UnterminatedContinuationLine, idx0, self.text_len()),
            };
        }
    }

    /// Can be called at any time during tokenization. Should be called at every
    /// character when consuming a multi-character token.
    ///
    /// Returns None if continuation is skipped without issue. Return Some(err)
    /// if there was an issue in the continuation.
    fn skip_continuation(&mut self) -> Result<bool, Error> {
        if let Some((idx0, '&')) = self.lookahead {
            self.bump();
            self.continuation(idx0)
        } else {
            Ok(true)
        }
    }

    /// Should be called at any time during tokenization when additional characters are EXPECTED.
    /// If it's not a token-splitting continuation, the ErrorCode is returned.
    fn expect_continuation(&mut self, code: ErrorCode, idx0: u32) -> Result<(), Error> {
        let idx1 = match self.lookahead {
            Some((i, _)) => i - 1,
            None => return self.error(code, idx0, self.text_len()),
        };

        if self.skip_continuation()? {
            Ok(())
        } else {
            self.error(code, idx0, idx1)
        }
    }

    fn get_next(&mut self, lookahead: (u32, char)) -> Result<Token, Error> {
        match lookahead {
            (idx0, '=') => {
                self.bump();

                if !self.skip_continuation()? {
                    Ok(self.token(TokenKind::Equals, idx0, idx0 + 1))
                } else {
                    match self.lookahead {
                        Some((idx1, '>')) => {
                            self.bump();
                            Ok(self.token(TokenKind::Arrow, idx0, idx1 + 1))
                        }
                        Some((idx1, '=')) => {
                            self.bump();
                            Ok(self.token(TokenKind::EqualsEquals, idx0, idx1 + 1))
                        }
                        _ => Ok(self.token(TokenKind::Equals, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, '+') => {
                self.bump();
                Ok(self.token(TokenKind::Plus, idx0, idx0 + 1))
            }
            (idx0, '-') => {
                self.bump();
                Ok(self.token(TokenKind::Minus, idx0, idx0 + 1))
            }
            (idx0, '*') => {
                self.bump();
                if !self.skip_continuation()? {
                    Ok(self.token(TokenKind::Star, idx0, idx0 + 1))
                } else {
                    match self.lookahead {
                        Some((idx1, '*')) => {
                            self.bump();
                            Ok(self.token(TokenKind::StarStar, idx0, idx1 + 1))
                        }
                        _ => Ok(self.token(TokenKind::Star, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, '/') => {
                self.bump();
                if !self.skip_continuation()? {
                    Ok(self.token(TokenKind::Slash, idx0, idx0 + 1))
                } else {
                    match self.lookahead {
                        Some((idx1, '/')) => {
                            self.bump();
                            Ok(self.token(TokenKind::SlashSlash, idx0, idx1 + 1))
                        }
                        Some((idx1, '=')) => {
                            self.bump();
                            Ok(self.token(TokenKind::SlashEquals, idx0, idx1 + 1))
                        }
                        _ => Ok(self.token(TokenKind::Slash, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, '%') => {
                self.bump();
                Ok(self.token(TokenKind::Percent, idx0, idx0 + 1))
            }
            (idx0, '(') => {
                self.bump();
                Ok(self.token(TokenKind::LeftParen, idx0, idx0 + 1))
            }
            (idx0, ')') => {
                self.bump();
                Ok(self.token(TokenKind::RightParen, idx0, idx0 + 1))
            }
            (idx0, '[') => {
                self.bump();
                Ok(self.token(TokenKind::LeftBracket, idx0, idx0 + 1))
            }
            (idx0, ']') => {
                self.bump();
                Ok(self.token(TokenKind::RightBracket, idx0, idx0 + 1))
            }
            (idx0, '<') => {
                self.bump();
                if !self.skip_continuation()? {
                    Ok(self.token(TokenKind::LeftAngle, idx0, idx0 + 1))
                } else {
                    match self.lookahead {
                        Some((idx1, '=')) => {
                            self.bump();
                            Ok(self.token(TokenKind::LeftAngleEquals, idx0, idx1 + 1))
                        }
                        _ => Ok(self.token(TokenKind::LeftAngle, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, '>') => {
                self.bump();
                if !self.skip_continuation()? {
                    Ok(self.token(TokenKind::RightAngle, idx0, idx0 + 1))
                } else {
                    match self.lookahead {
                        Some((idx1, '=')) => {
                            self.bump();
                            Ok(self.token(TokenKind::RightAngleEquals, idx0, idx1 + 1))
                        }
                        _ => Ok(self.token(TokenKind::RightAngle, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, '.') => {
                self.bump();
                if !self.skip_continuation()? {
                    Ok(self.token(TokenKind::Dot, idx0, idx0 + 1))
                } else {
                    match self.lookahead {
                        // if followed by a letter, then this must be an operator
                        Some((_, c)) if is_letter(c) => self.operator(idx0),
                        // If followed by a digit, then this must be an operator
                        Some((_, c)) if is_digit(c) => self.finish_real_literal_constant(idx0),
                        // else just return the dot token
                        _ => Ok(self.token(TokenKind::Dot, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, ',') => {
                self.bump();
                Ok(self.token(TokenKind::Comma, idx0, idx0 + 1))
            }
            (idx0, ':') => {
                self.bump();
                if !self.skip_continuation()? {
                    Ok(self.token(TokenKind::Colon, idx0, idx0 + 1))
                } else {
                    match self.lookahead {
                        Some((idx1, ':')) => {
                            self.bump();
                            Ok(self.token(TokenKind::ColonColon, idx0, idx1 + 1))
                        }
                        _ => Ok(self.token(TokenKind::Colon, idx0, idx0 + 1)),
                    }
                }
            }
            (idx0, ';') => {
                self.bump();
                Ok(self.token(TokenKind::SemiColon, idx0, idx0 + 1))
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
                self.commentary(idx0)
            }
            (idx0, c) if is_identifier_start(c) => {
                self.bump();
                self.identifierish(idx0)
            }
            (idx, _) => {
                self.bump();
                self.unrecognized_token(idx)
            }
        }
    }

    fn internal_next(&mut self) -> Option<Result<Token, Error>> {
        loop {
            return match self.lookahead {
                Some((_, '&')) => {
                    if let Err(e) = self.skip_continuation() {
                        return Some(Err(e));
                    }
                    continue;
                }
                Some((_, c)) if is_new_line_start(c) => self.consume_new_line(),
                Some((_, c)) if c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Some(l) => Some(self.get_next(l)),
                None => None,
            };
        }
    }

    fn take_until_terminate<F>(
        &mut self,
        idx0: u32,
        require_continue_context: bool,
        mut terminate: F,
    ) -> Result<u32, Error>
    where
        F: FnMut(Lookahead) -> TakeUntil,
    {
        let mut last_idx = idx0;
        loop {
            return match self.lookahead {
                Some((_, '&')) => {
                    self.bump();
                    let continue_context = self.continuation(idx0)?;
                    if require_continue_context && !continue_context {
                        self.error(
                            ErrorCode::DiscontinuedCharacterContext,
                            idx0,
                            self.lookahead.map_or(self.text_len(), |(i, _)| i),
                        )
                    } else {
                        continue;
                    }
                }
                None => match terminate(Lookahead::EOF) {
                    Continue => panic!("Cannot continue past EOF!"),
                    Stop => Ok(last_idx + 1),
                    Error(err_code) => self.error(err_code, idx0, self.text_len() - 1),
                },
                Some((idx1, c)) => match terminate(Lookahead::Character(c)) {
                    Continue => {
                        self.bump();
                        last_idx = idx1;
                        continue;
                    }
                    Stop => Ok(idx1),
                    Error(err_code) => self.error(err_code, idx0, idx1),
                },
            };
        }
    }

    fn take_until<F>(&mut self, idx0: u32, terminate: F) -> Result<u32, Error>
    where
        F: FnMut(Lookahead) -> TakeUntil,
    {
        self.take_until_terminate(idx0, false, terminate)
    }

    fn bump(&mut self) -> Option<(u32, char)> {
        self.lookahead = self.chars.next().map(|(i, c)| (i as u32, c));
        self.lookahead
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Result<Token, Error>> {
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
