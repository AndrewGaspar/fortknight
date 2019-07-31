use std::iter::Iterator;
use std::str::CharIndices;

use self::ErrorCode::*;
use self::TakeUntil::*;

use crate::data::FileData;
use crate::index::FileId;
use crate::intern::{InternedString, StringInterner};
use crate::span::Span;

#[cfg(test)]
mod tests;

mod token;

use token::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub location: Span,
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
            location: Span {
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
            *kind
        } else {
            TokenKind::Identifier
        };

        Ok(self.token(kind, idx0, idx1))
    }

    fn unrecognized_token(&mut self, idx0: u32) -> Result<Token, Error> {
        let idx1 = self.take_until(idx0, |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if c.is_whitespace() => Stop,
            _ => Continue,
        })?;

        self.error(UnrecognizedToken, idx0, idx1)
    }

    fn string_literal(&mut self, idx0: u32, quote: char) -> Result<Token, Error> {
        let mut escape = false;
        let idx1 = self.take_until(idx0, |lookahead: Lookahead| {
            if escape {
                escape = false;
                Continue
            } else {
                match lookahead {
                    Lookahead::Character('\\') => {
                        escape = true;
                        Continue
                    }
                    Lookahead::Character(c) if c == quote => Stop,
                    Lookahead::Character(c) if is_new_line_start(c) => {
                        Error(UnterminatedStringLiteral)
                    }
                    Lookahead::Character(_) => Continue,
                    Lookahead::EOF => Error(UnterminatedStringLiteral),
                }
            }
        })?;

        self.bump(); // consume the closing quote
        Ok(self.token(TokenKind::CharLiteralConstant, idx0, idx1 + 1))
    }

    fn digit_string(&mut self, idx0: u32) -> Result<Token, Error> {
        let idx1 = self.take_until(idx0, |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if is_digit(c) => Continue,
            _ => Stop,
        })?;

        Ok(self.token(TokenKind::DigitString, idx0, idx1 + 1))
    }

    // expected that last seen character was '!'
    // returns nothing - merely advances to end of comment.
    fn commentary(&mut self) {
        loop {
            match self.lookahead {
                Some((_, c)) if is_new_line_start(c) => return,
                None => return,
                Some(_) => {
                    self.bump();
                    continue;
                }
            }
        }
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

    fn continuation(&mut self, idx0: u32) -> Option<Result<(), Error>> {
        let mut first_line = true;

        loop {
            return match self.lookahead {
                Some((_, '!')) => {
                    self.bump();
                    self.commentary();
                    continue;
                }
                Some((_, c)) if is_new_line_start(c) => {
                    first_line = false;
                    match self.consume_new_line() {
                        // propagate errors
                        Some(Err(err)) => Some(Err(err)),
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
                Some((idx1, _)) if first_line => Some(self.error(UnexpectedToken, idx1, idx1 + 1)),
                // If an & is encountered, we're done processing the
                // continuation. The caller should continue whatever
                // tokenization process it was previously performing.
                Some((_, '&')) => {
                    self.bump();
                    Some(Ok(()))
                }
                // If a new token is encountered, then the continuation has
                // ended. The new character is a new token.
                Some(_) => None,
                None => Some(self.error(UnterminatedContinuationLine, idx0, self.text_len() - 1)),
            };
        }
    }

    // Can be called at any time during tokenization. Should be called at every
    // character when consuming a multi-character token.
    //
    // Returns None if continuation is skipped without issue. Return Some(err)
    // if there was an issue in the continuation.
    fn skip_continuation(&mut self) -> Option<Error> {
        if let Some((idx0, '&')) = self.lookahead {
            self.bump();
            if let Some(Err(err)) = self.continuation(idx0) {
                return Some(err);
            }
        }

        None
    }

    fn internal_next(&mut self) -> Option<Result<Token, Error>> {
        loop {
            return match self.lookahead {
                Some((idx0, '=')) => {
                    self.bump();

                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    match self.lookahead {
                        Some((idx1, '>')) => {
                            self.bump();
                            Some(Ok(self.token(TokenKind::Arrow, idx0, idx1 + 1)))
                        }
                        _ => Some(Ok(self.token(TokenKind::Equals, idx0, idx0 + 1))),
                    }
                }
                Some((idx0, '+')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::Plus, idx0, idx0 + 1)))
                }
                Some((idx0, '-')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::Minus, idx0, idx0 + 1)))
                }
                Some((idx0, '*')) => {
                    self.bump();

                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    match self.lookahead {
                        Some((idx1, '*')) => {
                            self.bump();
                            Some(Ok(self.token(TokenKind::StarStar, idx0, idx1 + 1)))
                        }
                        _ => Some(Ok(self.token(TokenKind::Star, idx0, idx0 + 1))),
                    }
                }
                Some((idx0, '/')) => {
                    self.bump();

                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    match self.lookahead {
                        Some((idx1, '/')) => {
                            self.bump();
                            Some(Ok(self.token(TokenKind::SlashSlash, idx0, idx1 + 1)))
                        }
                        _ => Some(Ok(self.token(TokenKind::Slash, idx0, idx0 + 1))),
                    }
                }
                Some((idx0, '%')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::Percent, idx0, idx0 + 1)))
                }
                Some((idx0, '(')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::LeftParen, idx0, idx0 + 1)))
                }
                Some((idx0, ')')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::RightParen, idx0, idx0 + 1)))
                }
                Some((idx0, '[')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::LeftBracket, idx0, idx0 + 1)))
                }
                Some((idx0, ']')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::RightBracket, idx0, idx0 + 1)))
                }
                Some((idx0, '.')) => {
                    self.bump();
                    Some(self.operator(idx0))
                }
                Some((idx0, ',')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::Comma, idx0, idx0 + 1)))
                }
                Some((_, c)) if is_new_line_start(c) => self.consume_new_line(),
                Some((idx0, ':')) => {
                    self.bump();

                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    match self.lookahead {
                        Some((idx1, ':')) => {
                            self.bump();
                            Some(Ok(self.token(TokenKind::ColonColon, idx0, idx1 + 1)))
                        }
                        _ => Some(Ok(self.token(TokenKind::Colon, idx0, idx0 + 1))),
                    }
                }
                Some((idx0, ';')) => {
                    self.bump();
                    Some(Ok(self.token(TokenKind::SemiColon, idx0, idx0 + 1)))
                }
                Some((_, '&')) => {
                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    continue;
                }
                Some((idx0, c)) if (c == '"' || c == '\'') => {
                    self.bump();
                    Some(self.string_literal(idx0, c))
                }
                Some((idx0, c)) if is_digit(c) => {
                    self.bump();
                    Some(self.digit_string(idx0))
                }
                Some((_, '!')) => {
                    self.bump();
                    self.commentary();
                    continue;
                }
                Some((idx0, c)) if is_identifier_start(c) => {
                    self.bump();
                    Some(self.identifierish(idx0))
                }
                Some((_, c)) if c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Some((idx, _)) => {
                    self.bump();
                    Some(self.unrecognized_token(idx))
                }
                None => None,
            };
        }
    }

    fn take_until<F>(&mut self, idx0: u32, mut terminate: F) -> Result<u32, Error>
    where
        F: FnMut(Lookahead) -> TakeUntil,
    {
        let mut last_idx = idx0;
        loop {
            return match self.lookahead {
                Some((_, '&')) => {
                    if let Some(err) = self.skip_continuation() {
                        return Err(err);
                    }

                    continue;
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
