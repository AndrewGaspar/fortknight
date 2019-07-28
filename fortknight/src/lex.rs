use std::convert::TryInto;
use std::iter::Iterator;
use std::slice;
use std::str::CharIndices;

use self::ErrorCode::*;
use self::TakeUntil::*;

use crate::data::FileData;
use crate::index::FileId;
use crate::intern::{InternedString, StringInterner};
use crate::span::Span;

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub location: Span,
    pub code: ErrorCode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    UnrecognizedToken,
    UnterminatedStringLiteral,
    UnterminatedOperator,
    UnterminatedContinuationLine,
    InvalidCarriageReturn,
    UnexpectedToken,
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
        UserStrIterator::new(self.string.as_bytes().iter())
    }
}

impl<'input> PartialEq for UserStr<'input> {
    fn eq(&self, other: &UserStr) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<'input> Eq for UserStr<'input> {}

#[derive(Clone, Debug)]
pub struct UserStrIterator<'input> {
    str_iter: slice::Iter<'input, u8>,
}

impl<'input> UserStrIterator<'input> {
    fn new(str_iter: slice::Iter<'input, u8>) -> UserStrIterator<'input> {
        UserStrIterator { str_iter: str_iter }
    }
}

// Iterator over a FortranUserStr. Ignores continuation. This allows us to
// tokenize the FORTRAN program without allocating any memory.
impl<'input> Iterator for UserStrIterator<'input> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        // if we're here, we can assume that the string is already a
        // valid identifier, which means the continuation is properly
        // terminated. Just continue until we see a closing ampersand.
        loop {
            return match self.str_iter.next() {
                Some(amp) if *amp == b'&' => {
                    while b'&' != *self.str_iter.next().unwrap() {}
                    continue;
                }
                Some(x) => Some(*x),
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

    pub fn iter(&self) -> impl Iterator<Item = u8> + 'input {
        self.user_str.iter().map(|c: u8| c.to_ascii_lowercase())
    }
}

impl<'input> PartialEq for CaseInsensitiveUserStr<'input> {
    fn eq(&self, other: &CaseInsensitiveUserStr) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<'input> Eq for CaseInsensitiveUserStr<'input> {}

#[derive(Copy, Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn try_into_identifierish(self) -> Option<IdentifierishToken> {
        if self.kind == TokenKind::Id || KEYWORDS.iter().find(|k| k.1 == self.kind).is_some() {
            Some(IdentifierishToken(self))
        } else {
            None
        }
    }

    pub fn try_into_operator(self) -> Option<OperatorToken> {
        if self.kind == TokenKind::DefinedOperator
            || INTRINSIC_OPERATORS
                .iter()
                .find(|k| k.1 == self.kind)
                .is_some()
        {
            Some(OperatorToken(self))
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct IdentifierishToken(Token);

impl IdentifierishToken {
    pub fn intern(&self, file_data: &FileData, interner: &mut StringInterner) -> InternedString {
        let user_str = UserStr::new(file_data.read_span(&self.0.span));
        interner.intern_string(String::from_utf8(user_str.iter().collect()).unwrap())
    }

    pub fn intern_case_insensitive(
        &self,
        file_data: &FileData,
        interner: &mut StringInterner,
    ) -> InternedString {
        let user_str = CaseInsensitiveUserStr::new(file_data.read_span(&self.0.span));
        interner.intern_string(String::from_utf8(user_str.iter().collect()).unwrap())
    }
}

#[derive(Copy, Clone, Debug)]
pub struct OperatorToken(Token);

impl OperatorToken {
    fn get_name_str<'a>(&self, file_data: &'a FileData) -> &'a str {
        let text = file_data.read_span(&self.0.span);
        debug_assert_eq!(".", &text[0..1]);
        debug_assert_eq!(".", &text[text.len() - 1..]);

        let mut name_span = self.0.span;
        name_span.start += 1;
        name_span.end -= 1;

        file_data.read_span(&name_span)
    }

    pub fn intern(&self, file_data: &FileData, interner: &mut StringInterner) -> InternedString {
        let user_str = UserStr::new(self.get_name_str(&file_data));
        interner.intern_string(String::from_utf8(user_str.iter().collect()).unwrap())
    }

    pub fn intern_case_insensitive(
        &self,
        file_data: &FileData,
        interner: &mut StringInterner,
    ) -> InternedString {
        let user_str = CaseInsensitiveUserStr::new(self.get_name_str(&file_data));
        interner.intern_string(String::from_utf8(user_str.iter().collect()).unwrap())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // statements
    Program,
    Module,
    End,
    Contains,
    Function,
    Subroutine,
    Submodule,
    Implicit,
    None,

    // Actions
    Print,

    // user strings
    Id,
    IntegerLiteralConstant,
    CharLiteralConstant,
    DigitString,
    DefinedOperator,

    // Symbols
    And,
    Equivalent,
    NotEquivalent,
    Not,
    Or,
    EqualsOp,
    NotEqualsOp,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
    True,
    False,

    Arrow,
    Equals,
    Plus,
    Minus,
    Slash,
    SlashSlash,
    Star,
    StarStar,
    Colon,
    ColonColon,

    // structure
    EOS,
    Comma,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,

    // Types
    Real,
    Double,
    Precision,
    Complex,
    Character,
    Logical,
    Integer,
    Percent,
    Kind,

    // Attributes
    Allocatable,
    Asynchronous,
    Codimension,
    Contiguous,
    Dimension,
    External,
    Intent,
    Intrinsic,
    Optional,
    Parameter,
    Pointer,
    Protected,
    Save,
    Target,
    Value,
    Volatile,

    // Access
    Public,
    Private,

    // LanguageBinding
    Bind,
    C,
    Name,

    // Intent
    In,
    Out,
    Inout,

    // modules
    Use,
    NonIntrinsic,
    Only,
    Operator,
}

pub struct Tokenizer<'input> {
    file_id: FileId,
    text: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(u32, char)>,
    is_start: bool,
    is_end: bool,
    last_token: Option<Result<Token, Error>>,
}

const KEYWORDS: &'static [(&'static str, TokenKind)] = {
    use self::TokenKind::*;

    &[
        ("ALLOCATABLE", Allocatable),
        ("ASYNCHRONOUS", Asynchronous),
        ("BIND", Bind),
        ("C", C),
        ("CHARACTER", Character),
        ("CODIMENSION", Codimension),
        ("COMPLEX", Complex),
        ("CONTAINS", Contains),
        ("CONTIGUOUS", Contiguous),
        ("DIMENSION", Dimension),
        ("DOUBLE", Double),
        ("END", End),
        ("EXTERNAL", External),
        ("FUNCTION", Function),
        ("IN", In),
        ("INOUT", Inout),
        ("INTEGER", Integer),
        ("INTENT", Intent),
        ("INTRINSIC", Intrinsic),
        ("IMPLICIT", Implicit),
        ("KIND", Kind),
        ("LOGICAL", Logical),
        ("MODULE", Module),
        ("NAME", Name),
        ("NON_INTRINSIC", NonIntrinsic),
        ("NONE", None),
        ("ONLY", Only),
        ("OPERATOR", Operator),
        ("OPTIONAL", Optional),
        ("OUT", Out),
        ("PARAMETER", Parameter),
        ("POINTER", Pointer),
        ("PRECISION", Precision),
        ("PRINT", Print),
        ("PRIVATE", Private),
        ("PROGRAM", Program),
        ("PROTECTED", Protected),
        ("PUBLIC", Public),
        ("REAL", Real),
        ("SAVE", Save),
        ("SUBROUTINE", Subroutine),
        ("SUBMODULE", Submodule),
        ("TARGET", Target),
        ("USE", Use),
        ("VALUE", Value),
        ("VOLATILE", Volatile),
    ]
};

const INTRINSIC_OPERATORS: &'static [(&'static str, TokenKind)] = {
    use self::TokenKind::*;

    &[
        ("AND", And),
        ("EQ", EqualsOp),
        ("EQV", Equivalent),
        ("FALSE", False),
        ("GE", GreaterThanOrEquals),
        ("GT", GreaterThan),
        ("LE", LessThanOrEquals),
        ("LT", LessThan),
        ("NE", NotEqualsOp),
        ("NEQV", NotEquivalent),
        ("NOT", Not),
        ("OR", Or),
        ("TRUE", True),
    ]
};

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
            is_start: true,
            is_end: false,
            last_token: None,
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
        let terminate = |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if is_operator_continue(c) => Continue,
            Lookahead::Character('.') => Stop,
            _ => Error(UnterminatedOperator),
        };

        match self.take_until(idx0, terminate) {
            Some(Ok(idx1)) => {
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
            Some(Err(err)) => Err(err),
            None => self.error(UnterminatedOperator, idx0, self.text_len() - 1),
        }
    }

    fn identifierish(&mut self, idx0: u32) -> Result<Token, Error> {
        let terminate = |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if is_identifier_continue(c) => Continue,
            _ => Stop,
        };

        match self.take_until(idx0, terminate) {
            Some(Ok(idx1)) => {
                let word = CaseInsensitiveUserStr::new(&self.text_span(idx0, idx1));

                let kind = KEYWORDS
                    .iter()
                    .filter(|&&(w, _)| CaseInsensitiveUserStr::new(w) == word)
                    .map(|&(_, ref t)| t.clone())
                    .next()
                    .unwrap_or_else(|| TokenKind::Id);

                Ok(self.token(kind, idx0, idx1))
            }
            Some(Err(err)) => Err(err),
            None => self.error(UnrecognizedToken, idx0, self.text_len() - 1),
        }
    }

    fn string_literal(&mut self, idx0: u32, quote: char) -> Result<Token, Error> {
        let mut escape = false;
        let terminate = |lookahead: Lookahead| {
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
        };

        match self.take_until(idx0, terminate) {
            Some(Ok(idx1)) => {
                self.bump(); // consume the closing quote
                Ok(self.token(TokenKind::CharLiteralConstant, idx0, idx1 + 1))
            }
            Some(Err(err)) => Err(err),
            None => self.error(UnterminatedStringLiteral, idx0, self.text_len() - 1),
        }
    }

    fn digit_string(&mut self, idx0: u32) -> Result<Token, Error> {
        let terminate = |lookahead: Lookahead| match lookahead {
            Lookahead::Character(c) if is_digit(c) => Continue,
            _ => Stop,
        };

        match self.take_until(idx0, terminate) {
            Some(Ok(idx1)) => Ok(self.token(TokenKind::DigitString, idx0, idx1 + 1)),
            Some(Err(err)) => Err(err),
            None => self.error(UnterminatedStringLiteral, idx0, self.text_len() - 1),
        }
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
                    Some(Ok(self.token(TokenKind::EOS, idx0, idx0 + 1)))
                }
                Some((idx0, '\r')) => {
                    match self.bump() {
                        Some((_, '\n')) => {
                            self.bump();
                            Some(Ok(self.token(TokenKind::EOS, idx0, idx0 + 2)))
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
                    Some(Ok(self.token(TokenKind::EOS, idx0, idx0 + 1)))
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
                // TODO: Read until next whitespace, discard whole token, register error, and continue
                Some((idx, _)) => Some(self.error(UnrecognizedToken, idx, self.text_len() - 1)),
                None => None,
            };
        }
    }

    fn take_until<F>(&mut self, idx0: u32, mut terminate: F) -> Option<Result<u32, Error>>
    where
        F: FnMut(Lookahead) -> TakeUntil,
    {
        let mut last_idx = idx0;
        loop {
            return match self.lookahead {
                Some((_, '&')) => {
                    if let Some(err) = self.skip_continuation() {
                        return Some(Err(err));
                    }

                    continue;
                }
                None => match terminate(Lookahead::EOF) {
                    Continue => panic!("Cannot continue past EOF!"),
                    Stop => Some(Ok(last_idx + 1)),
                    Error(err_code) => Some(self.error(err_code, idx0, self.text_len() - 1)),
                },
                Some((idx1, c)) => match terminate(Lookahead::Character(c)) {
                    Continue => {
                        self.bump();
                        last_idx = idx1;
                        continue;
                    }
                    Stop => Some(Ok(idx1)),
                    Error(err_code) => Some(self.error(err_code, idx0, idx1)),
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
        loop {
            if self.is_end {
                return None;
            }

            let mut next_token = self.internal_next();

            // reached EOF - change to is_end state and return an EOS.
            if next_token.is_none() {
                self.is_end = true;
                next_token = Some(Ok(self.token(
                    TokenKind::EOS,
                    self.text_len(),
                    self.text_len(),
                )));
            }

            if let Some(Ok(Token {
                kind: TokenKind::EOS,
                ..
            })) = next_token
            {
                if self.is_start {
                    continue;
                }

                if let Some(Ok(Token {
                    kind: TokenKind::EOS,
                    ..
                })) = self.last_token
                {
                    continue;
                }
            }

            self.is_start = false;
            self.last_token = next_token.clone();
            return next_token;
        }
    }
}

// assumed that starting . has been consumed
// the final character will need to be validated as a . and consumed
fn is_operator_continue(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

fn is_identifier_start(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

fn is_identifier_continue(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_')
}

fn is_new_line_start(c: char) -> bool {
    c == '\r' || c == '\n'
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}
