use crate::parser::lex::{KeywordTokenKind, Token, TokenKind};
use crate::span::Span;

use super::Classifier;

impl<'input, 'arena> Classifier<'input, 'arena> {
    pub(super) fn check(&mut self, expected: TokenKind) -> bool {
        if let Some(k) = self.tokenizer.peek_kind() {
            self.expect_token(k, expected)
        } else {
            self.tokenizer.push_expected(expected);
            false
        }
    }

    pub(super) fn check_and_bump(&mut self, expected: TokenKind) -> Option<Token> {
        if self.check(expected) {
            self.tokenizer.bump()
        } else {
            None
        }
    }

    pub(super) fn check_name(&mut self) -> bool {
        if let Some(k) = self.tokenizer.peek_kind() {
            if k.is_name() {
                return true;
            }
        }

        self.tokenizer.expected_tokens.push(TokenKind::Name);
        false
    }

    pub(super) fn check_name_and_bump(&mut self) -> Option<Token> {
        if self.check_name() {
            self.tokenizer.bump()
        } else {
            None
        }
    }

    pub(super) fn check_eos(&mut self) -> bool {
        if let Some(t) = self.tokenizer.peek_kind() {
            if t.is_eos() {
                return true;
            }
        } else {
            return true;
        }

        self.tokenizer.push_expected(TokenKind::NewLine);
        self.tokenizer.push_expected(TokenKind::SemiColon);
        false
    }

    pub(super) fn check_eos_and_bump(&mut self) -> Option<Token> {
        if self.check_eos() {
            Some(if let Some(t) = self.tokenizer.bump() {
                t
            } else {
                // Return a "NewLine" token for end-of-file
                Token {
                    kind: TokenKind::NewLine,
                    span: Span {
                        file_id: self.file_id,
                        start: self.text_len(),
                        end: self.text_len(),
                    },
                }
            })
        } else {
            None
        }
    }

    pub(super) fn check_declaration_type_spec_start(&mut self) -> bool {
        self.check_intrinsic_type()
            || self.check(TokenKind::Keyword(KeywordTokenKind::Type))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Class))
    }

    pub(super) fn check_routine_attribute(&mut self) -> bool {
        self.check(TokenKind::Keyword(KeywordTokenKind::Elemental))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Impure))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Module))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Non_Recursive))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Pure))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Recursive))
    }

    pub(super) fn check_intrinsic_type(&mut self) -> bool {
        self.check(TokenKind::Keyword(KeywordTokenKind::Integer))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Real))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Double))
            || self.check(TokenKind::Keyword(KeywordTokenKind::DoublePrecision))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Complex))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Character))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Logical))
    }

    pub(super) fn check_maybe_routine_prefix(&mut self) -> bool {
        self.check_routine_attribute()
            || self.check_intrinsic_type()
            || self.check(TokenKind::Keyword(KeywordTokenKind::Type))
            || self.check(TokenKind::Keyword(KeywordTokenKind::Class))
    }
}
