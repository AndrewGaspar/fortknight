use num_traits::FromPrimitive;

use crate::error::SemanticErrorCode;
use crate::parser::lex::{KeywordTokenKind, Letter, TokenKind};
use crate::span::Span;

use super::statements::{
    DeclarationTypeSpec, ImplicitSpec, ImplicitStmt, IntegerTypeSpec, IntrinsicTypeSpec,
    LetterSpec, Spanned, Stmt, StmtKind,
};
use super::Classifier;

impl<'input, 'arena> Classifier<'input, 'arena> {
    /// Parses an implicit-stmt after consuming the `IMPLICIT` token
    pub(super) fn implicit_stmt(&mut self, implicit_span: Span) -> Stmt<'arena> {
        if self.check(TokenKind::Keyword(KeywordTokenKind::None)) {
            let none_span = self.tokenizer.bump().unwrap().span;
            self.implicit_none_stmt(implicit_span.concat(none_span))
        } else if self.check_declaration_type_spec_start() {
            self.implicit_type_stmt(implicit_span)
        } else {
            self.unexpected_token(&implicit_span)
        }
    }

    /// R863: implicit-stmt, when not NONE
    ///
    /// Parses an implicit-stmt after consuming the `IMPLICIT` token
    fn implicit_type_stmt(&mut self, implicit_span: Span) -> Stmt<'arena> {
        let spec = match self.implicit_spec() {
            Some(spec) => spec,
            None => {
                // error condition, but return empty spec_list
                self.expect_eos();
                return Stmt {
                    kind: StmtKind::Implicit(ImplicitStmt::SpecList(&[])),
                    span: implicit_span,
                };
            }
        };

        let mut error_encountered = false;

        let specs = self
            .arena
            .implicit_specs
            .alloc_extend(std::iter::once(spec).chain(std::iter::from_fn(|| {
                if self.check(TokenKind::Comma) {
                    self.tokenizer.bump();
                } else if self.check_eos() {
                    return None;
                } else {
                    error_encountered = true;
                    self.emit_unexpected_token();
                    return None;
                };

                match self.implicit_spec() {
                    Some(spec) => Some(spec),
                    None => {
                        error_encountered = true;
                        return None;
                    }
                }
            })));

        if error_encountered {
            self.take_until_eos();
        } else {
            self.expect_eos();
        }

        Stmt {
            kind: StmtKind::Implicit(ImplicitStmt::SpecList(specs)),
            span: specs.last().map_or(implicit_span, |spec| spec.span),
        }
    }

    /// R863: implicit-stmt, when NONE
    ///
    /// Parses an implicit-stmt after consuming `IMPLICIT NONE`
    fn implicit_none_stmt(&mut self, implicit_none_span: Span) -> Stmt<'arena> {
        let has_spec_list = if self.check(TokenKind::LeftParen) {
            self.tokenizer.bump();
            true
        } else if self.check_eos() {
            false
        } else {
            self.emit_unexpected_token();
            self.take_until_eos();

            // act as if it's a bare `implicit none`
            false
        };

        if !has_spec_list {
            return Stmt {
                kind: StmtKind::Implicit(ImplicitStmt::NoneSpecList {
                    has_external: false,
                    has_type: false,
                }),
                span: implicit_none_span,
            };
        }

        let mut has_external = false;
        let mut has_type = false;

        let mut is_first = true;
        let end_span = loop {
            if self.check(TokenKind::Keyword(KeywordTokenKind::External)) {
                let span = self.tokenizer.bump().unwrap().span;

                if has_external {
                    self.emit_semantic_error(
                        SemanticErrorCode::ImplicitNoneDuplicateSpec,
                        span.start,
                        span.end,
                        "The EXTERNAL spec was specified multiple times. IMPLICIT NONE specs \
                            may only be specified once.",
                    );
                }

                has_external = true;
            } else if self.check(TokenKind::Keyword(KeywordTokenKind::Type)) {
                let span = self.tokenizer.bump().unwrap().span;

                if has_type {
                    self.emit_semantic_error(
                        SemanticErrorCode::ImplicitNoneDuplicateSpec,
                        span.start,
                        span.end,
                        "The TYPE spec was specified multiple times. IMPLICIT NONE specs \
                            may only be specified once.",
                    );
                }

                has_type = true;
            } else if is_first && self.check(TokenKind::RightParen) {
                break self.tokenizer.bump().unwrap().span;
            } else {
                self.emit_unexpected_token();

                break self.take_until_eos().unwrap_or(Span {
                    file_id: self.file_id,
                    start: self.text_len(),
                    end: self.text_len(),
                });
            };

            is_first = false;

            if self.check(TokenKind::Comma) {
                self.tokenizer.bump();
            } else if self.check(TokenKind::RightParen) {
                break self.tokenizer.bump().unwrap().span;
            } else {
                self.emit_unexpected_token();

                break self.take_until_eos().unwrap_or(Span {
                    file_id: self.file_id,
                    start: self.text_len(),
                    end: self.text_len(),
                });
            };
        };

        self.expect_eos();

        Stmt {
            kind: StmtKind::Implicit(ImplicitStmt::NoneSpecList {
                has_external,
                has_type,
            }),
            span: implicit_none_span.concat(end_span),
        }
    }

    /// R864: implicit-spec
    ///
    /// Parses an implicit-spec from its start
    fn implicit_spec(&mut self) -> Option<Spanned<ImplicitSpec<'arena>>> {
        let declaration_type_spec = if self.implicit_spec_type_lookahead_should_skip_kind_selector()
        {
            let intrinsic = if self.check(TokenKind::Keyword(KeywordTokenKind::Integer)) {
                IntrinsicTypeSpec::Integer(IntegerTypeSpec(None))
            } else if self.check(TokenKind::Keyword(KeywordTokenKind::Real)) {
                IntrinsicTypeSpec::Real(None)
            } else if self.check(TokenKind::Keyword(KeywordTokenKind::Complex)) {
                IntrinsicTypeSpec::Complex(None)
            } else if self.check(TokenKind::Keyword(KeywordTokenKind::Character)) {
                IntrinsicTypeSpec::Character(None)
            } else if self.check(TokenKind::Keyword(KeywordTokenKind::Logical)) {
                IntrinsicTypeSpec::Logical(None)
            } else {
                panic!("Internal compiler error: Expected one of the intrinsic type specs")
            };

            let t = self.tokenizer.bump().unwrap();
            Spanned::new(DeclarationTypeSpec::Intrinsic(intrinsic), t.span)
        } else {
            self.declaration_type_spec()?
        };

        if self.check(TokenKind::LeftParen) {
            self.tokenizer.bump();
        } else {
            self.emit_unexpected_token();
            return None;
        }

        let letter_spec = self.letter_spec()?.val;

        let letter_spec_list =
            self.arena
                .letter_specs
                .alloc_extend(std::iter::once(letter_spec).chain(std::iter::from_fn(|| {
                    if self.check(TokenKind::Comma) {
                        self.tokenizer.bump();
                    } else if self.check(TokenKind::RightParen) {
                        return None;
                    } else {
                        self.emit_unexpected_token();
                        return None;
                    }

                    Some(self.letter_spec()?.val)
                })));

        let end_span = if self.check(TokenKind::RightParen) {
            self.tokenizer.bump().unwrap().span
        } else {
            // expected token already emitted - return
            return None;
        };

        Some(Spanned::new(
            ImplicitSpec {
                declaration_type_spec: declaration_type_spec.val,
                letter_spec_list,
            },
            declaration_type_spec.span.concat(end_span),
        ))
    }

    fn implicit_spec_type_lookahead_should_skip_kind_selector(&mut self) -> bool {
        match self.tokenizer.peek_nth_kind(0) {
            Some(t)
                if t.is_intrinsic_type_spec_start()
                    && t != TokenKind::Keyword(KeywordTokenKind::Double)
                    && t != TokenKind::Keyword(KeywordTokenKind::DoublePrecision) => {}
            _ => return false,
        };

        if Some(TokenKind::LeftParen) != self.tokenizer.peek_nth_kind(1) {
            // If the next token is next a left paren, this parse will fail anyway, let it parse.
            return false;
        }
        match self.idx_past_parentheticals_in_statement(1) {
            Some(next_idx) => {
                // If the following idx is the start of another parenthetical, then the first
                // parenthetical must be a kind-selector
                Some(TokenKind::LeftParen) != self.tokenizer.peek_nth_kind(next_idx)
            }
            None => true,
        }
    }

    /// R865: letter-spec
    ///
    /// Parses a letter-spec from its start
    fn letter_spec(&mut self) -> Option<Spanned<LetterSpec>> {
        let (letter, start_span) = match self.tokenizer.peek_kind() {
            Some(TokenKind::Letter(l)) => (l, self.tokenizer.bump().unwrap().span),
            _ => {
                self.emit_expected_token(
                    &(0..26)
                        .map(|i| TokenKind::Letter(Letter::from_u8(i).unwrap()))
                        .collect::<Vec<_>>(),
                );
                return None;
            }
        };

        if self.check(TokenKind::Minus) {
            self.tokenizer.bump();
        } else {
            return Some(Spanned::new(
                LetterSpec {
                    start: letter,
                    end: None,
                },
                start_span,
            ));
        }

        let (end_letter, end_span) = match self.tokenizer.peek_kind() {
            Some(TokenKind::Letter(l)) => (l, self.tokenizer.bump().unwrap().span),
            _ => {
                self.emit_expected_token(
                    &(0..26)
                        .map(|i| TokenKind::Letter(Letter::from_u8(i).unwrap()))
                        .collect::<Vec<_>>(),
                );
                return Some(Spanned::new(
                    LetterSpec {
                        start: letter,
                        end: None,
                    },
                    start_span,
                ));
            }
        };

        Some(Spanned::new(
            LetterSpec {
                start: letter,
                end: Some(end_letter),
            },
            start_span.concat(end_span),
        ))
    }
}
