use crate::parser::lex::{KeywordTokenKind, Token, TokenKind};
use crate::span::Span;

use super::statements::{Spanned, Stmt, StmtKind};
use super::{eos_or, Classifier};

impl<'input, 'arena> Classifier<'input, 'arena> {
    /// Parses a statement after consuming a `BLOCK DATA` token chain.
    pub(crate) fn stmt_from_block_data(&mut self, block_data_span: Span) -> Stmt<'arena> {
        let name = match self.peek() {
            Some(t) if t.is_name() => {
                let t = self.bump().unwrap();

                Some(Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                ))
            }
            Some(t) if Self::is_eos(&t) => None,
            None => None,
            _ => {
                self.emit_expected_token(&eos_or(&[TokenKind::Keyword(KeywordTokenKind::Name)]));

                self.take_until_eos();

                None
            }
        };

        self.expect_eos();

        Stmt {
            kind: StmtKind::BlockData { name },
            span: Span {
                file_id: self.file_id,
                start: block_data_span.start,
                end: name.map_or(block_data_span.end, |spanned| spanned.span.end),
            },
        }
    }

    /// Parses a statement after consuming a single `BLOCK` token. Could be a block-data-stmt or
    /// a block-stmt.
    pub(crate) fn stmt_from_block(&mut self, start_span: Span) -> Stmt<'arena> {
        match self.peek() {
            Some(t) if Self::is_eos(&t) => Stmt {
                kind: StmtKind::Block { name: None },
                span: start_span,
            },
            None => Stmt {
                kind: StmtKind::Block { name: None },
                span: start_span,
            },
            Some(Token {
                kind: TokenKind::Keyword(KeywordTokenKind::Data),
                ..
            }) => {
                let end = self.bump().unwrap().span.end;
                self.stmt_from_block_data(Span {
                    file_id: self.file_id,
                    start: start_span.start,
                    end,
                })
            }
            _ => {
                self.emit_expected_token(&eos_or(&[TokenKind::Keyword(KeywordTokenKind::Data)]));

                self.take_until_eos();

                Stmt {
                    kind: StmtKind::Block { name: None },
                    span: start_span,
                }
            }
        }
    }

    pub(crate) fn stmt_from_end_block_data(&mut self, end_block_data_span: Span) -> Stmt<'arena> {
        let name = match self.peek() {
            Some(t) if t.is_name() => {
                let t = self.bump().unwrap();

                Some(Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                ))
            }
            Some(t) if Self::is_eos(&t) => None,
            None => None,
            _ => {
                self.emit_expected_token(&eos_or(&[TokenKind::Name]));

                self.take_until_eos();

                None
            }
        };

        self.expect_eos();

        Stmt {
            kind: StmtKind::EndBlockData { name },
            span: name.map_or(end_block_data_span, |name| {
                end_block_data_span.concat(name.span)
            }),
        }
    }

    pub(crate) fn stmt_from_end_block(&mut self, end_block_span: Span) -> Stmt<'arena> {
        let name = match self.peek() {
            // If we see `Data`, we always classify as `EndBlockData`, even though it could actually
            // be an `EndBlock` with name "data". We'll resolve the ambiguity at the ast level
            Some(Token {
                kind: TokenKind::Keyword(KeywordTokenKind::Data),
                ..
            }) => {
                let span = self.bump().unwrap().span;
                return self.stmt_from_end_block_data(end_block_span.concat(span));
            }
            Some(t) if t.is_name() => {
                let t = self.bump().unwrap();

                Some(Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                ))
            }
            Some(t) if Self::is_eos(&t) => None,
            None => None,
            _ => {
                self.emit_expected_token(&eos_or(&[
                    TokenKind::Keyword(KeywordTokenKind::Data),
                    TokenKind::Name,
                ]));

                self.take_until_eos();

                None
            }
        };

        self.expect_eos();

        Stmt {
            kind: StmtKind::EndBlock { name },
            span: name.map_or(end_block_span, |name| end_block_span.concat(name.span)),
        }
    }
}
