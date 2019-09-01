use crate::intern::InternedName;
use crate::parser::lex::{KeywordTokenKind, Token, TokenKind};
use crate::span::Span;

use super::statements::{ImportStmt, Spanned, Stmt, StmtKind};
use super::{eos_or, Classifier};

impl<'input, 'arena> Classifier<'input, 'arena> {
    pub(super) fn import_statement(&mut self, start_span: Span) -> Stmt<'arena> {
        match self.peek() {
            Some(Token {
                kind: TokenKind::Comma,
                ..
            }) => {
                self.bump();

                match self.peek() {
                    Some(Token {
                        kind: TokenKind::Keyword(KeywordTokenKind::All),
                        ..
                    }) => {
                        let end = self.bump().unwrap().span.end;

                        Stmt {
                            kind: StmtKind::Import(ImportStmt::AllSpecifier),
                            span: Span {
                                file_id: self.file_id,
                                start: start_span.start,
                                end,
                            },
                        }
                    }
                    Some(Token {
                        kind: TokenKind::Keyword(KeywordTokenKind::None),
                        ..
                    }) => {
                        let end = self.bump().unwrap().span.end;

                        Stmt {
                            kind: StmtKind::Import(ImportStmt::NoneSpecifier),
                            span: Span {
                                file_id: self.file_id,
                                start: start_span.start,
                                end,
                            },
                        }
                    }
                    Some(Token {
                        kind: TokenKind::Keyword(KeywordTokenKind::Only),
                        ..
                    }) => {
                        let span = self.bump().unwrap().span;

                        self.import_only_statement(start_span, span)
                    }
                    _ => {
                        self.emit_expected_token(&eos_or(&[
                            TokenKind::Keyword(KeywordTokenKind::All),
                            TokenKind::Keyword(KeywordTokenKind::None),
                            TokenKind::Keyword(KeywordTokenKind::Only),
                        ]));

                        // advance to end of statement
                        self.take_until_eos();

                        // Treat like an unqualified import statement
                        Stmt {
                            kind: StmtKind::Import(ImportStmt::NoSpecifier(&[])),
                            span: Span {
                                file_id: self.file_id,
                                start: start_span.start,
                                end: start_span.end,
                            },
                        }
                    }
                }
            }
            Some(Token {
                kind: TokenKind::ColonColon,
                ..
            }) => {
                self.bump();
                self.import_unspecified_statement(start_span)
            }
            Some(t) if t.is_name() => self.import_unspecified_statement(start_span),
            Some(t) if Self::is_eos(&t) => {
                self.expect_eos();

                Stmt {
                    kind: StmtKind::Import(ImportStmt::NoSpecifier(&[])),
                    span: Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end: start_span.end,
                    },
                }
            }
            None => {
                self.expect_eos();

                Stmt {
                    kind: StmtKind::Import(ImportStmt::NoSpecifier(&[])),
                    span: Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end: start_span.end,
                    },
                }
            }
            _ => {
                self.emit_expected_token(&eos_or(&[
                    TokenKind::Comma,
                    TokenKind::ColonColon,
                    TokenKind::Name,
                ]));

                // advance to end of statement
                self.take_until_eos();

                Stmt {
                    kind: StmtKind::Import(ImportStmt::NoSpecifier(&[])),
                    span: Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end: start_span.end,
                    },
                }
            }
        }
    }

    /// Parses the rest of an import statement with no specifiers, starting from the first name
    fn import_unspecified_statement(&mut self, start_span: Span) -> Stmt<'arena> {
        let imports = self.import_name_list();

        Stmt {
            kind: StmtKind::Import(ImportStmt::NoSpecifier(imports)),
            span: Span {
                file_id: self.file_id,
                start: start_span.start,
                end: imports.last().map_or(start_span.end, |imp| imp.span.end),
            },
        }
    }

    /// Parses the rest of an import, only statement, starting from colon
    fn import_only_statement(&mut self, start_span: Span, only_end_span: Span) -> Stmt<'arena> {
        match self.peek() {
            Some(Token {
                kind: TokenKind::Colon,
                ..
            }) => {
                self.bump();
            }
            _ => {
                self.emit_expected_token(&[TokenKind::Colon]);

                self.take_until_eos();

                return Stmt {
                    kind: StmtKind::Import(ImportStmt::OnlySpecifier(&[])),
                    span: Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end: only_end_span.end,
                    },
                };
            }
        };

        let imports = self.import_name_list();

        Stmt {
            kind: StmtKind::Import(ImportStmt::OnlySpecifier(imports)),
            span: Span {
                file_id: self.file_id,
                start: start_span.start,
                end: imports.last().map_or(only_end_span.end, |imp| imp.span.end),
            },
        }
    }

    /// Parses an import-name-list
    fn import_name_list(&mut self) -> &'arena [Spanned<InternedName>] {
        let first_name = loop {
            match self.peek() {
                Some(t) if t.is_name() => {
                    let t = *t;
                    break Spanned::new(
                        t.try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                        t.span,
                    );
                }
                Some(t) if Self::is_eos(&t) => {
                    self.emit_expected_token(&[TokenKind::Name]);

                    // consume EOS and stop parsing
                    self.bump();
                    return &[];
                }
                _ => {
                    self.emit_expected_token(&[TokenKind::Name]);

                    // consume token and try again
                    self.bump();
                }
            }
        };

        self.arena
            .names
            .alloc_extend(std::iter::once(first_name).chain(std::iter::from_fn(|| {
                // This loop is for handling cases where we reach an unrecognized token but would
                // like to continue parsing and emitting meaningful errors for as long as possible
                loop {
                    match self.peek()? {
                        Token {
                            kind: TokenKind::Comma,
                            ..
                        } => {
                            // Consume comma, move on to name
                            self.bump();
                        }
                        // Reached EOS, return None to indicate end of list
                        t if Self::is_eos(&t) => {
                            self.bump();
                            return None;
                        }
                        _ => {
                            // unexpected token - advance until we consume a comma or exit at EOS

                            self.emit_expected_token(&eos_or(&[TokenKind::Comma]));

                            match self.skip_to_comma_or_eos() {
                                Some(()) => {}
                                _ => return None,
                            }
                        }
                    };

                    match self.peek() {
                        // Found name as expected - return it
                        Some(t) if t.is_name() => {
                            let t = self.bump().unwrap();
                            return Some(Spanned::new(
                                t.try_intern_contents(&mut self.interner, &self.text)
                                    .unwrap(),
                                t.span,
                            ));
                        }
                        // Unexpected token - get to a comma and let's try parsing the next name, or
                        // stop parsing if we reach EOS.
                        _ => {
                            self.emit_expected_token(&[TokenKind::Name]);

                            match self.skip_to_comma_or_eos() {
                                // Try parsing the next name
                                Some(()) => continue,
                                // End of statement - stop parsing statement
                                _ => return None,
                            }
                        }
                    }
                }
            })))
    }
}
