use crate::intern::InternedName;
use crate::parser::lex::{KeywordTokenKind, Token, TokenKind};
use crate::span::Span;

use super::statements::{ImportStmt, Spanned, Stmt, StmtKind};
use super::{eos_or, Classifier};

impl<'input, 'arena> Classifier<'input, 'arena> {
    pub(super) fn import_statement(&mut self, start_span: Span) -> Stmt<'arena> {
        if self.check(TokenKind::Comma) {
            self.tokenizer.bump();

            if self.check(TokenKind::Keyword(KeywordTokenKind::All)) {
                let end = self.tokenizer.bump().unwrap().span.end;

                Stmt {
                    kind: StmtKind::Import(ImportStmt::AllSpecifier),
                    span: Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end,
                    },
                }
            } else if self.check(TokenKind::Keyword(KeywordTokenKind::None)) {
                let end = self.tokenizer.bump().unwrap().span.end;

                Stmt {
                    kind: StmtKind::Import(ImportStmt::NoneSpecifier),
                    span: Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end,
                    },
                }
            } else if self.check(TokenKind::Keyword(KeywordTokenKind::Only)) {
                let span = self.tokenizer.bump().unwrap().span;

                self.import_only_statement(start_span, span)
            } else {
                self.emit_unexpected_token();

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
        } else if self.check(TokenKind::ColonColon) {
            self.tokenizer.bump();
            self.import_unspecified_statement(start_span)
        } else if self.check_name() {
            self.import_unspecified_statement(start_span)
        } else if self.check_eos() {
            self.expect_eos();

            Stmt {
                kind: StmtKind::Import(ImportStmt::NoSpecifier(&[])),
                span: Span {
                    file_id: self.file_id,
                    start: start_span.start,
                    end: start_span.end,
                },
            }
        } else {
            self.emit_unexpected_token();
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
        if self.check(TokenKind::Colon) {
            self.tokenizer.bump();
        } else {
            self.emit_unexpected_token();
            self.take_until_eos();

            return Stmt {
                kind: StmtKind::Import(ImportStmt::OnlySpecifier(&[])),
                span: Span {
                    file_id: self.file_id,
                    start: start_span.start,
                    end: only_end_span.end,
                },
            };
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
            if self.check_name() {
                let t = self.tokenizer.bump().unwrap();
                break Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                );
            } else if self.check_eos() {
                self.emit_unexpected_token();

                // consume EOS and stop parsing
                self.tokenizer.bump();
                return &[];
            } else {
                self.emit_unexpected_token();

                // consume token and try again
                self.tokenizer.bump();
            }
        };

        self.arena
            .names
            .alloc_extend(std::iter::once(first_name).chain(std::iter::from_fn(|| {
                // This loop is for handling cases where we reach an unrecognized token but would
                // like to continue parsing and emitting meaningful errors for as long as possible
                loop {
                    if self.check(TokenKind::Comma) {
                        // Consume comma, move on to name
                        self.tokenizer.bump();
                    }
                    // Reached EOS, return None to indicate end of list
                    else if self.check_eos() {
                        self.tokenizer.bump();
                        return None;
                    } else {
                        // unexpected token - advance until we consume a comma or exit at EOS
                        self.emit_unexpected_token();

                        match self.skip_to_comma_or_eos() {
                            Some(()) => {}
                            _ => return None,
                        }
                    };

                    if self.check_name() {
                        // Found name as expected - return it
                        let t = self.tokenizer.bump().unwrap();
                        return Some(Spanned::new(
                            t.try_intern_contents(&mut self.interner, &self.text)
                                .unwrap(),
                            t.span,
                        ));
                    } else {
                        // Unexpected token - get to a comma and let's try parsing the next name, or
                        // stop parsing if we reach EOS.
                        self.emit_unexpected_token();

                        match self.skip_to_comma_or_eos() {
                            // Try parsing the next name
                            Some(()) => continue,
                            // End of statement - stop parsing statement
                            _ => return None,
                        }
                    }
                }
            })))
    }
}
