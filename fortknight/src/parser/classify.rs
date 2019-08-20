use std::cell::RefCell;
use std::fmt::Write;
use std::iter::Peekable;

use crate::error::{AnalysisErrorKind, DiagnosticSink, ParserErrorCode};
use crate::index::FileId;
use crate::intern::InternedName;
use crate::parser::lex::{KeywordTokenKind, Token, TokenKind, Tokenizer, TokenizerOptions};
use crate::span::Span;

mod statements;

#[cfg(test)]
mod tests;

use statements::{ParentIdentifier, Spanned};
pub use statements::{Stmt, StmtKind};

pub struct Classifier<'input> {
    file_id: FileId,
    text: &'input str,
    diagnostics: &'input RefCell<DiagnosticSink>,
    tokenizer: Peekable<Tokenizer<'input>>,
    interner: &'input mut crate::intern::StringInterner,
}

impl<'input> Classifier<'input> {
    pub fn new(
        options: &TokenizerOptions,
        file_id: FileId,
        text: &'input str,
        diagnostics: &'input RefCell<DiagnosticSink>,
        interner: &'input mut crate::intern::StringInterner,
    ) -> Self {
        Classifier {
            file_id,
            text,
            diagnostics,
            tokenizer: Tokenizer::new(options, file_id, text, diagnostics).peekable(),
            interner,
        }
    }

    fn bump(&mut self) -> Option<Token> {
        self.tokenizer.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokenizer.peek()
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
        self.diagnostics.borrow_mut().emit_error_from_contents(
            &self.text,
            AnalysisErrorKind::Parser(err),
            Span {
                file_id: self.file_id,
                start,
                end,
            },
            msg,
        )
    }

    fn text_len(&self) -> u32 {
        self.text.len() as u32
    }

    fn end_program_construct<MakeEndConstruct>(
        &mut self,
        start_span: &Span,
        make_end_construct: MakeEndConstruct,
    ) -> Stmt
    where
        MakeEndConstruct: FnOnce(Option<Spanned<InternedName>>) -> StmtKind,
    {
        let (name, end) = match self.tokenizer.peek() {
            Some(t) if t.is_name() => (
                Some(Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                )),
                self.bump().unwrap().span.end,
            ),
            _ => (None, start_span.end),
        };

        self.expect_eos();

        Stmt {
            kind: make_end_construct(name),
            span: Span {
                file_id: self.file_id,
                start: start_span.start,
                end,
            },
        }
    }

    fn expected_token(&mut self, tokens: &[TokenKind], start_span: &Span) -> Stmt {
        let mut msg = if tokens.len() == 1 {
            format!("expected {}", tokens[0].friendly_name())
        } else if tokens.len() == 2 {
            format!(
                "expected {} or {}",
                tokens[0].friendly_name(),
                tokens[1].friendly_name()
            )
        } else {
            let mut msg: String = "expected one of ".into();
            let len = tokens.len();
            for (i, t) in tokens.iter().enumerate() {
                if i == len - 1 {
                    write!(&mut msg, "or {}", t.friendly_name()).unwrap();
                } else {
                    write!(&mut msg, "{}, ", t.friendly_name()).unwrap();
                }
            }

            msg
        };

        match self.peek() {
            Some(t) => {
                write!(&mut msg, ", found {}", t.friendly_name()).unwrap();
                let span = t.span.clone();

                self.emit_error_span(ParserErrorCode::ExpectedToken, span, &msg);
            }
            None => {
                write!(&mut msg, ", found end of file").unwrap();

                self.emit_error_span(
                    ParserErrorCode::ExpectedToken,
                    Span {
                        file_id: self.file_id,
                        start: self.text_len(),
                        end: self.text_len(),
                    },
                    &msg,
                );
            }
        };

        match self.take_until_eos() {
            Some(span) => self.unclassifiable(start_span.start, span.end),
            None => self.unclassifiable(start_span.start, start_span.end),
        }
    }

    fn unclassifiable(&self, idx0: u32, idx1: u32) -> Stmt {
        Stmt {
            kind: StmtKind::Unclassifiable,
            span: Span {
                file_id: self.file_id,
                start: idx0,
                end: idx1,
            },
        }
    }

    fn is_eos(t: &Token) -> bool {
        match t.kind {
            TokenKind::NewLine | TokenKind::SemiColon | TokenKind::Commentary => true,
            _ => false,
        }
    }

    fn take_until_eos(&mut self) -> Option<Span> {
        let mut last_seen = None;
        for t in &mut self.tokenizer {
            if Self::is_eos(&t) {
                return last_seen;
            }

            last_seen = Some(t.span);
        }

        last_seen
    }

    fn expect_eos(&mut self) {
        let start = if let Some(t) = self.tokenizer.peek() {
            t.span.start
        } else {
            return;
        };

        match self.tokenizer.peek() {
            Some(t) if Self::is_eos(&t) => {
                self.bump();
                return;
            }
            _ => {
                let end = self.take_until_eos().map_or(self.text_len(), |s| s.end);
                self.emit_error(
                    ParserErrorCode::ExpectedEOS,
                    start,
                    end,
                    "expected newline, commentary or `;`",
                )
            }
        };
    }

    fn program(&mut self, start_span: &Span) -> Stmt {
        let (name, end) = match self.tokenizer.peek() {
            Some(t) if t.is_name() => (
                Some(Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                )),
                self.bump().unwrap().span.end,
            ),
            _ => (None, start_span.end),
        };

        self.expect_eos();

        Stmt {
            kind: StmtKind::Program { name },
            span: Span {
                file_id: self.file_id,
                start: start_span.start,
                end,
            },
        }
    }

    fn end_program(&mut self, start_span: &Span) -> Stmt {
        self.end_program_construct(start_span, |name| StmtKind::EndProgram { name })
    }

    /// Call for statement classifications that aren't implemented
    fn unimplemented(&mut self, start_span: &Span) -> Stmt {
        match self.take_until_eos() {
            Some(span) => self.unclassifiable(start_span.start, span.end),
            None => self.unclassifiable(start_span.start, start_span.end),
        }
    }

    /// TODO: Parse procedures
    fn procedure(&mut self, start_span: &Span) -> Stmt {
        self.unimplemented(start_span)
    }

    // TODO: Parse module procedure statement
    fn module_procedure(&mut self, start_span: &Span) -> Stmt {
        self.unimplemented(start_span)
    }

    // TODO: Parse function statement
    fn function(&mut self, start_span: &Span) -> Stmt {
        self.unimplemented(start_span)
    }

    // TODO: Parse function statement
    fn subroutine(&mut self, start_span: &Span) -> Stmt {
        self.unimplemented(start_span)
    }

    // TODO: Parse function or subroutine statement when it's not yet obvious which it is
    fn subroutine_or_function(&mut self, start_span: &Span) -> Stmt {
        match self.take_until_eos() {
            Some(span) => self.unclassifiable(start_span.start, span.end),
            None => self.unclassifiable(start_span.start, start_span.end),
        }
    }

    /// Parses from a statement starting with 'module'. Can be a procedure-stmt, module-stmt,
    /// function-stmt, or subroutine-stmt
    fn module(&mut self, start_span: &Span) -> Stmt {
        let (name, end) = match self.tokenizer.peek() {
            // Don't consume module keyword for procedures - let the delegated routine parse it.
            Some(Token {
                kind: TokenKind::Keyword(KeywordTokenKind::Procedure),
                span,
            }) => {
                let span = span.clone();
                return self.module_procedure(&Span {
                    file_id: self.file_id,
                    start: start_span.start,
                    end: span.end,
                });
            }
            Some(Token {
                kind: TokenKind::Keyword(KeywordTokenKind::Function),
                ..
            }) => return self.function(start_span),
            Some(Token {
                kind: TokenKind::Keyword(KeywordTokenKind::Subroutine),
                ..
            }) => return self.subroutine(start_span),
            Some(t) if t.is_maybe_routine_prefix() => {
                return self.subroutine_or_function(start_span);
            }
            Some(t) if t.is_name() => (
                Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                ),
                self.bump().unwrap().span.end,
            ),
            _ => {
                return self.expected_token(
                    &[
                        TokenKind::Keyword(KeywordTokenKind::Procedure),
                        TokenKind::Keyword(KeywordTokenKind::Function),
                        TokenKind::Keyword(KeywordTokenKind::Subroutine),
                        TokenKind::Keyword(KeywordTokenKind::Type),
                        TokenKind::Keyword(KeywordTokenKind::Class),
                        TokenKind::Keyword(KeywordTokenKind::Integer),
                        TokenKind::Keyword(KeywordTokenKind::Real),
                        TokenKind::Keyword(KeywordTokenKind::Double),
                        TokenKind::Keyword(KeywordTokenKind::DoublePrecision),
                        TokenKind::Keyword(KeywordTokenKind::Complex),
                        TokenKind::Keyword(KeywordTokenKind::Character),
                        TokenKind::Keyword(KeywordTokenKind::Logical),
                        TokenKind::Name,
                    ],
                    start_span,
                );
            }
        };

        self.expect_eos();

        Stmt {
            kind: StmtKind::Module { name: name },
            span: Span {
                file_id: self.file_id,
                start: start_span.start,
                end,
            },
        }
    }

    /// Parses a statement starting with `endmodule` or `end module`.
    fn end_module(&mut self, start_span: &Span) -> Stmt {
        self.end_program_construct(start_span, |name| StmtKind::EndModule { name })
    }

    /// Parses a submodule statement
    fn submodule(&mut self, start_span: &Span) -> Stmt {
        match self.peek() {
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) => self.bump().unwrap(),
            _ => {
                return self.expected_token(&[TokenKind::LeftParen], start_span);
            }
        };

        // Parse the parent identifier.
        let ancestor_module_name = match self.peek() {
            Some(t) if t.is_name() => self
                .bump()
                .unwrap()
                .try_intern_contents(&mut self.interner, &self.text)
                .unwrap(),
            _ => {
                return self.expected_token(&[TokenKind::Name], start_span);
            }
        };

        let parent_submodule_name = match self.peek() {
            Some(Token {
                kind: TokenKind::Colon,
                ..
            }) => {
                self.bump();
                Some(match self.peek() {
                    Some(t) if t.is_name() => self
                        .bump()
                        .unwrap()
                        .try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    _ => {
                        return self.expected_token(&[TokenKind::Name], start_span);
                    }
                })
            }
            Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) => {
                // Don't consume yet - just return parent submodule name
                None
            }
            _ => {
                return self.expected_token(&[TokenKind::Colon, TokenKind::RightParen], start_span);
            }
        };

        let parent_identifier = ParentIdentifier {
            ancestor_module_name,
            parent_submodule_name,
        };

        match self.peek() {
            Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) => self.bump(),
            _ => {
                return self.expected_token(&[TokenKind::RightParen], start_span);
            }
        };

        let name = match self.peek() {
            Some(t) if t.is_name() => {
                let t = self.bump().unwrap();

                let span = t.span;

                Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    span,
                )
            }
            _ => {
                return self.expected_token(&[TokenKind::Name], start_span);
            }
        };

        self.expect_eos();

        let span = name.span;
        Stmt {
            kind: StmtKind::Submodule {
                parent_identifier,
                name,
            },
            span: Span {
                file_id: self.file_id,
                start: start_span.start,
                end: span.end,
            },
        }
    }

    /// Parses a statement starting with `endsubmodule` or `end submodule`.
    fn end_submodule(&mut self, start_span: &Span) -> Stmt {
        self.end_program_construct(start_span, |name| StmtKind::EndSubmodule { name })
    }

    fn end(&mut self, start_span: &Span) -> Stmt {
        let token = match self.tokenizer.peek() {
            Some(token) => token,
            None => {
                return Stmt {
                    kind: StmtKind::End,
                    span: *start_span,
                }
            }
        };

        match token.kind {
            TokenKind::Keyword(KeywordTokenKind::Program) => {
                self.bump();
                self.end_program(start_span)
            }
            TokenKind::Keyword(KeywordTokenKind::Module) => {
                self.bump();
                self.end_module(start_span)
            }
            TokenKind::Keyword(KeywordTokenKind::Submodule) => {
                self.bump();
                self.end_submodule(start_span)
            }
            _ => {
                self.expect_eos();
                Stmt {
                    kind: StmtKind::End,
                    span: *start_span,
                }
            }
        }
    }

    fn internal_next(&mut self) -> Option<Stmt> {
        let token = self.tokenizer.next()?;

        let stmt = match token.kind {
            TokenKind::Keyword(KeywordTokenKind::Program) => self.program(&token.span),
            TokenKind::Keyword(KeywordTokenKind::EndProgram) => self.end_program(&token.span),
            TokenKind::Keyword(KeywordTokenKind::Module) => self.module(&token.span),
            TokenKind::Keyword(KeywordTokenKind::EndModule) => self.end_module(&token.span),
            TokenKind::Keyword(KeywordTokenKind::End) => self.end(&token.span),
            TokenKind::Keyword(KeywordTokenKind::Submodule) => self.submodule(&token.span),
            TokenKind::Keyword(KeywordTokenKind::EndSubmodule) => self.end_submodule(&token.span),
            _ => self.unclassifiable(token.span.start, token.span.end),
        };

        Some(stmt)
    }
}

impl<'input> Iterator for Classifier<'input> {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        self.internal_next()
    }
}
