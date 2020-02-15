use std::cell::RefCell;
use std::fmt::Write;

use num_bigint::BigUint;
use typed_arena::Arena;

use crate::error::{AnalysisErrorKind, DiagnosticSink, ParserErrorCode, SemanticErrorCode};
use crate::index::FileId;
use crate::intern::InternedName;
use crate::parser::lex::{
    KeywordTokenKind, LexMode, PeekableTokenizer, Token, TokenKind, TokenizerOptions,
};
use crate::{num::Uint, span::Span};

#[macro_use]
mod macros;

mod assignment;
mod block;
mod expectations;
mod expressions;
mod format;
mod implicit;
mod import_stmt;
mod list;
mod statements;
mod types;
mod use_stmt;

#[cfg(test)]
mod tests;

pub use list::CommaResult;
use statements::{
    Expr, FormatItem, ImplicitSpec, LetterSpec, Only, ParentIdentifier, Rename, Spanned,
    TypeParamSpec,
};
pub use statements::{LabeledStmt, Stmt, StmtKind};

pub enum Lookahead<'a> {
    Token(&'a Token),
    EOF,
}

pub enum TakeUntil {
    Continue,
    Stop,
    Abort,
}

#[derive(Default)]
pub struct ClassifierArena<'arena> {
    onlys: Arena<Spanned<Only>>,
    renames: Arena<Spanned<Rename>>,
    names: Arena<Spanned<InternedName>>,
    implicit_specs: Arena<Spanned<ImplicitSpec<'arena>>>,
    type_param_specs: Arena<TypeParamSpec<'arena>>,
    letter_specs: Arena<LetterSpec>,
    expressions: Arena<Expr<'arena>>,
    big_uints: Arena<BigUint>,
    string_literals: Arena<String>,
    format_items: Arena<FormatItem<'arena>>,
    vs: Arena<statements::V<'arena>>,
}

impl<'arena> ClassifierArena<'arena> {
    pub fn new() -> Self {
        ClassifierArena::default()
    }
}

struct TokenState<'input> {
    tokenizer: PeekableTokenizer<'input>,
    pub(crate) expected_tokens: Vec<TokenKind>,
}

impl<'input> TokenState<'input> {
    pub(crate) fn clear_current_state(&mut self) {
        self.expected_tokens.clear();
    }

    pub(crate) fn bump(&mut self) -> Option<Token> {
        self.clear_current_state();
        self.tokenizer.next()
    }

    pub(crate) fn peek(&mut self) -> Option<&Token> {
        self.tokenizer.peek()
    }

    pub(crate) fn peek_nth(&mut self, n: usize) -> Option<&Token> {
        self.tokenizer.peek_nth(n)
    }

    fn peek_kind(&mut self) -> Option<TokenKind> {
        self.tokenizer.peek().map(|t| t.kind)
    }

    pub(crate) fn peek_nth_kind(&mut self, n: usize) -> Option<TokenKind> {
        self.tokenizer.peek_nth(n).map(|t| t.kind)
    }

    pub(crate) fn expect_token(&mut self, kind: TokenKind, expected: TokenKind) -> bool {
        if kind == expected {
            true
        } else {
            self.expected_tokens.push(expected);
            false
        }
    }

    pub(crate) fn push_expected(&mut self, expected: TokenKind) {
        self.expected_tokens.push(expected);
    }
}

pub struct Classifier<'input, 'arena> {
    file_id: FileId,
    text: &'input str,
    diagnostics: &'input RefCell<DiagnosticSink>,
    tokenizer: TokenState<'input>,
    interner: &'input mut crate::intern::StringInterner,
    arena: &'arena ClassifierArena<'arena>,
}

impl<'input, 'arena> Classifier<'input, 'arena> {
    pub fn new(
        options: &TokenizerOptions,
        file_id: FileId,
        text: &'input str,
        diagnostics: &'input RefCell<DiagnosticSink>,
        interner: &'input mut crate::intern::StringInterner,
        arena: &'arena ClassifierArena<'arena>,
    ) -> Self {
        Classifier {
            file_id,
            text,
            diagnostics,
            tokenizer: TokenState {
                tokenizer: PeekableTokenizer::new(options, file_id, text, diagnostics),
                expected_tokens: Vec::new(),
            },
            interner,
            arena,
        }
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

    fn emit_semantic_error(&mut self, err: SemanticErrorCode, start: u32, end: u32, msg: &str) {
        self.diagnostics.borrow_mut().emit_error_from_contents(
            &self.text,
            AnalysisErrorKind::Semantic(err),
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

    fn end_program_construct<'a, MakeEndConstruct>(
        &'a mut self,
        start_span: &Span,
        make_end_construct: MakeEndConstruct,
    ) -> Stmt<'arena>
    where
        MakeEndConstruct: FnOnce(Option<Spanned<InternedName>>) -> StmtKind<'arena>,
    {
        let (name, end) = match self.tokenizer.peek() {
            Some(t) if t.is_name() => (
                Some(Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                )),
                self.tokenizer.bump().unwrap().span.end,
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

    fn emit_expected_token(&mut self, tokens: &[TokenKind]) {
        debug_assert!(
            !tokens.is_empty(),
            "Internal compiler error: tried to emit expected tokens, but no tokens were expected"
        );

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

        match self.tokenizer.peek() {
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
    }

    fn expect_token(&mut self, kind: TokenKind, expected: TokenKind) -> bool {
        self.tokenizer.expect_token(kind, expected)
    }

    fn emit_unexpected_token(&mut self) {
        let mut tokens: Vec<_> = self.tokenizer.expected_tokens.iter().cloned().collect();
        tokens.sort();
        tokens.dedup();
        self.emit_expected_token(&tokens[..]);
        self.tokenizer.expected_tokens.clear();
    }

    fn unexpected_token(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.emit_unexpected_token();

        match self.take_until_eos() {
            Some(span) => self.unclassifiable(start_span.start, span.end),
            None => self.unclassifiable(start_span.start, start_span.end),
        }
    }

    fn unclassifiable(&self, idx0: u32, idx1: u32) -> Stmt<'arena> {
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
        t.kind.is_eos()
    }

    fn take_until<F>(&mut self, mut terminate: F) -> Result<u32, u32>
    where
        F: FnMut(Option<&Token>) -> TakeUntil,
    {
        loop {
            return match self.tokenizer.peek() {
                Some(t) => match terminate(Some(t)) {
                    TakeUntil::Continue => {
                        self.tokenizer.bump();
                        continue;
                    }
                    TakeUntil::Stop => Ok(t.span.end),
                    TakeUntil::Abort => Err(t.span.end),
                },
                None => match terminate(None) {
                    TakeUntil::Continue => panic!("Internal error: Tried to classify past EOF!"),
                    TakeUntil::Stop => Ok(self.text_len()),
                    TakeUntil::Abort => Err(self.text_len()),
                },
            };
        }
    }

    /// This function helps lookahead past a parenthetical - it returns the index ahead where the
    /// current parenthetical ends. If we reach the end of a statement, None is returned.
    fn idx_past_parentheticals_in_statement(&mut self, start_idx: usize) -> Option<usize> {
        let mut depth = 0;
        let mut idx = start_idx;

        if Some(TokenKind::LeftParen) != self.tokenizer.peek_nth_kind(idx) {
            return None;
        }

        loop {
            match self.tokenizer.peek_nth_kind(idx) {
                Some(TokenKind::LeftParen) => {
                    depth += 1;
                }
                Some(TokenKind::RightParen) => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(idx + 1);
                    }
                }
                Some(t) if t.is_eos() => {
                    return None;
                }
                None => {
                    return None;
                }
                _ => {}
            }

            idx += 1;
        }
    }

    fn take_until_eos(&mut self) -> Option<Span> {
        self.tokenizer.clear_current_state();
        let mut last_seen: Option<Span> = self.tokenizer.peek().map(|t| t.span);
        while let Some(t) = self.tokenizer.bump() {
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
            Some(t) if Self::is_eos(t) => {
                self.tokenizer.bump();
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

    fn program(&mut self, start_span: &Span) -> Stmt<'arena> {
        let (name, end) = match self.tokenizer.peek() {
            Some(t) if t.is_name() => {
                let t = *t;
                (
                    Some(Spanned::new(
                        t.try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                        t.span,
                    )),
                    self.tokenizer.bump().unwrap().span.end,
                )
            }
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

    fn end_program(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.end_program_construct(start_span, |name| StmtKind::EndProgram { name })
    }

    /// Call for statement classifications that aren't implemented
    fn unimplemented(&mut self, start_span: &Span) -> Stmt<'arena> {
        match self.take_until_eos() {
            Some(span) => self.unclassifiable(start_span.start, span.end),
            None => self.unclassifiable(start_span.start, self.text_len()),
        }
    }

    /// TODO: Parse procedures
    fn procedure(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.unimplemented(start_span)
    }

    // TODO: Parse module procedure statement
    fn module_procedure(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.unimplemented(start_span)
    }

    // TODO: Parse function statement
    fn function(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.unimplemented(start_span)
    }

    // TODO: Parse function statement
    fn subroutine(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.unimplemented(start_span)
    }

    // TODO: Parse function or subroutine statement when it's not yet obvious which it is
    fn subroutine_or_function(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.unimplemented(start_span)
    }

    /// Parses from a statement starting with 'module'. Can be a procedure-stmt, module-stmt,
    /// function-stmt, or subroutine-stmt
    fn module(&mut self, start_span: &Span) -> Stmt<'arena> {
        let (name, end) = if self.check(TokenKind::Keyword(KeywordTokenKind::Procedure)) {
            return self.module_procedure(start_span);
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Function)) {
            return self.function(start_span);
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Subroutine)) {
            return self.subroutine(start_span);
        } else if self.check_maybe_routine_prefix() {
            return self.subroutine_or_function(start_span);
        } else if self.check_name() {
            let t = self.tokenizer.bump().unwrap();
            (
                Spanned::new(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                ),
                t.span.end,
            )
        } else {
            return self.unexpected_token(start_span);
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
    fn end_module(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.end_program_construct(start_span, |name| StmtKind::EndModule { name })
    }

    /// Parses a submodule statement
    fn submodule(&mut self, start_span: &Span) -> Stmt<'arena> {
        if self.check(TokenKind::LeftParen) {
            self.tokenizer.bump();
        } else {
            return self.unexpected_token(start_span);
        }

        // Parse the parent identifier.
        let ancestor_module_name = if self.check_name() {
            self.tokenizer
                .bump()
                .unwrap()
                .try_intern_contents(&mut self.interner, &self.text)
                .unwrap()
        } else {
            return self.unexpected_token(start_span);
        };

        let parent_submodule_name = if self.check(TokenKind::Colon) {
            self.tokenizer.bump();
            Some(if self.check_name() {
                self.tokenizer
                    .bump()
                    .unwrap()
                    .try_intern_contents(&mut self.interner, &self.text)
                    .unwrap()
            } else {
                return self.unexpected_token(start_span);
            })
        } else if self.check(TokenKind::RightParen) {
            None
        } else {
            return self.unexpected_token(start_span);
        };

        let parent_identifier = ParentIdentifier {
            ancestor_module_name,
            parent_submodule_name,
        };

        if self.check(TokenKind::RightParen) {
            self.tokenizer.bump();
        } else {
            return self.unexpected_token(start_span);
        }

        let name = if self.check_name() {
            let t = self.tokenizer.bump().unwrap();

            let span = t.span;

            Spanned::new(
                t.try_intern_contents(&mut self.interner, &self.text)
                    .unwrap(),
                span,
            )
        } else {
            return self.unexpected_token(start_span);
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
    fn end_submodule(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.end_program_construct(start_span, |name| StmtKind::EndSubmodule { name })
    }

    fn end(&mut self, start_span: &Span) -> Stmt<'arena> {
        if self.tokenizer.peek().is_none() {
            Stmt {
                kind: StmtKind::End,
                span: *start_span,
            }
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Program)) {
            self.tokenizer.bump();
            self.end_program(start_span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Module)) {
            self.tokenizer.bump();
            self.end_module(start_span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Submodule)) {
            self.tokenizer.bump();
            self.end_submodule(start_span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Block)) {
            let span = self.tokenizer.bump().unwrap().span;
            self.stmt_from_end_block(start_span.concat(span))
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::BlockData)) {
            let span = self.tokenizer.bump().unwrap().span;
            self.stmt_from_end_block_data(start_span.concat(span))
        } else {
            self.expect_eos();
            Stmt {
                kind: StmtKind::End,
                span: *start_span,
            }
        }
    }

    /// Parses a contains-stmt after the contains
    fn contains(&mut self, start_span: Span) -> Stmt<'arena> {
        self.expect_eos();

        Stmt {
            kind: StmtKind::Contains,
            span: start_span,
        }
    }

    pub fn next_stmt(&mut self) -> Option<LabeledStmt<'arena>> {
        // Check if there are any more tokens and return None if not.
        self.tokenizer.peek()?;

        let label = if let Some(t) = self.check_and_bump(TokenKind::DigitString) {
            let num = t.try_into_uint(&self.text, &self.arena.big_uints).unwrap();
            let label = match num {
                Uint::Big(_) => None,
                Uint::Small(x) => {
                    if x >= 100000 {
                        None
                    } else {
                        Some(x)
                    }
                }
            };

            let label_span = t.span;

            if let Some(label) = label {
                Some(Spanned::new(label, label_span))
            } else {
                self.emit_error_span(
                    ParserErrorCode::TooLongLabel,
                    label_span,
                    "A label may not exceed five digits.",
                );

                None
            }
        } else {
            None
        };

        // Look ahead for assignments
        // TODO: This just looks for a variable name then =, which is incomplete. Need to support
        // array elements, array sections, substrings, structure components, etc.
        let is_assignment = if self.check_name() {
            match self.tokenizer.peek_nth_kind(1) {
                Some(TokenKind::Equals) => true,
                _ => false,
            }
        } else {
            false
        };

        let stmt = if is_assignment {
            self.assignment_stmt()
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::End)) {
            let token = self.tokenizer.bump().unwrap();
            self.end(&token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Implicit)) {
            let token = self.tokenizer.bump().unwrap();
            self.implicit_stmt(token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Import)) {
            let token = self.tokenizer.bump().unwrap();
            self.import_statement(token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Program)) {
            let token = self.tokenizer.bump().unwrap();
            self.program(&token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::EndProgram)) {
            let token = self.tokenizer.bump().unwrap();
            self.end_program(&token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Module)) {
            let token = self.tokenizer.bump().unwrap();
            self.module(&token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::EndModule)) {
            let token = self.tokenizer.bump().unwrap();
            self.end_module(&token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Submodule)) {
            let token = self.tokenizer.bump().unwrap();
            self.submodule(&token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::EndSubmodule)) {
            let token = self.tokenizer.bump().unwrap();
            self.end_submodule(&token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Use)) {
            let token = self.tokenizer.bump().unwrap();
            self.use_statement(&token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Contains)) {
            let token = self.tokenizer.bump().unwrap();
            self.contains(token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Block)) {
            let token = self.tokenizer.bump().unwrap();
            self.stmt_from_block(token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::BlockData)) {
            let token = self.tokenizer.bump().unwrap();
            self.stmt_from_block_data(token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::EndBlock)) {
            let token = self.tokenizer.bump().unwrap();
            self.stmt_from_end_block(token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::EndBlockData)) {
            let token = self.tokenizer.bump().unwrap();
            self.stmt_from_end_block_data(token.span)
        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Format)) {
            self.format()
        } else if let Some(t) = self.check_eos_and_bump() {
            if let Some(label) = &label {
                self.emit_error_span(
                    ParserErrorCode::LabeledEmptyStatement,
                    label.span,
                    "Labels are not permitted on blank lines",
                );
            }

            Stmt {
                kind: StmtKind::Empty,
                span: t.span,
            }
        } else if self.tokenizer.peek().is_none() {
            return None;
        } else {
            let span = self.tokenizer.peek().unwrap().span;
            self.unexpected_token(&span)
        };

        Some(LabeledStmt { stmt, label })
    }
}
