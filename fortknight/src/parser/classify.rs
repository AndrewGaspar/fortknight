use std::cell::RefCell;
use std::fmt::Write;

use peek_nth::{IteratorExt, PeekableNth};
use typed_arena::Arena;

use crate::error::{AnalysisErrorKind, DiagnosticSink, ParserErrorCode, SemanticErrorCode};
use crate::index::FileId;
use crate::intern::InternedName;
use crate::parser::lex::{KeywordTokenKind, Token, TokenKind, Tokenizer, TokenizerOptions};
use crate::span::Span;

mod block;
mod implicit;
mod import_stmt;
mod statements;
mod types;
mod use_stmt;

#[cfg(test)]
mod tests;

use statements::{
    Expr, ImplicitSpec, LetterSpec, Only, ParentIdentifier, Rename, Spanned, TypeParamSpec,
};
pub use statements::{Stmt, StmtKind};

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
}

impl<'arena> ClassifierArena<'arena> {
    pub fn new() -> Self {
        ClassifierArena::default()
    }
}

pub struct Classifier<'input, 'arena> {
    file_id: FileId,
    text: &'input str,
    diagnostics: &'input RefCell<DiagnosticSink>,
    tokenizer: PeekableNth<Tokenizer<'input>>,
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
            tokenizer: Tokenizer::new(options, file_id, text, diagnostics).peekable_nth(),
            interner,
            arena,
        }
    }

    fn bump(&mut self) -> Option<Token> {
        self.tokenizer.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokenizer.peek()
    }

    fn peek_nth(&mut self, n: usize) -> Option<&Token> {
        self.tokenizer.peek_nth(n)
    }

    fn peek_kind(&mut self) -> Option<TokenKind> {
        self.peek().map(|t| t.kind)
    }

    fn peek_nth_kind(&mut self, n: usize) -> Option<TokenKind> {
        self.peek_nth(n).map(|t| t.kind)
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

    fn emit_expected_token(&mut self, tokens: &[TokenKind]) {
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
    }

    fn expected_token(&mut self, tokens: &[TokenKind], start_span: &Span) -> Stmt<'arena> {
        self.emit_expected_token(tokens);

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
        F: FnMut(Lookahead) -> TakeUntil,
    {
        loop {
            return match self.peek() {
                Some(t) => match terminate(Lookahead::Token(t)) {
                    TakeUntil::Continue => {
                        self.bump();
                        continue;
                    }
                    TakeUntil::Stop => Ok(t.span.end),
                    TakeUntil::Abort => Err(t.span.end),
                },
                None => match terminate(Lookahead::EOF) {
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

        if Some(TokenKind::LeftParen) != self.peek_nth_kind(idx) {
            return None;
        }

        loop {
            match self.peek_nth_kind(idx) {
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
        let mut last_seen: Option<Span> = self.peek().map(|t| t.span);
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

    /// R1022: expr
    ///
    /// Parses an expression from the beginning of the expression. When an error is encountered, an
    /// error is emitted and None is returned without consuming the erroneous token.
    fn expr(&mut self) -> Option<Spanned<Expr<'arena>>> {
        unimplemented!()
    }

    fn program(&mut self, start_span: &Span) -> Stmt<'arena> {
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
        match self.take_until_eos() {
            Some(span) => self.unclassifiable(start_span.start, span.end),
            None => self.unclassifiable(start_span.start, self.text_len()),
        }
    }

    /// Parses from a statement starting with 'module'. Can be a procedure-stmt, module-stmt,
    /// function-stmt, or subroutine-stmt
    fn module(&mut self, start_span: &Span) -> Stmt<'arena> {
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
    fn end_module(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.end_program_construct(start_span, |name| StmtKind::EndModule { name })
    }

    /// Parses a submodule statement
    fn submodule(&mut self, start_span: &Span) -> Stmt<'arena> {
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
    fn end_submodule(&mut self, start_span: &Span) -> Stmt<'arena> {
        self.end_program_construct(start_span, |name| StmtKind::EndSubmodule { name })
    }

    fn end(&mut self, start_span: &Span) -> Stmt<'arena> {
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
            TokenKind::Keyword(KeywordTokenKind::Block) => {
                let span = self.bump().unwrap().span;
                self.stmt_from_end_block(start_span.concat(span))
            }
            TokenKind::Keyword(KeywordTokenKind::BlockData) => {
                let span = self.bump().unwrap().span;
                self.stmt_from_end_block_data(start_span.concat(span))
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

    /// Parses a contains-stmt after the contains
    fn contains(&mut self, start_span: Span) -> Stmt<'arena> {
        self.expect_eos();

        Stmt {
            kind: StmtKind::Contains,
            span: start_span,
        }
    }

    pub fn next_stmt(&mut self) -> Option<Stmt<'arena>> {
        let token = self.tokenizer.next()?;

        let stmt = match token.kind {
            TokenKind::Keyword(KeywordTokenKind::End) => self.end(&token.span),
            TokenKind::Keyword(KeywordTokenKind::Implicit) => self.implicit_stmt(token.span),
            TokenKind::Keyword(KeywordTokenKind::Import) => self.import_statement(token.span),
            TokenKind::Keyword(KeywordTokenKind::Program) => self.program(&token.span),
            TokenKind::Keyword(KeywordTokenKind::EndProgram) => self.end_program(&token.span),
            TokenKind::Keyword(KeywordTokenKind::Module) => self.module(&token.span),
            TokenKind::Keyword(KeywordTokenKind::EndModule) => self.end_module(&token.span),
            TokenKind::Keyword(KeywordTokenKind::Submodule) => self.submodule(&token.span),
            TokenKind::Keyword(KeywordTokenKind::EndSubmodule) => self.end_submodule(&token.span),
            TokenKind::Keyword(KeywordTokenKind::Use) => self.use_statement(&token.span),
            TokenKind::Keyword(KeywordTokenKind::Contains) => self.contains(token.span),
            TokenKind::Keyword(KeywordTokenKind::Block) => self.stmt_from_block(token.span),
            TokenKind::Keyword(KeywordTokenKind::BlockData) => {
                self.stmt_from_block_data(token.span)
            }
            TokenKind::Keyword(KeywordTokenKind::EndBlock) => self.stmt_from_end_block(token.span),
            TokenKind::Keyword(KeywordTokenKind::EndBlockData) => {
                self.stmt_from_end_block_data(token.span)
            }
            _ => self.unclassifiable(token.span.start, token.span.end),
        };

        Some(stmt)
    }

    /// Called when there's an error parsing some comma separated list - attempt to skip to the next
    /// item in the list, or to the end. Returns Some(()) when reaching a comma, returns None when
    /// reached EOS.
    ///
    /// The comma/EOS is consumed
    fn skip_to_comma_or_eos(&mut self) -> Option<()> {
        // advance to next `only` or EOS
        self.take_until(|lookahead| match lookahead {
            Lookahead::Token(&Token {
                kind: TokenKind::Comma,
                ..
            }) => TakeUntil::Stop,
            Lookahead::Token(t) if Self::is_eos(t) => TakeUntil::Stop,
            Lookahead::EOF => TakeUntil::Stop,
            _ => TakeUntil::Continue,
        })
        .unwrap();

        // bumps the final token so that we're either ready to parse the next only or the next
        // statement
        self.bump();

        match self.peek() {
            Some(t) if Self::is_eos(t) => None,
            None => None,
            // Consume the comma so we're ready to parse the next potential "only"
            _ => Some(()),
        }
    }
}

fn eos_or(tokens: &[TokenKind]) -> Vec<TokenKind> {
    let mut vec = Vec::with_capacity(3 + tokens.len());

    vec.extend_from_slice(&[
        TokenKind::SemiColon,
        TokenKind::NewLine,
        TokenKind::Commentary,
    ]);
    vec.extend_from_slice(tokens);

    vec
}

const INTRINSIC_TYPE_SPEC_TOKENS: &'static [TokenKind] = &[
    TokenKind::Keyword(KeywordTokenKind::Integer),
    TokenKind::Keyword(KeywordTokenKind::Real),
    TokenKind::Keyword(KeywordTokenKind::Double),
    TokenKind::Keyword(KeywordTokenKind::DoublePrecision),
    TokenKind::Keyword(KeywordTokenKind::Complex),
    TokenKind::Keyword(KeywordTokenKind::Character),
    TokenKind::Keyword(KeywordTokenKind::Logical),
];

fn intrinsic_type_spec_or(tokens: &[TokenKind]) -> Vec<TokenKind> {
    let mut vec = Vec::with_capacity(INTRINSIC_TYPE_SPEC_TOKENS.len() + tokens.len());

    vec.extend_from_slice(INTRINSIC_TYPE_SPEC_TOKENS);
    vec.extend_from_slice(tokens);

    vec
}

fn declaration_type_spec_or(tokens: &[TokenKind]) -> Vec<TokenKind> {
    let mut vec = Vec::with_capacity(2 + INTRINSIC_TYPE_SPEC_TOKENS.len() + tokens.len());

    vec.extend_from_slice(INTRINSIC_TYPE_SPEC_TOKENS);
    vec.extend_from_slice(&[
        TokenKind::Keyword(KeywordTokenKind::Type),
        TokenKind::Keyword(KeywordTokenKind::Class),
    ]);
    vec.extend_from_slice(tokens);

    vec
}
