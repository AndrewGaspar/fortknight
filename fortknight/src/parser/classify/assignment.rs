//! Expression parsing for classifier

use crate::parser::lex::TokenKind;

use super::statements::{AssignmentStmt, Designator, Spanned, Stmt, StmtKind, Variable};
use super::Classifier;

impl<'input, 'arena> Classifier<'input, 'arena> {
    /// R1032: assignment-stmt
    ///
    /// Parses an assignment-stmt from the beginning of the statement. When an error is encountered,
    /// an error is emitted and the statement is skipped, marking it as unclassifiable.
    pub(super) fn assignment_stmt(&mut self) -> Stmt<'arena> {
        // TODO: This isn't right, but just parsing variable names for now.
        let designator = match self.peek_kind() {
            Some(t) if t.is_name() => {
                let t = self.bump().unwrap();
                Spanned::new(
                    Designator::ObjectName(
                        t.try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                    ),
                    t.span,
                )
            }
            _ => {
                self.emit_expected_token(&[TokenKind::Name]);
                let end_span = self.take_until_eos();
                return self.unclassifiable(
                    end_span.map_or(self.text_len(), |s| s.start),
                    end_span.map_or(self.text_len(), |s| s.end),
                );
            }
        };

        let variable = Spanned::new(
            Variable::DesignatorOrFunctionReference(designator.val),
            designator.span,
        );

        match self.peek_kind() {
            Some(TokenKind::Equals) => {
                self.bump();
            }
            _ => {
                self.emit_expected_token(&[TokenKind::Equals]);
                let end_span = self.take_until_eos();
                return self.unclassifiable(
                    designator.span.start,
                    end_span.map_or(designator.span.end, |s| s.end),
                );
            }
        };

        let expr = match self.expr() {
            Some(expr) => Spanned::new(&*self.arena.expressions.alloc(expr.val), expr.span),
            _ => {
                self.emit_expected_token(&[
                    TokenKind::Name,
                    TokenKind::LeftParen,
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::NotOp,
                ]);
                let end_span = self.take_until_eos();
                return self.unclassifiable(
                    designator.span.start,
                    end_span.map_or(designator.span.end, |s| s.end),
                );
            }
        };

        self.expect_eos();

        Stmt::new(
            StmtKind::Assignment(AssignmentStmt { variable, expr }),
            designator.span.concat(expr.span),
        )
    }
}
