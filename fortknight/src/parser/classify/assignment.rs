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
        let start_span = self.tokenizer.peek().unwrap().span;

        // TODO: This isn't right, but just parsing variable names for now.
        let designator = if self.check_name() {
            let t = self.tokenizer.bump().unwrap();
            Spanned::new(
                Designator::ObjectName(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                ),
                t.span,
            )
        } else {
            return self.unexpected_token(&start_span);
        };

        let variable = Spanned::new(
            Variable::DesignatorOrFunctionReference(designator.val),
            designator.span,
        );

        if self.check(TokenKind::Equals) {
            self.tokenizer.bump();
        } else {
            return self.unexpected_token(&start_span);
        }

        let expr = match self.expr() {
            Some(expr) => Spanned::new(&*self.arena.expressions.alloc(expr.val), expr.span),
            _ => {
                return self.unexpected_token(&start_span);
            }
        };

        self.expect_eos();

        Stmt {
            kind: StmtKind::Assignment(AssignmentStmt { variable, expr }),
            span: designator.span.concat(expr.span),
        }
    }
}
