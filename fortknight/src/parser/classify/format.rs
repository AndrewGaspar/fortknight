use crate::span::Span;

use super::statements::{FormatStmt, Stmt};
use super::Classifier;

impl<'input, 'arena> Classifier<'input, 'arena> {
    pub(super) fn format<'a>(&mut self, start_span: &Span) -> Stmt<'arena> {
        todo!()
    }
}
