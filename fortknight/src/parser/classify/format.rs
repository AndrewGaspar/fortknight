use crate::span::Span;

use super::statements::Stmt;
use super::Classifier;

impl<'input, 'arena> Classifier<'input, 'arena> {
    pub(super) fn stmt_from_format(&mut self, format_span: Span) -> Stmt<'arena> {
        unimplemented!()
    }
}
