use crate::span::Span;

use super::statements::Stmt;
use super::Classifier;

impl<'input, 'arena> Classifier<'input, 'arena> {
    pub(super) fn format<'a>(&mut self, _start_span: &Span) -> Stmt<'arena> {
        todo!()
    }
}
