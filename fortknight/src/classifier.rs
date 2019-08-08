use crate::lex::Token;

mod statements;

pub struct Classifier<'t> {
    tokens: &'t [Token],
}

impl<'t> Iterator for Classifier<'t> {
    type Item = statements::Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
