use super::lex::Tokenizer;
use crate::parser::lex::Token;

mod statements;

pub struct Classifier<'t> {
    tokenizer: Tokenizer<'t>,
}

impl<'t> Iterator for Classifier<'t> {
    type Item = statements::Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
