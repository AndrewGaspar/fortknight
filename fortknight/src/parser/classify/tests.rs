use std::cell::RefCell;
/// Tests to verify correct statements are returned by classifier
use std::default::Default;

use crate::error::DiagnosticSink;
use crate::index::FileId;
use crate::intern::StringInterner;
use crate::parser::classify::{Classifier, StmtKind};
use crate::parser::lex::TokenizerOptions;

fn get_stmts_unwrap(text: &str) -> Vec<StmtKind> {
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let mut interner = StringInterner::new();

    let classifier = Classifier::new(
        &TokenizerOptions::default(),
        FileId(0),
        text,
        &sink,
        &mut interner,
    );

    classifier.map(|s| s.kind).collect()
}

#[test]
fn program() {
    use StmtKind::*;

    assert_eq!(
        vec![Program { name: None }, EndProgram { name: None }],
        get_stmts_unwrap("program; end program")
    );
}
