//! Tests for the LowLevelLexer

use std::cell::RefCell;
use std::default::Default;

use crate::error::DiagnosticSink;
use crate::index::FileId;
use crate::parser::lex::low_level::LowLevelLexer;
use crate::parser::lex::TokenizerOptions;

fn ll_lex(text: &str) -> String {
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    LowLevelLexer::new(&TokenizerOptions::default(), FileId(0), text, &sink)
        .map(|(_, c)| c)
        .collect()
}

#[test]
fn basic() {
    assert_eq!("howdyall", ll_lex("howdy&\n&all"));
    assert_eq!("howdyall", ll_lex("howdy&\r\n&all"));
    assert_eq!("howdy all", ll_lex("howdy&\nall"));
    assert_eq!("howdy all", ll_lex("howdy&\n all"));
    assert_eq!("howdy  all", ll_lex("howdy&\n  all"));
    assert_eq!("howdy\nall", ll_lex("howdy\r\nall"));
    assert_eq!("howdyall", ll_lex("howdy&\n  !comment\n&all"));
    assert_eq!("howdy all", ll_lex("howdy&\n  !comment\n& all"));
}

#[test]
fn in_string_literal() {
    assert_eq!("\"howdyall\"", ll_lex("\"howdy&\n&all\""));
    assert_eq!(
        "\"howdy!commentall\"",
        ll_lex("\"howdy&\n&!comment&\n&all\"")
    );

    // this is erroneous, but this is how we want the low level lexer to handle the error case
    assert_eq!("\"howdyall\"", ll_lex("\"howdy&\nall\""));
    assert_eq!("\"howdy all\"", ll_lex("\"howdy&\n all\""));
}
