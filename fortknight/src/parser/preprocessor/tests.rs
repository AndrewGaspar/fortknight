//! Tests for the FortranPreprocessor

use std::cell::RefCell;

use crate::error::DiagnosticSink;
use crate::index::FileId;
use crate::parser::preprocessor::FortranPreprocessor;

fn ll_lex(text: &str) -> String {
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    FortranPreprocessor::new(false, FileId(0), text, &sink)
        .map(|(_, c)| c)
        .collect()
}

fn ll_lex_iw(text: &str) -> String {
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let mut lex = FortranPreprocessor::new(false, FileId(0), text, &sink);

    lex.insignificant_whitespace(true);

    lex.map(|(_, c)| c).collect()
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

#[test]
fn rock_star() {
    assert_eq!(
        "'hey now  you''re a rock star,  get your game on,  go play!'",
        ll_lex(
            "'hey now &
        & you''re a rock star, &
        & get your game on, &
        & go play!'"
        )
    );
}

#[test]
fn insignificant_whitespace() {
    assert_eq!("w o m b o", ll_lex("w o m b o"));
    assert_eq!("wombo", ll_lex_iw("w o m b o"));
}
