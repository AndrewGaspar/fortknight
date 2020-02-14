//! Test the FormatLexer

use std::cell::RefCell;

use crate::{error::DiagnosticSink, index::FileId};

use super::{
    FormatLexer, FormatToken,
    FormatTokenKind::{self, *},
};

fn tokens(text: &str) -> Vec<FormatTokenKind> {
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    FormatLexer::new(FileId(0), text, &sink)
        .map(|t| t.kind)
        .collect()
}

#[test]
fn basic() {
    assert_eq!(vec![DigitString], tokens("1"));
    assert_eq!(
        vec![DigitString, P, F, DigitString, Dot, DigitString],
        tokens("10PF77.88")
    );
    assert_eq!(
        vec![
            DigitString,
            P,
            F,
            DigitString,
            Dot,
            DigitString,
            Comma,
            Asterisk,
            LeftParen,
            I,
            DigitString,
            RightParen
        ],
        tokens("10PF77.88,*(I8)")
    );
    assert_eq!(
        vec![
            DigitString,
            P,
            F,
            DigitString,
            Dot,
            DigitString,
            Comma,
            Asterisk,
            LeftParen,
            I,
            DigitString,
            RightParen
        ],
        tokens(" 1 0 P F 7 7 . 8 8 , * ( I 8 ) ")
    );
    assert_eq!(
        vec![
            DigitString,
            P,
            F,
            DigitString,
            Dot,
            DigitString,
            Comma,
            Asterisk,
            LeftParen,
            I,
            DigitString,
            RightParen
        ],
        tokens(" 1 0 P F 7 7 . 8 8 , * ( I 8 ) ")
    );
    assert_eq!(
        vec![
            DT,
            CharLiteralConstant,
            LeftParen,
            Plus,
            DigitString,
            Comma,
            Minus,
            DigitString,
            Comma,
            Plus,
            DigitString,
            Comma,
            Minus,
            DigitString,
            RightParen
        ],
        tokens(" D T ' a s d f ' ( +1, -1, +2, -2 ) ")
    );
}
