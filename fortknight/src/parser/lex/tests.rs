use std::cell::RefCell;
use std::default::Default;

use super::{
    token::{KeywordTokenKind, Letter, TokenKind},
    Tokenizer, TokenizerOptions,
};
use crate::error::DiagnosticSink;
use crate::index::FileId;

mod intrinsics;
mod preprocessor;
mod strings;

// pub fn get_tokens(text: &str) -> Vec<Result<TokenKind, ParserErrorCode>> {
//     let sink = DiagnosticSink{};
//     let tokenizer = Tokenizer::new(FileId(0), text, &sink);

//     tokenizer
//         .map(|x| match x {
//             Ok(t) => Ok(t.kind),
//             Err(e) => Err(e.0[0].code),
//         })
//         .collect()
// }

pub fn get_tokens_unwrap(text: &str) -> Vec<TokenKind> {
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let tokenizer = Tokenizer::new(&TokenizerOptions::default(), FileId(0), text, &sink);

    tokenizer.map(|x| x.kind).collect()
}

#[test]
fn basic() {
    use super::Letter::*;
    use KeywordTokenKind::*;
    use TokenKind::*;

    assert_eq!(
        vec![
            Keyword(Program),
            TokenKind::Name,
            NewLine,
            Keyword(If),
            LeftParen,
            TokenKind::Letter(X),
            EqualsOp,
            DigitString,
            RightParen,
            Keyword(Then),
            NewLine,
            Keyword(Call),
            TokenKind::Name,
            LeftParen,
            DigitString,
            Comma,
            DigitString,
            RightParen,
            NewLine,
            Keyword(EndIf),
            NewLine,
            Keyword(End),
            Keyword(Program),
            TokenKind::Name,
        ],
        get_tokens_unwrap(
            "\
PROGRAM foo
    if (x .eq. 7) then
        call mysub(8, 9)
    endif
end program foo",
        ),
    );
}

#[test]
fn with_continuations() {
    use super::KeywordTokenKind::*;
    use super::Letter::*;
    use super::TokenKind::{self, *};

    assert_eq!(
        vec![
            Keyword(Program),
            TokenKind::Name,
            NewLine,
            Keyword(If),
            LeftParen,
            TokenKind::Letter(X),
            EqualsOp,
            DigitString,
            RightParen,
            Keyword(Then),
            NewLine,
            Keyword(Call),
            TokenKind::Name,
            LeftParen,
            DigitString,
            Comma,
            DigitString,
            RightParen,
            NewLine,
            Keyword(EndIf),
            NewLine,
            Keyword(End),
            Keyword(Program),
            TokenKind::Name,
        ],
        get_tokens_unwrap(
            "\
PROG&
&RAM foo
    i&
    &f (x .eq. 7) Th&
    &e&
    &N
        call &
        mys&
        &ub(8, 9)
    endif
e&
&n&
&d program foo",
        ),
    );
}

#[test]
fn bad_token() {
    assert_eq!(
        vec![
            TokenKind::Letter(Letter::X),
            TokenKind::Unknown,
            // Result::Err(ParserErrorCode::UnrecognizedToken),
            TokenKind::Letter(Letter::Y),
        ],
        get_tokens_unwrap("x @ y"),
        // get_tokens("x @ y"),
    );
}

#[test]
fn commentary() {
    use super::KeywordTokenKind::*;
    use super::Letter::*;
    use TokenKind::*;

    assert_eq!(
        vec![
            Keyword(Program),
            TokenKind::Name,
            NewLine,
            Commentary,
            Keyword(If),
            LeftParen,
            TokenKind::Letter(X),
            EqualsOp,
            DigitString,
            RightParen,
            Keyword(Then),
            Commentary,
            Keyword(Call),
            TokenKind::Name,
            LeftParen,
            DigitString,
            Comma,
            DigitString,
            RightParen,
            NewLine,
            Keyword(EndIf),
            NewLine,
            Keyword(End),
            Keyword(Program),
            TokenKind::Name,
        ],
        get_tokens_unwrap(
            "\
PROGRAM foo
    ! This is a comment
    if (x .eq. 7) then ! And another one
        call mysub(8, 9)
    endif
end program foo"
        ),
    );
}

#[test]
fn end_commentary() {
    assert_eq!(
        vec![TokenKind::Commentary],
        get_tokens_unwrap("! some comment at the end"),
    );
}

#[test]
fn continuation() {
    assert_eq!(
        vec![TokenKind::Colon, TokenKind::Colon],
        get_tokens_unwrap(
            ":&
:"
        )
    );

    assert_eq!(
        vec![TokenKind::Colon, TokenKind::Colon],
        get_tokens_unwrap(
            ":&
    :"
        )
    );

    assert_eq!(
        vec![TokenKind::ColonColon],
        get_tokens_unwrap(
            ":&
            &:"
        )
    );
}

#[test]
fn unknown_pound() {
    assert_eq!(vec![TokenKind::Unknown], get_tokens_unwrap("#"));
}

#[test]
fn unknown_c_block_comment() {
    use TokenKind::{Name, Slash, Star};

    assert_eq!(
        vec![Slash, Star, Name, Star, Slash,],
        get_tokens_unwrap("/* asdf */")
    );
}
#[test]
fn dots_vs_operators() {
    assert_eq!(vec![TokenKind::EqualsOp], get_tokens_unwrap(".eq."));

    assert_eq!(vec![TokenKind::Dot], get_tokens_unwrap("."));
    assert_eq!(
        vec![TokenKind::Dot, TokenKind::Dot],
        get_tokens_unwrap(
            ".&
."
        )
    );

    assert_eq!(
        // vec![Err(ParserErrorCode::UnterminatedOperator)],
        vec![TokenKind::Unknown],
        get_tokens_unwrap(".eq"),
    );

    assert_eq!(
        vec![TokenKind::RealLiteralConstant],
        get_tokens_unwrap(".8")
    );
}

#[test]
fn real_literal_constant() {
    use TokenKind::{Plus, RealLiteralConstant};

    assert_eq!(vec![RealLiteralConstant], get_tokens_unwrap("1.2"));
    assert_eq!(vec![RealLiteralConstant], get_tokens_unwrap("100.278"));
    assert_eq!(vec![RealLiteralConstant], get_tokens_unwrap("1.2e+7"));
    assert_eq!(vec![RealLiteralConstant], get_tokens_unwrap("1.2d+7"));
    assert_eq!(vec![RealLiteralConstant], get_tokens_unwrap("1.2d-7"));
    assert_eq!(vec![RealLiteralConstant], get_tokens_unwrap("1.2D7"));
    assert_eq!(vec![RealLiteralConstant], get_tokens_unwrap("1D7"));
    assert_eq!(vec![RealLiteralConstant], get_tokens_unwrap(".1D7"));
    assert_eq!(vec![RealLiteralConstant], get_tokens_unwrap("1.E-78"));

    assert_eq!(
        vec![Plus, RealLiteralConstant],
        get_tokens_unwrap("+19.52E-78")
    );

    // assert_eq!(Err(MissingExponent), get_tokens("1.E--78")[0]);
    // assert_eq!(Err(MissingExponent), get_tokens(".0E")[0]);
    // assert_eq!(Err(MissingExponent), get_tokens(".0Ea")[0]);
    // assert_eq!(Err(MissingExponent), get_tokens(".0E+-9")[0]);
    // assert_eq!(Err(MissingExponent), get_tokens("100.0d")[0]);

    assert_eq!(TokenKind::Unknown, get_tokens_unwrap("1.E--78")[0]);
    assert_eq!(TokenKind::Unknown, get_tokens_unwrap(".0E")[0]);
    assert_eq!(TokenKind::Unknown, get_tokens_unwrap(".0Ea")[0]);
    assert_eq!(TokenKind::Unknown, get_tokens_unwrap(".0E+-9")[0]);
    assert_eq!(TokenKind::Unknown, get_tokens_unwrap("100.0d")[0]);
}
