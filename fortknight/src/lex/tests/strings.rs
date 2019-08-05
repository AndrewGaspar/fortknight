use crate::index::FileId;
use crate::lex::Tokenizer;
use crate::lex::{tests::get_tokens_unwrap, token::TokenKind::CharLiteralConstant, ErrorCode::*};

#[test]
fn empty_string() {
    assert_eq!(vec![CharLiteralConstant], get_tokens_unwrap("''"));

    assert_eq!(vec![CharLiteralConstant], get_tokens_unwrap("\"\""));
}

#[test]
fn double_quote() {
    assert_eq!(
        vec![CharLiteralConstant],
        get_tokens_unwrap(r#""Lorem ipsum!!!""#)
    );
}

#[test]
fn single_quote() {
    assert_eq!(
        vec![CharLiteralConstant],
        get_tokens_unwrap("'Lorem impsum!!!'")
    );
}

#[test]
fn escaped_quote() {
    assert_eq!(
        vec![CharLiteralConstant],
        get_tokens_unwrap("'Lorem ''impsum!!!'")
    );
}

#[test]
fn continuation() {
    assert_eq!(
        vec![CharLiteralConstant],
        get_tokens_unwrap(
            "'hey now &
        & you''re a rock star, &
        & get your game on, &
        & go play!'"
        )
    );
}

#[test]
fn discontinued() {
    assert_eq!(
        vec![DiscontinuedCharacterContext],
        Tokenizer::new(
            FileId(0),
            "'asdf &
         foo'"
        )
        .flat_map(|t| t.unwrap_err().0)
        .map(|e| e.code)
        .collect::<Vec<_>>()
    );
}

#[test]
fn multiple_errors() {
    assert_eq!(
        vec![DiscontinuedCharacterContext, UnterminatedStringLiteral],
        Tokenizer::new(
            FileId(0),
            "'asdf &
         foo"
        )
        .flat_map(|t| t.unwrap_err().0)
        .map(|e| e.code)
        .collect::<Vec<_>>()
    );
}
