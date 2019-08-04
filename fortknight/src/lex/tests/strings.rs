use crate::lex::{tests::get_tokens_unwrap, token::TokenKind::CharLiteralConstant};

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
