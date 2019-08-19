/// Tests tokenization of preprocessor elements
use crate::error::DiagnosticSink;
use crate::index::FileId;
use crate::parser::lex::{TokenKind, Tokenizer, TokenizerOptions};

pub fn get_tokens_unwrap(text: &str) -> Vec<TokenKind> {
    let mut sink = DiagnosticSink::Raw(Box::new(std::io::sink()));
    let tokenizer = Tokenizer::new(
        &TokenizerOptions {
            tokenize_preprocessor: true,
        },
        FileId(0),
        text,
        &mut sink,
    );

    tokenizer.map(|x| x.kind).collect()
}

#[test]
fn pound_gnu() {
    assert_eq!(vec![TokenKind::Pound], get_tokens_unwrap("#"));
}

#[test]
fn c_commentary_gnu() {
    assert_eq!(
        vec![TokenKind::CBlockCommentary],
        get_tokens_unwrap("/* asdf */")
    );
}

#[test]
fn c_commentary_with_ampersand_gnu() {
    assert_eq!(
        vec![TokenKind::CBlockCommentary],
        get_tokens_unwrap("/* asdf & */")
    );
}
