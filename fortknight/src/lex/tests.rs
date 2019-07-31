// use crate::{AnalysisEngine, AnalysisOptions};
use super::Tokenizer;
use crate::index::FileId;

#[test]
fn basic() {
    let tokenizer = Tokenizer::new(
        FileId(0),
        "\
PROGRAM foo
    if (x .eq. 7) then
        call mysub(8, 9)
    endif
end program foo",
    );
    {
        use super::KeywordTokenKind::*;
        use super::TokenKind::{self, *};

        assert_eq!(
            vec![
                Keyword(Program),
                TokenKind::Name,
                NewLine,
                Keyword(If),
                LeftParen,
                TokenKind::Name,
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
            tokenizer.map(|x| x.unwrap().kind).collect::<Vec<_>>(),
        );
    }
}

#[test]
fn with_continuations() {
    let tokenizer = Tokenizer::new(
        FileId(0),
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
    );
    {
        use super::KeywordTokenKind::*;
        use super::TokenKind::{self, *};

        assert_eq!(
            vec![
                Keyword(Program),
                TokenKind::Name,
                NewLine,
                Keyword(If),
                LeftParen,
                TokenKind::Name,
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
            tokenizer.map(|x| x.unwrap().kind).collect::<Vec<_>>(),
        );
    }
}

#[test]
fn bad_token() {
    let tokenizer = Tokenizer::new(FileId(0), "x @ y");
    {
        assert_eq!(
            {
                use super::ErrorCode::*;
                use super::TokenKind::*;

                vec![Ok(Name), Result::Err(UnrecognizedToken), Ok(Name)]
            },
            tokenizer
                .map(|x| {
                    match x {
                        Ok(t) => Ok(t.kind),
                        Err(e) => Err(e.code),
                    }
                })
                .collect::<Vec<_>>(),
        );
    }
}

#[test]
fn commentary() {
    let tokenizer = Tokenizer::new(
        FileId(0),
        "\
PROGRAM foo
    ! This is a comment
    if (x .eq. 7) then ! And another one
        call mysub(8, 9)
    endif
end program foo",
    );
    {
        use super::KeywordTokenKind::*;
        use super::TokenKind::{self, *};

        assert_eq!(
            vec![
                Keyword(Program),
                TokenKind::Name,
                NewLine,
                Commentary,
                NewLine,
                Keyword(If),
                LeftParen,
                TokenKind::Name,
                EqualsOp,
                DigitString,
                RightParen,
                Keyword(Then),
                Commentary,
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
            tokenizer.map(|x| x.unwrap().kind).collect::<Vec<_>>(),
        );
    }
}

#[test]
fn end_commentary() {
    let tokenizer = Tokenizer::new(FileId(0), "! some comment at the end");
    {
        use super::TokenKind::Commentary;

        assert_eq!(
            vec![Commentary],
            tokenizer.map(|x| x.unwrap().kind).collect::<Vec<_>>(),
        );
    }
}
