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
        use super::TokenKind::*;

        assert_eq!(
            vec![
                Program,
                Identifier,
                NewLine,
                If,
                LeftParen,
                Identifier,
                EqualsOp,
                DigitString,
                RightParen,
                Then,
                NewLine,
                Call,
                Identifier,
                LeftParen,
                DigitString,
                Comma,
                DigitString,
                RightParen,
                NewLine,
                EndIf,
                NewLine,
                End,
                Program,
                Identifier,
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
        use super::TokenKind::*;

        assert_eq!(
            vec![
                Program,
                Identifier,
                NewLine,
                If,
                LeftParen,
                Identifier,
                EqualsOp,
                DigitString,
                RightParen,
                Then,
                NewLine,
                Call,
                Identifier,
                LeftParen,
                DigitString,
                Comma,
                DigitString,
                RightParen,
                NewLine,
                EndIf,
                NewLine,
                End,
                Program,
                Identifier,
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

                vec![
                    Ok(Identifier),
                    Result::Err(UnrecognizedToken),
                    Ok(Identifier),
                ]
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
