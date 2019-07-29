// use crate::{AnalysisEngine, AnalysisOptions};
use crate::index::FileId;
use super::Tokenizer;

#[test]
fn basic() {
    // let options = AnalysisOptions::default();
    // let engine = AnalysisEngine::new(options);

    let tokenizer = Tokenizer::new(FileId(0), r#"
program foo
    if (x .eq. 7) then
        call mysub(8, 9)
    endif
end program foo
    "#);
        
    {
        use super::TokenKind::*;

        assert_eq!(
            vec![
                Program,
                Identifier,
                EOS,
                If,
                LeftParen,
                Identifier,
                EqualsOp,
                DigitString,
                RightParen,
                Then,
                EOS,
                Call,
                Identifier,
                LeftParen,
                DigitString,
                Comma,
                DigitString,
                RightParen,
                EOS,
                EndIf,
                EOS,
                End,
                Program,
                Identifier,
                EOS,
            ],
            tokenizer.map(|x| x.unwrap().kind).collect::<Vec<_>>(),
        );
    }
}