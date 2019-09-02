//! Tests to verify correct block-* statements are returned by classifier

use crate::error::DiagnosticSink;
use crate::intern::StringInterner;
use crate::parser::classify::statements::Spanned;
use crate::parser::classify::{ClassifierArena, StmtKind};
use std::cell::RefCell;

use super::{classifier, get_stmts, test_span};

#[test]
fn block_data() {
    use StmtKind::*;

    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    let foo = interner.intern_name("foo".into());

    {
        let mut c = classifier("block data; end block data", &sink, &mut interner, &arena);

        assert_eq!(
            vec![BlockData { name: None }, EndBlockData { name: None }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("blockdata; endblockdata", &sink, &mut interner, &arena);

        assert_eq!(
            vec![BlockData { name: None }, EndBlockData { name: None }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("blockdata; endblock data", &sink, &mut interner, &arena);

        assert_eq!(
            vec![BlockData { name: None }, EndBlockData { name: None }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("blockdata; end blockdata", &sink, &mut interner, &arena);

        assert_eq!(
            vec![BlockData { name: None }, EndBlockData { name: None }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "block data foo; end block data foo",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![
                BlockData {
                    name: Some(Spanned::new(foo, test_span(11, 14)))
                },
                EndBlockData {
                    name: Some(Spanned::new(foo, test_span(31, 34)))
                }
            ],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "blockdata foo; end blockdata foo",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![
                BlockData {
                    name: Some(Spanned::new(foo, test_span(10, 13)))
                },
                EndBlockData {
                    name: Some(Spanned::new(foo, test_span(29, 32)))
                }
            ],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "blockdata foo; endblockdata foo",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![
                BlockData {
                    name: Some(Spanned::new(foo, test_span(10, 13)))
                },
                EndBlockData {
                    name: Some(Spanned::new(foo, test_span(28, 31)))
                }
            ],
            get_stmts(&mut c)
        );
    }
}
