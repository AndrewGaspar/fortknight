use std::cell::RefCell;
/// Tests to verify correct statements are returned by classifier
use std::default::Default;

use crate::error::DiagnosticSink;
use crate::index::FileId;
use crate::intern::StringInterner;
use crate::parser::classify::statements::{ParentIdentifier, Spanned};
use crate::parser::classify::{Classifier, StmtKind};
use crate::parser::lex::TokenizerOptions;
use crate::span::Span;

#[test]
fn program() {
    use StmtKind::*;

    assert_eq!(
        vec![Program { name: None }, EndProgram { name: None }],
        get_stmts_unwrap("program; end program")
    );
}

#[test]
fn module() {
    use StmtKind::*;

    let mut interner = StringInterner::new();

    assert_eq!(
        vec![
            Module {
                name: Spanned::new(interner.intern_name("foo".into()), test_span(7, 10))
            },
            EndModule { name: None }
        ],
        get_stmts_with_interner_unwrap("module foo; end module", &mut interner)
    );

    assert_eq!(
        vec![
            Module {
                name: Spanned::new(interner.intern_name("foo".into()), test_span(7, 10))
            },
            EndModule { name: None }
        ],
        get_stmts_with_interner_unwrap("module foo; endmodule", &mut interner)
    );

    assert_eq!(
        vec![
            Module {
                name: Spanned::new(interner.intern_name("foo".into()), test_span(7, 10))
            },
            EndModule {
                name: Some(Spanned::new(
                    interner.intern_name("foo".into()),
                    test_span(22, 25)
                ))
            }
        ],
        get_stmts_with_interner_unwrap("module foo; endmodule foo", &mut interner)
    );
}

#[test]
fn bare_module() {
    assert_eq!(
        vec![StmtKind::Unclassifiable, StmtKind::EndModule { name: None }],
        get_stmts_unwrap("module;end module")
    );

    assert_eq!(vec![StmtKind::Unclassifiable], get_stmts_unwrap("module"));
}

#[test]
fn submodule() {
    let mut interner = StringInterner::new();

    assert_eq!(
        vec![
            StmtKind::Submodule {
                parent_identifier: ParentIdentifier {
                    ancestor_module_name: interner.intern_name("parent".into()),
                    parent_submodule_name: None
                },
                name: Spanned::new(interner.intern_name("mysub".into()), test_span(19, 24))
            },
            StmtKind::EndSubmodule { name: None }
        ],
        get_stmts_with_interner_unwrap("submodule (parent) mysub; end submodule", &mut interner)
    );

    assert_eq!(
        vec![
            StmtKind::Submodule {
                parent_identifier: ParentIdentifier {
                    ancestor_module_name: interner.intern_name("parent".into()),
                    parent_submodule_name: Some(interner.intern_name("another_sub".into()))
                },
                name: Spanned::new(interner.intern_name("mysub".into()), test_span(32, 37))
            },
            StmtKind::EndSubmodule { name: None }
        ],
        get_stmts_with_interner_unwrap(
            "submodule (parent: another_sub) mysub; end submodule",
            &mut interner
        )
    );

    assert_eq!(
        vec![
            StmtKind::Submodule {
                parent_identifier: ParentIdentifier {
                    ancestor_module_name: interner.intern_name("parent".into()),
                    parent_submodule_name: Some(interner.intern_name("another_sub".into()))
                },
                name: Spanned::new(interner.intern_name("mysub".into()), test_span(32, 37))
            },
            StmtKind::EndSubmodule {
                name: Some(Spanned::new(
                    interner.intern_name("mysub".into()),
                    test_span(52, 57)
                ))
            }
        ],
        get_stmts_with_interner_unwrap(
            "submodule (parent: another_sub) mysub; endsubmodule mysub",
            &mut interner
        )
    );
}

#[test]
fn bare_submodule() {
    assert_eq!(
        vec![StmtKind::Unclassifiable],
        get_stmts_unwrap("submodule")
    );
}

fn get_stmts_unwrap(text: &str) -> Vec<StmtKind> {
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let mut interner = StringInterner::new();

    let classifier = Classifier::new(
        &TokenizerOptions::default(),
        FileId(0),
        text,
        &sink,
        &mut interner,
    );

    classifier.map(|s| s.kind).collect()
}

fn get_stmts_with_interner_unwrap(text: &str, interner: &mut StringInterner) -> Vec<StmtKind> {
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));

    let classifier = Classifier::new(
        &TokenizerOptions::default(),
        FileId(0),
        text,
        &sink,
        interner,
    );

    classifier.map(|s| s.kind).collect()
}

fn test_span(start: u32, end: u32) -> Span {
    Span {
        file_id: FileId(0),
        start,
        end,
    }
}
