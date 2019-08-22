use std::cell::RefCell;
/// Tests to verify correct statements are returned by classifier
use std::default::Default;

use crate::error::DiagnosticSink;
use crate::index::FileId;
use crate::intern::StringInterner;
use crate::parser::classify::statements::{
    ModuleImportList, ModuleNature, ParentIdentifier, Spanned,
};
use crate::parser::classify::{Classifier, ClassifierArena, StmtKind};
use crate::parser::lex::TokenizerOptions;
use crate::span::Span;

#[test]
fn program() {
    use StmtKind::*;

    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();
    let mut c = classifier("program; end program", &sink, &mut interner, &arena);

    assert_eq!(
        vec![Program { name: None }, EndProgram { name: None }],
        get_stmts(&mut c)
    );
}

#[test]
fn module() {
    use StmtKind::*;

    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    let foo = interner.intern_name("foo".into());

    {
        let mut c = classifier("module foo; end module", &sink, &mut interner, &arena);

        assert_eq!(
            vec![
                Module {
                    name: Spanned::new(foo, test_span(7, 10))
                },
                EndModule { name: None }
            ],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("module foo; endmodule", &sink, &mut interner, &arena);

        assert_eq!(
            vec![
                Module {
                    name: Spanned::new(foo, test_span(7, 10))
                },
                EndModule { name: None }
            ],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("module foo; endmodule foo", &sink, &mut interner, &arena);

        assert_eq!(
            vec![
                Module {
                    name: Spanned::new(foo, test_span(7, 10))
                },
                EndModule {
                    name: Some(Spanned::new(foo, test_span(22, 25)))
                }
            ],
            get_stmts(&mut c)
        );
    }
}

#[test]
fn bare_module() {
    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    {
        let mut c = classifier("module;end module", &sink, &mut interner, &arena);

        assert_eq!(
            vec![StmtKind::Unclassifiable, StmtKind::EndModule { name: None }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("module", &sink, &mut interner, &arena);

        assert_eq!(vec![StmtKind::Unclassifiable], get_stmts(&mut c));
    }
}

#[test]
fn submodule() {
    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    let parent = interner.intern_name("parent".into());
    let mysub = interner.intern_name("mysub".into());
    let another_sub = interner.intern_name("another_sub".into());

    {
        let mut c = classifier(
            "submodule (parent) mysub; end submodule",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![
                StmtKind::Submodule {
                    parent_identifier: ParentIdentifier {
                        ancestor_module_name: parent,
                        parent_submodule_name: None
                    },
                    name: Spanned::new(mysub, test_span(19, 24))
                },
                StmtKind::EndSubmodule { name: None }
            ],
            get_stmts(&mut c)
        );
    }
    {
        let mut c = classifier(
            "submodule (parent: another_sub) mysub; end submodule",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![
                StmtKind::Submodule {
                    parent_identifier: ParentIdentifier {
                        ancestor_module_name: parent,
                        parent_submodule_name: Some(another_sub)
                    },
                    name: Spanned::new(mysub, test_span(32, 37))
                },
                StmtKind::EndSubmodule { name: None }
            ],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "submodule (parent: another_sub) mysub; endsubmodule mysub",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![
                StmtKind::Submodule {
                    parent_identifier: ParentIdentifier {
                        ancestor_module_name: parent,
                        parent_submodule_name: Some(another_sub)
                    },
                    name: Spanned::new(mysub, test_span(32, 37))
                },
                StmtKind::EndSubmodule {
                    name: Some(Spanned::new(mysub, test_span(52, 57)))
                }
            ],
            get_stmts(&mut c)
        );
    }
}

#[test]
fn bare_submodule() {
    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();
    let mut c = classifier("submodule", &sink, &mut interner, &arena);

    assert_eq!(vec![StmtKind::Unclassifiable], get_stmts(&mut c));
}

#[test]
fn use_statement() {
    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    let foo = interner.intern_name("foo".into());

    {
        let mut c = classifier("use foo", &sink, &mut interner, &arena);

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::Unspecified,
                name: foo,
                imports: ModuleImportList::Unspecified
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("use, non_intrinsic foo", &sink, &mut interner, &arena);

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::Unspecified
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("use, non_intrinsic :: foo", &sink, &mut interner, &arena);

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::Unspecified
            }],
            get_stmts(&mut c)
        );
    }
}

fn classifier<'input, 'arena>(
    text: &'input str,
    sink: &'input RefCell<DiagnosticSink>,
    interner: &'input mut StringInterner,
    arena: &'arena ClassifierArena,
) -> Classifier<'input, 'arena> {
    Classifier::new(
        &TokenizerOptions::default(),
        FileId(0),
        text,
        sink,
        interner,
        arena,
    )
}

fn get_stmts<'a>(classifier: &'_ mut Classifier<'_, 'a>) -> Vec<StmtKind<'a>> {
    let mut stmts = vec![];

    while let Some(stmt) = classifier.next_stmt() {
        stmts.push(stmt.kind);
    }

    stmts
}

fn test_span(start: u32, end: u32) -> Span {
    Span {
        file_id: FileId(0),
        start,
        end,
    }
}
