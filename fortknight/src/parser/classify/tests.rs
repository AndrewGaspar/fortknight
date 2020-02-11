use std::cell::RefCell;
/// Tests to verify correct statements are returned by classifier
use std::default::Default;

use crate::error::DiagnosticSink;
use crate::index::FileId;
use crate::intern::StringInterner;
use crate::parser::classify::statements::{
    DefinedIoGenericSpec, DefinedOperator, GenericSpec, ImportStmt, ModuleImportList, ModuleNature,
    Only, ParentIdentifier, Rename, Spanned,
};
use crate::parser::classify::{Classifier, ClassifierArena, StmtKind};
use crate::parser::lex::TokenizerOptions;
use crate::span::Span;

mod assignment_tests;
mod block_tests;
mod format_tests;
mod implicit_tests;
mod label_tests;

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
    let you = interner.intern_name("you".into());
    let bar = interner.intern_name("bar".into());
    let me = interner.intern_name("me".into());

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

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : you",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![Spanned::new(
            Only::GenericOrOnlyUseName(you),
            test_span(34, 37),
        )];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : you, me",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![
            Spanned::new(Only::GenericOrOnlyUseName(you), test_span(34, 37)),
            Spanned::new(Only::GenericOrOnlyUseName(me), test_span(39, 41)),
        ];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : you, me => you",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![
            Spanned::new(Only::GenericOrOnlyUseName(you), test_span(34, 37)),
            Spanned::new(
                Only::Rename(Rename::Name { from: me, to: you }),
                test_span(39, 48),
            ),
        ];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : operator(.foo.)",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![Spanned::new(
            Only::GenericSpec(GenericSpec::Operator(
                DefinedOperator::DefinedUnaryOrBinaryOp(foo),
            )),
            test_span(34, 49),
        )];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : operator(.foo.) => operator(.bar.)",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![Spanned::new(
            Only::Rename(Rename::Operator { from: foo, to: bar }),
            test_span(34, 68),
        )];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : operator(.foo.) => operator(.bar.), me, you",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![
            Spanned::new(
                Only::Rename(Rename::Operator { from: foo, to: bar }),
                test_span(34, 68),
            ),
            Spanned::new(Only::GenericOrOnlyUseName(me), test_span(70, 72)),
            Spanned::new(Only::GenericOrOnlyUseName(you), test_span(74, 77)),
        ];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : operator(.foo.) => operator(.bar.), me, you, assignment(=)",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![
            Spanned::new(
                Only::Rename(Rename::Operator { from: foo, to: bar }),
                test_span(34, 68),
            ),
            Spanned::new(Only::GenericOrOnlyUseName(me), test_span(70, 72)),
            Spanned::new(Only::GenericOrOnlyUseName(you), test_span(74, 77)),
            Spanned::new(
                Only::GenericSpec(GenericSpec::Assignment),
                test_span(79, 92),
            ),
        ];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : read(formatted)",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![Spanned::new(
            Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                DefinedIoGenericSpec::ReadFormatted,
            )),
            test_span(34, 49),
        )];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : read(formatted), read(unformatted), &
                write(formatted), write(unformatted)",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![
            Spanned::new(
                Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                    DefinedIoGenericSpec::ReadFormatted,
                )),
                test_span(34, 49),
            ),
            Spanned::new(
                Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                    DefinedIoGenericSpec::ReadUnformatted,
                )),
                test_span(51, 68),
            ),
            Spanned::new(
                Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                    DefinedIoGenericSpec::WriteFormatted,
                )),
                test_span(88, 104),
            ),
            Spanned::new(
                Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                    DefinedIoGenericSpec::WriteUnformatted,
                )),
                test_span(106, 124),
            ),
        ];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, only : READ(FORMATTED), READ(UNFORMATTED), &
                WRITE(FORMATTED), WRITE(UNFORMATTED)",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_onlys = vec![
            Spanned::new(
                Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                    DefinedIoGenericSpec::ReadFormatted,
                )),
                test_span(34, 49),
            ),
            Spanned::new(
                Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                    DefinedIoGenericSpec::ReadUnformatted,
                )),
                test_span(51, 68),
            ),
            Spanned::new(
                Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                    DefinedIoGenericSpec::WriteFormatted,
                )),
                test_span(88, 104),
            ),
            Spanned::new(
                Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                    DefinedIoGenericSpec::WriteUnformatted,
                )),
                test_span(106, 124),
            ),
        ];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::OnlyList(&expected_onlys)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, me => you",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_renames = vec![Spanned::new(
            Rename::Name { from: me, to: you },
            test_span(27, 36),
        )];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::RenameList(&expected_renames)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, me => you, you => me",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_renames = vec![
            Spanned::new(Rename::Name { from: me, to: you }, test_span(27, 36)),
            Spanned::new(Rename::Name { from: you, to: me }, test_span(38, 47)),
        ];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::RenameList(&expected_renames)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, me => you, you => me, me => you",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_renames = vec![
            Spanned::new(Rename::Name { from: me, to: you }, test_span(27, 36)),
            Spanned::new(Rename::Name { from: you, to: me }, test_span(38, 47)),
            Spanned::new(Rename::Name { from: me, to: you }, test_span(49, 58)),
        ];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::RenameList(&expected_renames)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, me => you, you => me, operator(.foo.) => operator(.bar.)",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_renames = vec![
            Spanned::new(Rename::Name { from: me, to: you }, test_span(27, 36)),
            Spanned::new(Rename::Name { from: you, to: me }, test_span(38, 47)),
            Spanned::new(Rename::Operator { from: foo, to: bar }, test_span(49, 83)),
        ];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::RenameList(&expected_renames)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use, non_intrinsic :: foo, operator(.foo.) => operator(.bar.)",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_renames = vec![Spanned::new(
            Rename::Operator { from: foo, to: bar },
            test_span(27, 61),
        )];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::NonIntrinsic,
                name: foo,
                imports: ModuleImportList::RenameList(&expected_renames)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use foo, operator(.foo.) => operator(.bar.)",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_renames = vec![Spanned::new(
            Rename::Operator { from: foo, to: bar },
            test_span(9, 43),
        )];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::Unspecified,
                name: foo,
                imports: ModuleImportList::RenameList(&expected_renames)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("use foo, me => you", &sink, &mut interner, &arena);

        let expected_renames = vec![Spanned::new(
            Rename::Name { from: me, to: you },
            test_span(9, 18),
        )];

        assert_eq!(
            vec![StmtKind::Use {
                module_nature: ModuleNature::Unspecified,
                name: foo,
                imports: ModuleImportList::RenameList(&expected_renames)
            }],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "use foo, me => you\n\
             use bar, you => me",
            &sink,
            &mut interner,
            &arena,
        );

        let expected_renames = vec![Spanned::new(
            Rename::Name { from: me, to: you },
            test_span(9, 18),
        )];

        let expected_bar_renames = vec![Spanned::new(
            Rename::Name { from: you, to: me },
            test_span(28, 37),
        )];

        assert_eq!(
            vec![
                StmtKind::Use {
                    module_nature: ModuleNature::Unspecified,
                    name: foo,
                    imports: ModuleImportList::RenameList(&expected_renames)
                },
                StmtKind::Use {
                    module_nature: ModuleNature::Unspecified,
                    name: bar,
                    imports: ModuleImportList::RenameList(&expected_bar_renames)
                }
            ],
            get_stmts(&mut c)
        );
    }
}

#[test]
fn imports() {
    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    let you = interner.intern_name("you".into());
    let me = interner.intern_name("me".into());

    {
        let mut c = classifier("import", &sink, &mut interner, &arena);

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::NoSpecifier(&[]))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("import, all", &sink, &mut interner, &arena);

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::AllSpecifier)],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("import, none", &sink, &mut interner, &arena);

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::NoneSpecifier)],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("import, only : me", &sink, &mut interner, &arena);

        let expected_imports = vec![Spanned::new(me, test_span(15, 17))];

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::OnlySpecifier(
                &expected_imports
            ))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("import, only : me, you", &sink, &mut interner, &arena);

        let expected_imports = vec![
            Spanned::new(me, test_span(15, 17)),
            Spanned::new(you, test_span(19, 22)),
        ];

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::OnlySpecifier(
                &expected_imports
            ))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("import, only : me", &sink, &mut interner, &arena);

        let expected_imports = vec![Spanned::new(me, test_span(15, 17))];

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::OnlySpecifier(
                &expected_imports
            ))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("import me", &sink, &mut interner, &arena);

        let expected_imports = vec![Spanned::new(me, test_span(7, 9))];

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::NoSpecifier(&expected_imports))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("import me, you", &sink, &mut interner, &arena);

        let expected_imports = vec![
            Spanned::new(me, test_span(7, 9)),
            Spanned::new(you, test_span(11, 14)),
        ];

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::NoSpecifier(&expected_imports))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("import :: me", &sink, &mut interner, &arena);

        let expected_imports = vec![Spanned::new(me, test_span(10, 12))];

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::NoSpecifier(&expected_imports))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("import :: me, you", &sink, &mut interner, &arena);

        let expected_imports = vec![
            Spanned::new(me, test_span(10, 12)),
            Spanned::new(you, test_span(14, 17)),
        ];

        assert_eq!(
            vec![StmtKind::Import(ImportStmt::NoSpecifier(&expected_imports))],
            get_stmts(&mut c)
        );
    }
}

pub(crate) fn classifier<'input, 'arena>(
    text: &'input str,
    sink: &'input RefCell<DiagnosticSink>,
    interner: &'input mut StringInterner,
    arena: &'arena ClassifierArena<'arena>,
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

pub(crate) fn get_stmts<'a>(classifier: &'_ mut Classifier<'_, 'a>) -> Vec<StmtKind<'a>> {
    let mut stmts = vec![];

    while let Some(stmt) = classifier.next_stmt() {
        stmts.push(stmt.stmt.kind);
    }

    stmts
}

pub(crate) fn test_span(start: u32, end: u32) -> Span {
    Span {
        file_id: FileId(0),
        start,
        end,
    }
}
