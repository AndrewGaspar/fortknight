//! Tests to verify correct implicit-* statements are returned by classifier

use crate::error::DiagnosticSink;
use crate::intern::StringInterner;
use crate::parser::classify::statements::{
    DeclarationTypeSpec, DerivedTypeSpec, ImplicitSpec, ImplicitStmt, IntegerTypeSpec,
    IntrinsicTypeSpec, LetterSpec, Spanned,
};
use crate::parser::classify::{ClassifierArena, StmtKind};
use crate::parser::lex::Letter;
use std::cell::RefCell;

use super::{classifier, get_stmts, test_span};

#[test]
fn implicit_none() {
    use StmtKind::*;

    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    {
        let mut c = classifier("implicit none", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Implicit(ImplicitStmt::NoneSpecList {
                has_external: false,
                has_type: false,
            })],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("implicit none()", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Implicit(ImplicitStmt::NoneSpecList {
                has_external: false,
                has_type: false,
            })],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("implicit none(external)", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Implicit(ImplicitStmt::NoneSpecList {
                has_external: true,
                has_type: false,
            })],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "implicit none(external, type)",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![Implicit(ImplicitStmt::NoneSpecList {
                has_external: true,
                has_type: true,
            })],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "IMPLICIT NONE(EXTERNAL, TYPE)",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![Implicit(ImplicitStmt::NoneSpecList {
                has_external: true,
                has_type: true,
            })],
            get_stmts(&mut c)
        );
    }

    {
        // this will report an error, but we should still get an implicit statement out of it
        let mut c = classifier(
            "implicit none(external, type, type)",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![Implicit(ImplicitStmt::NoneSpecList {
                has_external: true,
                has_type: true,
            })],
            get_stmts(&mut c)
        );
    }
}

#[test]
fn implicit_spec() {
    use StmtKind::*;

    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    let foo = interner.intern_name("foo".into());

    {
        let mut c = classifier("implicit integer(a-z)", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Implicit(ImplicitStmt::SpecList(&[Spanned::new(
                ImplicitSpec {
                    declaration_type_spec: DeclarationTypeSpec::Intrinsic(
                        IntrinsicTypeSpec::Integer(IntegerTypeSpec(None))
                    ),
                    letter_spec_list: &[LetterSpec {
                        start: Letter::A,
                        end: Some(Letter::Z)
                    }]
                },
                test_span(9, 21)
            )]))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("implicit type(integer)(a-z)", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Implicit(ImplicitStmt::SpecList(&[Spanned::new(
                ImplicitSpec {
                    declaration_type_spec: DeclarationTypeSpec::TypeIntrinsic(
                        IntrinsicTypeSpec::Integer(IntegerTypeSpec(None))
                    ),
                    letter_spec_list: &[LetterSpec {
                        start: Letter::A,
                        end: Some(Letter::Z)
                    }]
                },
                test_span(9, 27)
            )]))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "implicit type(integer)(a-z), real(e-f), logical(q)",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![Implicit(ImplicitStmt::SpecList(&[
                Spanned::new(
                    ImplicitSpec {
                        declaration_type_spec: DeclarationTypeSpec::TypeIntrinsic(
                            IntrinsicTypeSpec::Integer(IntegerTypeSpec(None))
                        ),
                        letter_spec_list: &[LetterSpec {
                            start: Letter::A,
                            end: Some(Letter::Z)
                        }]
                    },
                    test_span(9, 27)
                ),
                Spanned::new(
                    ImplicitSpec {
                        declaration_type_spec: DeclarationTypeSpec::Intrinsic(
                            IntrinsicTypeSpec::Real(None)
                        ),
                        letter_spec_list: &[LetterSpec {
                            start: Letter::E,
                            end: Some(Letter::F)
                        }]
                    },
                    test_span(29, 38)
                ),
                Spanned::new(
                    ImplicitSpec {
                        declaration_type_spec: DeclarationTypeSpec::Intrinsic(
                            IntrinsicTypeSpec::Logical(None)
                        ),
                        letter_spec_list: &[LetterSpec {
                            start: Letter::Q,
                            end: None
                        }]
                    },
                    test_span(40, 50)
                ),
            ]))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("implicit type(foo)(f)", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Implicit(ImplicitStmt::SpecList(&[Spanned::new(
                ImplicitSpec {
                    declaration_type_spec: DeclarationTypeSpec::TypeDerived(DerivedTypeSpec {
                        name: foo,
                        spec_list: &[],
                    }),
                    letter_spec_list: &[LetterSpec {
                        start: Letter::F,
                        end: None
                    }]
                },
                test_span(9, 21)
            ),]))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "implicit type(foo)(f), class(*)(x)",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![Implicit(ImplicitStmt::SpecList(&[
                Spanned::new(
                    ImplicitSpec {
                        declaration_type_spec: DeclarationTypeSpec::TypeDerived(DerivedTypeSpec {
                            name: foo,
                            spec_list: &[],
                        }),
                        letter_spec_list: &[LetterSpec {
                            start: Letter::F,
                            end: None
                        }]
                    },
                    test_span(9, 21)
                ),
                Spanned::new(
                    ImplicitSpec {
                        declaration_type_spec: DeclarationTypeSpec::ClassWildcard,
                        letter_spec_list: &[LetterSpec {
                            start: Letter::X,
                            end: None
                        }]
                    },
                    test_span(23, 34)
                ),
            ]))],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "implicit type(foo)(f), class(*)(x), type(*)(y-z)",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![Implicit(ImplicitStmt::SpecList(&[
                Spanned::new(
                    ImplicitSpec {
                        declaration_type_spec: DeclarationTypeSpec::TypeDerived(DerivedTypeSpec {
                            name: foo,
                            spec_list: &[],
                        }),
                        letter_spec_list: &[LetterSpec {
                            start: Letter::F,
                            end: None
                        }]
                    },
                    test_span(9, 21)
                ),
                Spanned::new(
                    ImplicitSpec {
                        declaration_type_spec: DeclarationTypeSpec::ClassWildcard,
                        letter_spec_list: &[LetterSpec {
                            start: Letter::X,
                            end: None
                        }]
                    },
                    test_span(23, 34)
                ),
                Spanned::new(
                    ImplicitSpec {
                        declaration_type_spec: DeclarationTypeSpec::TypeWildcard,
                        letter_spec_list: &[LetterSpec {
                            start: Letter::Y,
                            end: Some(Letter::Z)
                        }]
                    },
                    test_span(36, 48)
                ),
            ]))],
            get_stmts(&mut c)
        );
    }
}
