//! Tests to verify correct assignment statements are returned by classifier

use crate::error::DiagnosticSink;
use crate::intern::StringInterner;
use crate::num::Uint;
use crate::parser::classify::statements::{
    AssignmentStmt, CharLiteralConstant, Designator, DigitString, Exponent, ExponentLetter,
    ExponentPart, Expr, IntLiteralConstant, KindParam, LiteralConstant, LogicalLiteralConstant,
    PrimaryRaw, RealLiteralConstant, Sign, SignedDigitString, Significand, Spanned, Variable,
};
use crate::parser::classify::{ClassifierArena, StmtKind};
use std::cell::RefCell;

use super::{classifier, get_stmts, test_span};

#[test]
fn assignment() {
    use StmtKind::*;

    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    let foo = interner.intern_name("foo".into());
    let bar = interner.intern_name("bar".into());

    {
        let mut c = classifier("foo = 88", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::IntLiteralConstant(IntLiteralConstant {
                            digit_string: Uint::Small(88),
                            kind_param: None
                        })
                    )),
                    span: test_span(6, 8)
                }
            })],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("foo = 88;bar=7.5E-10", &sink, &mut interner, &arena);

        assert_eq!(
            vec![
                Assignment(AssignmentStmt {
                    variable: Spanned {
                        val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                        span: test_span(0, 3)
                    },
                    expr: Spanned {
                        val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                            LiteralConstant::IntLiteralConstant(IntLiteralConstant {
                                digit_string: Uint::Small(88),
                                kind_param: None
                            })
                        )),
                        span: test_span(6, 8)
                    }
                }),
                Assignment(AssignmentStmt {
                    variable: Spanned {
                        val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(bar)),
                        span: test_span(9, 12)
                    },
                    expr: Spanned {
                        val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                            LiteralConstant::RealLiteralConstant(RealLiteralConstant {
                                significand: Significand {
                                    integer: Some(Uint::Small(7)),
                                    decimal: Some(Uint::Small(5)),
                                },
                                exponent_part: Some(ExponentPart {
                                    letter: ExponentLetter::E,
                                    exponent: Exponent(SignedDigitString {
                                        sign: Some(Sign::Minus),
                                        digit_string: DigitString(Uint::Small(10))
                                    })
                                }),
                                kind_param: None
                            })
                        )),
                        span: test_span(13, 20)
                    }
                })
            ],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("foo = .true.", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::LogicalLiteralConstant(LogicalLiteralConstant {
                            value: true,
                            kind_param: None
                        })
                    )),
                    span: test_span(6, 12)
                }
            })],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("foo = .false.", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::LogicalLiteralConstant(LogicalLiteralConstant {
                            value: false,
                            kind_param: None
                        })
                    )),
                    span: test_span(6, 13)
                }
            })],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("foo = .true._bar", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::LogicalLiteralConstant(LogicalLiteralConstant {
                            value: true,
                            kind_param: Some(KindParam::ScalarIntConstantName(bar))
                        })
                    )),
                    span: test_span(6, 16)
                }
            })],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("foo = 'howdy, partner!'", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::CharLiteralConstant(CharLiteralConstant {
                            kind_param: None,
                            string: "howdy, partner!",
                        })
                    )),
                    span: test_span(6, 23)
                }
            }),],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("foo = 'howdy, ''partner''!'", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::CharLiteralConstant(CharLiteralConstant {
                            kind_param: None,
                            string: "howdy, 'partner'!",
                        })
                    )),
                    span: test_span(6, 27)
                }
            }),],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("foo = 'howdy, \"partner\"!'", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::CharLiteralConstant(CharLiteralConstant {
                            kind_param: None,
                            string: "howdy, \"partner\"!",
                        })
                    )),
                    span: test_span(6, 25)
                }
            }),],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "foo = \"howdy, \"\"partner\"\"!\"",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::CharLiteralConstant(CharLiteralConstant {
                            kind_param: None,
                            string: "howdy, \"partner\"!",
                        })
                    )),
                    span: test_span(6, 27)
                }
            }),],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier("foo = bar_'howdy, partner!'", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::CharLiteralConstant(CharLiteralConstant {
                            kind_param: Some(KindParam::ScalarIntConstantName(bar)),
                            string: "howdy, partner!",
                        })
                    )),
                    span: test_span(6, 27)
                }
            }),],
            get_stmts(&mut c)
        );
    }

    {
        let mut c = classifier(
            "foo = bar _ 'howdy, partner!'",
            &sink,
            &mut interner,
            &arena,
        );

        assert_eq!(
            vec![Assignment(AssignmentStmt {
                variable: Spanned {
                    val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                    span: test_span(0, 3)
                },
                expr: Spanned {
                    val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::CharLiteralConstant(CharLiteralConstant {
                            kind_param: Some(KindParam::ScalarIntConstantName(bar)),
                            string: "howdy, partner!",
                        })
                    )),
                    span: test_span(6, 29)
                }
            }),],
            get_stmts(&mut c)
        );
    }
}
