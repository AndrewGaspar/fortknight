// Tests to verify label parsing works correctly

use std::cell::RefCell;

use crate::error::DiagnosticSink;
use crate::intern::StringInterner;
use crate::num::Uint;
use crate::parser::classify::statements::{
    AssignmentStmt, Designator, Expr, IntLiteralConstant, LiteralConstant, PrimaryRaw, Variable,
};
use crate::parser::classify::{
    Classifier, ClassifierArena, LabeledStmt, Spanned,
    StmtKind::{self, *},
};

use super::{classifier, test_span};

fn get_labeled_stmts<'a>(
    classifier: &'_ mut Classifier<'_, 'a>,
) -> Vec<(Option<u32>, StmtKind<'a>)> {
    let mut stmts = vec![];

    while let Some(stmt) = classifier.next_stmt() {
        let LabeledStmt { label, stmt } = stmt;
        stmts.push((label.map(|l| l.val), stmt.kind));
    }

    stmts
}

#[test]
fn labeled_assignment() {
    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    let foo = interner.intern_name("foo".into());

    {
        let mut c = classifier("800 foo = 88", &sink, &mut interner, &arena);

        assert_eq!(
            vec![(
                Some(800),
                Assignment(AssignmentStmt {
                    variable: Spanned {
                        val: Variable::DesignatorOrFunctionReference(Designator::ObjectName(foo)),
                        span: test_span(4, 7)
                    },
                    expr: Spanned {
                        val: &Expr::Primary(PrimaryRaw::LiteralConstant(
                            LiteralConstant::IntLiteralConstant(IntLiteralConstant {
                                digit_string: Uint::Small(88),
                                kind_param: None
                            })
                        )),
                        span: test_span(10, 12)
                    }
                })
            )],
            get_labeled_stmts(&mut c)
        );
    }
}
