//! Tests to verify correct block-* statements are returned by classifier

use crate::error::DiagnosticSink;
use crate::intern::StringInterner;
use crate::num::Uint;
use crate::parser::classify::statements::{
    CharLiteralConstant, DataEditDesc, FormatItem, FormatSpecification, FormatStmt,
    IntLiteralConstant, Sign, SignedIntLiteralConstant, M, R, V, W,
};
use crate::parser::classify::{ClassifierArena, StmtKind};
use std::cell::RefCell;

use super::{classifier, get_stmts};

fn w(num: u32) -> W<'static> {
    W(IntLiteralConstant {
        digit_string: Uint::Small(num),
        kind_param: None,
    })
}

fn m(num: u32) -> M<'static> {
    M(IntLiteralConstant {
        digit_string: Uint::Small(num),
        kind_param: None,
    })
}

fn r(num: u32) -> R<'static> {
    R(IntLiteralConstant {
        digit_string: Uint::Small(num),
        kind_param: None,
    })
}

const fn v(sign: Option<Sign>, num: u32) -> V<'static> {
    V(SignedIntLiteralConstant {
        sign,
        int_literal_constant: IntLiteralConstant {
            digit_string: Uint::Small(num),
            kind_param: None,
        },
    })
}

fn i(r: Option<R<'static>>, w: W<'static>, m: Option<M<'static>>) -> FormatItem<'static> {
    FormatItem::DataEditDesc(r, DataEditDesc::I(w, m))
}

fn b(r: Option<R<'static>>, w: W<'static>, m: Option<M<'static>>) -> FormatItem<'static> {
    FormatItem::DataEditDesc(r, DataEditDesc::B(w, m))
}

fn dt(
    r: Option<R<'static>>,
    text: Option<&'static str>,
    vs: Option<&'static [V<'static>]>,
) -> FormatItem<'static> {
    FormatItem::DataEditDesc(
        r,
        DataEditDesc::DT(
            text.map(|t| CharLiteralConstant {
                kind_param: None,
                string: t,
            }),
            vs,
        ),
    )
}

#[test]
fn format() {
    use StmtKind::*;

    let mut interner = StringInterner::new();
    let sink = RefCell::new(DiagnosticSink::Raw(Box::new(std::io::sink())));
    let arena = ClassifierArena::new();

    {
        let mut c = classifier("format()", &sink, &mut interner, &arena);

        assert_eq!(
            vec![Format(FormatStmt(FormatSpecification {
                format_items: &[],
                unlimited_format_item: None,
            }))],
            get_stmts(&mut c)
        );
    }

    assert_eq!(
        vec![Format(FormatStmt(FormatSpecification {
            format_items: &[i(None, w(20), None)],
            unlimited_format_item: None,
        }))],
        get_stmts(&mut classifier("format(I20)", &sink, &mut interner, &arena))
    );

    assert_eq!(
        vec![Format(FormatStmt(FormatSpecification {
            format_items: &[i(None, w(20), Some(m(3)))],
            unlimited_format_item: None,
        }))],
        get_stmts(&mut classifier(
            "format(I20.3)",
            &sink,
            &mut interner,
            &arena
        ))
    );

    assert_eq!(
        vec![Format(FormatStmt(FormatSpecification {
            format_items: &[i(Some(r(9)), w(20), Some(m(3)))],
            unlimited_format_item: None,
        }))],
        get_stmts(&mut classifier(
            "format(9I20.3)",
            &sink,
            &mut interner,
            &arena
        ))
    );

    assert_eq!(
        vec![Format(FormatStmt(FormatSpecification {
            format_items: &[i(None, w(20), Some(m(3))), b(None, w(17), Some(m(8)))],
            unlimited_format_item: None,
        }))],
        get_stmts(&mut classifier(
            "format(I20.3, B17.8)",
            &sink,
            &mut interner,
            &arena
        ))
    );

    assert_eq!(
        vec![Format(FormatStmt(FormatSpecification {
            format_items: &[dt(None, Some("Foobar"), None)],
            unlimited_format_item: None,
        }))],
        get_stmts(&mut classifier(
            "format(DT'Foobar')",
            &sink,
            &mut interner,
            &arena
        ))
    );

    {
        const VS: [V<'static>; 3] = [v(None, 11), v(None, 2), v(None, 31)];

        assert_eq!(
            vec![Format(FormatStmt(FormatSpecification {
                format_items: &[dt(Some(r(8)), Some("Foobar"), Some(&VS))],
                unlimited_format_item: None,
            }))],
            get_stmts(&mut classifier(
                "format(8DT'Foobar'(11,2,31))",
                &sink,
                &mut interner,
                &arena
            ))
        );
    }

    {
        const VS: [V<'static>; 3] = [v(Some(Sign::Minus), 1), v(None, 2), v(Some(Sign::Plus), 3)];

        assert_eq!(
            vec![Format(FormatStmt(FormatSpecification {
                format_items: &[dt(Some(r(8)), Some("Foobar"), Some(&VS))],
                unlimited_format_item: None,
            }))],
            get_stmts(&mut classifier(
                "format(8DT'Foobar'(-1,2,+3))",
                &sink,
                &mut interner,
                &arena
            ))
        );
    }
}
