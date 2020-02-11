macro_rules! expect {
    ($c:expr, $start_span:expr, $e:expr) => {{
        let start_span: crate::span::Span = $start_span;
        let c: &mut crate::parser::classify::Classifier = $c;
        if let Some(t) = c.check_and_bump($e) {
            t
        } else {
            return c.unexpected_token(&start_span);
        }
    }};
}

macro_rules! expect_eos {
    ($c:expr, $start_span:expr) => {{
        let start_span: crate::span::Span = $start_span;
        let c: &mut crate::parser::classify::Classifier = $c;
        if let Some(t) = c.check_eos_and_bump() {
            t
        } else {
            return c.unexpected_token(&start_span);
        }
    }};
}
