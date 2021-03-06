//! Expression parsing for classifier

use std::iter::FromIterator;

use num_bigint::BigUint;

use crate::num::Uint;
use crate::parser::lex::{Token, TokenKind};
use crate::span::Span;
use crate::string::{CaseInsensitiveContinuationStr, ContinuationStr};

use super::statements::{
    BozLiteralConstant, CharLiteralConstant, ComplexLiteralConstant, ComplexLiteralPart,
    DigitString, Exponent, ExponentLetter, ExponentPart, Expr, IntLiteralConstant, KindParam,
    LiteralConstant, LogicalLiteralConstant, PrimaryRaw, RealLiteralConstant, Sign,
    SignedDigitString, SignedIntLiteralConstant, SignedRealLiteralConstant, Significand, Spanned,
    UnaryOp,
};
use super::Classifier;

impl<'input, 'arena> Classifier<'input, 'arena> {
    /// R1022: expr
    ///
    /// Parses an expression from the beginning of the expression. When an error is encountered, an
    /// error is emitted and None is returned without consuming the erroneous token.
    pub(super) fn expr(&mut self) -> Option<Spanned<Expr<'arena>>> {
        // TODO: Implement as iterative algorithm rather than recursive so that we don't have to
        // wory about stack overflow

        // Check if this is a unary expression
        let unary_op = if self.check(TokenKind::NotOp) {
            Some((UnaryOp::Not, self.tokenizer.bump().unwrap().span))
        } else if self.check(TokenKind::Plus) {
            Some((UnaryOp::Plus, self.tokenizer.bump().unwrap().span))
        } else if self.check(TokenKind::Minus) {
            Some((UnaryOp::Minus, self.tokenizer.bump().unwrap().span))
        } else if self.check(TokenKind::DefinedOperator) {
            let t = self.tokenizer.bump().unwrap();
            Some((
                UnaryOp::Defined(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                ),
                t.span,
            ))
        } else {
            None
        };

        if let Some((unary_op, span)) = unary_op {
            let rest_of_expr = self.expr()?;

            return Some(Spanned::new(
                Expr::UnaryOperation {
                    op: unary_op,
                    target: self.arena.expressions.alloc(rest_of_expr.val),
                },
                span.concat(rest_of_expr.span),
            ));
        }

        if self.check(TokenKind::LeftParen) {
            let parse_complex_literal_constant = match self.check_complex_literal_part(1) {
                Some(next_idx) => match self.tokenizer.peek_nth_kind(next_idx) {
                    Some(TokenKind::Comma) => true,
                    _ => false,
                },
                _ => false,
            };

            if !parse_complex_literal_constant {
                let start_span = self.tokenizer.bump().unwrap().span;

                let expr = self.expr()?.val;

                let close_span = if self.check(TokenKind::RightParen) {
                    self.tokenizer.bump().unwrap().span
                } else {
                    return None;
                };

                return Some(Spanned::new(expr, start_span.concat(close_span)));
            }
        }

        let primary = self.primary()?;

        Some(Spanned::new(Expr::Primary(primary.val), primary.span))
    }

    pub(super) fn primary(&mut self) -> Option<Spanned<PrimaryRaw<'arena>>> {
        if self.check(TokenKind::LeftParen) {
            let start_span = self.tokenizer.bump().unwrap().span;

            let real_part = self.complex_literal_part()?.val;
            assert_eq!(
                TokenKind::Comma,
                self.tokenizer.bump().unwrap().kind,
                "Internal compiler error: We should not have tried to parse this \
                     complex-literal-constant unless there's a comma here."
            );
            let imag_part = self.complex_literal_part()?.val;

            let end_span = if self.check(TokenKind::RightParen) {
                self.tokenizer.bump().unwrap().span
            } else {
                return None;
            };

            Some(Spanned::new(
                PrimaryRaw::LiteralConstant(LiteralConstant::ComplexLiteralConstant(
                    ComplexLiteralConstant {
                        real_part,
                        imag_part,
                    },
                )),
                start_span.concat(end_span),
            ))
        } else if self.check(TokenKind::DigitString) {
            match (
                self.tokenizer.peek_nth_kind(1),
                self.tokenizer.peek_nth_kind(2),
            ) {
                (Some(TokenKind::Underscore), Some(TokenKind::CharLiteralConstant)) => {
                    let char_literal_constant = self.check_and_bump_char_literal_constant(true)?;

                    Some(Spanned::new(
                        PrimaryRaw::LiteralConstant(LiteralConstant::CharLiteralConstant(
                            char_literal_constant.val,
                        )),
                        char_literal_constant.span,
                    ))
                }
                _ => {
                    let int_literal_constant = self.check_and_bump_int_literal_constant(true)?;

                    Some(Spanned::new(
                        PrimaryRaw::LiteralConstant(LiteralConstant::IntLiteralConstant(
                            int_literal_constant.val,
                        )),
                        int_literal_constant.span,
                    ))
                }
            }
        } else if self.check(TokenKind::BinaryConstant)
            || self.check(TokenKind::OctalConstant)
            || self.check(TokenKind::HexConstant)
        {
            let t = self.tokenizer.bump().unwrap();
            let uint = t.try_into_uint(&self.text, &self.arena.big_uints).unwrap();

            let boz_constant = match t.kind {
                TokenKind::BinaryConstant => BozLiteralConstant::BinaryConstant(uint),
                TokenKind::OctalConstant => BozLiteralConstant::OctalConstant(uint),
                TokenKind::HexConstant => BozLiteralConstant::HexConstant(uint),
                k => panic!("Internal compiler error: unexpected token kind: {:?}", k),
            };

            Some(Spanned::new(
                PrimaryRaw::LiteralConstant(LiteralConstant::BozLiteralConstant(boz_constant)),
                t.span,
            ))
        } else if self.check(TokenKind::RealLiteralConstant) {
            let real_literal_constant = self.real_literal_constant()?;

            Some(Spanned::new(
                PrimaryRaw::LiteralConstant(LiteralConstant::RealLiteralConstant(
                    real_literal_constant.val,
                )),
                real_literal_constant.span,
            ))
        } else if self.check(TokenKind::CharLiteralConstant) {
            let char_literal_constant = self.check_and_bump_char_literal_constant(true)?;

            Some(Spanned::new(
                PrimaryRaw::LiteralConstant(LiteralConstant::CharLiteralConstant(
                    char_literal_constant.val,
                )),
                char_literal_constant.span,
            ))
        } else if self.check(TokenKind::True) || self.check(TokenKind::False) {
            let logical_literal_constant = self.logical_literal_constant()?;

            Some(Spanned::new(
                PrimaryRaw::LiteralConstant(LiteralConstant::LogicalLiteralConstant(
                    logical_literal_constant.val,
                )),
                logical_literal_constant.span,
            ))
        } else if self.check_name() {
            match (
                self.tokenizer.peek_nth_kind(1),
                self.tokenizer.peek_nth_kind(2),
            ) {
                // Check for both `_ char-literal-constant` and `char-literal-constant`, since it's
                // possible the name may end with an underscore, which is legal.
                (Some(TokenKind::Underscore), Some(TokenKind::CharLiteralConstant))
                | (Some(TokenKind::CharLiteralConstant), _) => {
                    let char_literal_constant = self.check_and_bump_char_literal_constant(true)?;

                    Some(Spanned::new(
                        PrimaryRaw::LiteralConstant(LiteralConstant::CharLiteralConstant(
                            char_literal_constant.val,
                        )),
                        char_literal_constant.span,
                    ))
                }
                _ => unimplemented!(),
            }
        } else {
            return None;
        }
    }

    /// Parses a KindParam, starting with the `_`.
    fn kind_param(&mut self) -> Option<Spanned<KindParam<'arena>>> {
        let start_span = if self.check(TokenKind::Underscore) {
            self.tokenizer.bump().unwrap().span
        } else {
            return None;
        };

        if self.check_name() {
            let t = self.tokenizer.bump().unwrap();
            Some(Spanned::new(
                KindParam::ScalarIntConstantName(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                ),
                start_span.concat(t.span),
            ))
        } else if self.check(TokenKind::DigitString) {
            let t = self.tokenizer.bump().unwrap();
            Some(Spanned::new(
                KindParam::DigitString(t.try_into_uint(&self.text, &self.arena.big_uints).unwrap()),
                start_span.concat(t.span),
            ))
        } else {
            None
        }
    }

    fn reverse_kind_param(&mut self) -> Option<Spanned<KindParam<'arena>>> {
        let (start_span, kind_param) = if self.check_name() {
            let t = self.tokenizer.bump().unwrap();

            let (last_char_index, last_char) =
                ContinuationStr::new(&self.text[t.span.start as usize..t.span.end as usize])
                    .char_indices()
                    .last()
                    .unwrap();

            if last_char == '_' {
                // Handle the case of t being a name with a trailing underscore.
                let token = Token {
                    kind: TokenKind::Name,
                    span: Span {
                        file_id: self.file_id,
                        start: t.span.start,
                        end: t.span.start + last_char_index as u32,
                    },
                };

                // Early return - rest of function attempts to parse an underscore token
                return Some(Spanned::new(
                    KindParam::ScalarIntConstantName(
                        token
                            .try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                    ),
                    t.span,
                ));
            } else {
                (
                    t.span,
                    KindParam::ScalarIntConstantName(
                        t.try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                    ),
                )
            }
        } else if self.check(TokenKind::DigitString) {
            let t = self.tokenizer.bump().unwrap();
            (
                t.span,
                KindParam::DigitString(t.try_into_uint(&self.text, &self.arena.big_uints).unwrap()),
            )
        } else {
            return None;
        };

        if self.check(TokenKind::Underscore) {
            Some(Spanned::new(
                kind_param,
                start_span.concat(self.tokenizer.bump().unwrap().span),
            ))
        } else {
            None
        }
    }

    /// Checks to see if we're looking at a real-part/imag-part without consuming any tokens.
    /// Returns index past real-part/imag-part, if it's possible that what we're looking at is a
    /// real-part or imag-part.
    fn check_complex_literal_part(&mut self, idx: usize) -> Option<usize> {
        let idx = match self.tokenizer.peek_nth_kind(idx)? {
            t if t.is_name() => return Some(idx + 1),
            t if t.is_sign() => idx + 1,
            TokenKind::DigitString | TokenKind::RealLiteralConstant => idx,
            _ => return None,
        };

        match self.tokenizer.peek_nth_kind(idx)? {
            TokenKind::DigitString | TokenKind::RealLiteralConstant => {
                match self.tokenizer.peek_nth_kind(idx + 1) {
                    Some(TokenKind::Underscore) => match self.tokenizer.peek_nth_kind(idx + 2)? {
                        t if t.is_name() => Some(idx + 3),
                        TokenKind::DigitString => Some(idx + 3),
                        _ => None,
                    },
                    None => Some(idx + 1),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Parses a real-part or imag-part
    fn complex_literal_part(&mut self) -> Option<Spanned<ComplexLiteralPart<'arena>>> {
        if self.check_name() {
            let t = self.tokenizer.bump().unwrap();
            Some(Spanned::new(
                ComplexLiteralPart::NamedConstant(
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                ),
                t.span,
            ))
        } else if self.check(TokenKind::Plus) || self.check(TokenKind::Minus) {
            match self.tokenizer.peek_nth_kind(1) {
                Some(TokenKind::DigitString) => {
                    let signed_int_literal_constant =
                        self.expect_signed_int_literal_constant(true)?;

                    Some(Spanned::new(
                        ComplexLiteralPart::SignedIntLiteralConstant(
                            signed_int_literal_constant.val,
                        ),
                        signed_int_literal_constant.span,
                    ))
                }
                Some(TokenKind::RealLiteralConstant) => {
                    let signed_real_literal_constant = self.signed_real_literal_constant()?;

                    Some(Spanned::new(
                        ComplexLiteralPart::SignedRealLiteralConstant(
                            signed_real_literal_constant.val,
                        ),
                        signed_real_literal_constant.span,
                    ))
                }
                _ => {
                    // bump the +|-, emit expected token
                    self.tokenizer.bump();
                    None
                }
            }
        } else if self.check(TokenKind::DigitString) {
            let signed_int_literal_constant = self.expect_signed_int_literal_constant(true)?;

            Some(Spanned::new(
                ComplexLiteralPart::SignedIntLiteralConstant(signed_int_literal_constant.val),
                signed_int_literal_constant.span,
            ))
        } else if self.check(TokenKind::RealLiteralConstant) {
            let signed_real_literal_constant = self.signed_real_literal_constant()?;

            Some(Spanned::new(
                ComplexLiteralPart::SignedRealLiteralConstant(signed_real_literal_constant.val),
                signed_real_literal_constant.span,
            ))
        } else {
            None
        }
    }

    pub(super) fn check_and_bump_int_literal_constant(
        &mut self,
        has_kind_param: bool,
    ) -> Option<Spanned<IntLiteralConstant<'arena>>> {
        let t = self.check_and_bump(TokenKind::DigitString)?;

        let uint = t.try_into_uint(&self.text, &self.arena.big_uints).unwrap();

        let (kind_param, span) = if has_kind_param && self.check(TokenKind::Underscore) {
            let Spanned {
                val: kind_param,
                span: kind_span,
            } = self.kind_param()?;

            (Some(kind_param), t.span.concat(kind_span))
        } else {
            (None, t.span)
        };

        Some(Spanned::new(
            IntLiteralConstant {
                digit_string: uint,
                kind_param,
            },
            span,
        ))
    }

    pub(super) fn expect_int_literal_constant(
        &mut self,
        has_kind_param: bool,
    ) -> Option<Spanned<IntLiteralConstant<'arena>>> {
        if let Some(x) = self.check_and_bump_int_literal_constant(has_kind_param) {
            Some(x)
        } else {
            self.emit_unexpected_token();
            None
        }
    }

    pub(super) fn check_and_bump_signed_int_literal_constant(
        &mut self,
        has_kind_param: bool,
    ) -> Option<Spanned<SignedIntLiteralConstant>> {
        let has_sign = self.check(TokenKind::Plus) || self.check(TokenKind::Minus);

        let is_signed_int_literal_constant = if has_sign {
            self.tokenizer.peek_nth_kind(1) == Some(TokenKind::DigitString)
        } else {
            self.check(TokenKind::DigitString)
        };

        if !is_signed_int_literal_constant {
            return None;
        }

        self.expect_signed_int_literal_constant(has_kind_param)
    }

    pub(super) fn expect_signed_int_literal_constant(
        &mut self,
        has_kind_param: bool,
    ) -> Option<Spanned<SignedIntLiteralConstant<'arena>>> {
        let sign = if let Some(t) = self.check_and_bump(TokenKind::Plus) {
            Some(Spanned::new(Sign::Plus, t.span))
        } else if let Some(t) = self.check_and_bump(TokenKind::Minus) {
            Some(Spanned::new(Sign::Minus, t.span))
        } else if self.check(TokenKind::DigitString) {
            None
        } else {
            self.emit_unexpected_token();
            return None;
        };

        let int_literal_constant = self.check_and_bump_int_literal_constant(has_kind_param)?;

        Some(Spanned::new(
            SignedIntLiteralConstant {
                sign: sign.map(|s| s.val),
                int_literal_constant: int_literal_constant.val,
            },
            sign.map_or(int_literal_constant.span, |s| {
                s.span.concat(int_literal_constant.span)
            }),
        ))
    }

    fn real_literal_constant(&mut self) -> Option<Spanned<RealLiteralConstant<'arena>>> {
        if !self.check(TokenKind::RealLiteralConstant) {
            return None;
        }

        let t = self.tokenizer.bump().unwrap();

        enum Either {
            Small(u32),
            Big(BigUint),
        }

        let mut cont_str = CaseInsensitiveContinuationStr::new(
            &self.text[t.span.start as usize..t.span.end as usize],
        )
        .iter()
        .peekable();

        fn consume_uint(cont_str: &mut std::iter::Peekable<impl Iterator<Item = char>>) -> Either {
            let mut u = Either::Small(0);
            loop {
                let digit = cont_str.next().unwrap() as u8 - b'0';

                u = loop {
                    break match u {
                        Either::Small(x) => {
                            match x
                                .checked_mul(10u32)
                                .and_then(|x| x.checked_add(digit as u32))
                            {
                                Some(x) => Either::Small(x),
                                None => {
                                    u = Either::Big(x.into());
                                    continue;
                                }
                            }
                        }
                        Either::Big(mut x) => {
                            x *= 10u32;
                            x += digit as u32;
                            Either::Big(x)
                        }
                    };
                };

                match cont_str.peek() {
                    Some(c) if !c.is_ascii_digit() => return u,
                    None => return u,
                    _ => {}
                }
            }
        };

        let integer = match cont_str.peek().unwrap() {
            '.' => None,
            c if c.is_ascii_digit() => Some(match consume_uint(&mut cont_str) {
                Either::Small(x) => Uint::Small(x),
                Either::Big(x) => Uint::Big(self.arena.big_uints.alloc(x)),
            }),
            _ => panic!(
                "Internal compiler error: This isn't a real literal constant, but it was tokenized \
                 as one"
            ),
        };

        let decimal = match cont_str.peek().unwrap() {
            '.' => {
                cont_str.next();

                match cont_str.peek() {
                    Some(c) => match c {
                        c if c.is_ascii_digit() => Some(match consume_uint(&mut cont_str) {
                            Either::Small(x) => Uint::Small(x),
                            Either::Big(x) => Uint::Big(self.arena.big_uints.alloc(x)),
                        }),
                        'e' | 'd' | '_' => None,
                        _ => panic!(
                            "Internal compiler error: This isn't a real literal constant, but it \
                             was tokenized as one"
                        ),
                    },
                    None => None,
                }
            }
            'e' | 'd' | '_' => None,
            _ => panic!(
                "Internal compiler error: This isn't a real literal constant, but it was tokenized \
                 as one"
            ),
        };

        let significand = Significand { integer, decimal };

        let exponent_letter = match cont_str.peek() {
            Some('e') => {
                cont_str.next();
                Some(ExponentLetter::E)
            }
            Some('d') => {
                cont_str.next();
                Some(ExponentLetter::D)
            }
            None => None,
            _ => panic!(
                "Internal compiler error: This isn't a real literal constant, but it was tokenized \
                 as one"
            ),
        };

        let exponent_part = if let Some(letter) = exponent_letter {
            let sign = match cont_str.peek() {
                Some('+') => {
                    cont_str.next();
                    Some(Sign::Plus)
                }
                Some('-') => {
                    cont_str.next();
                    Some(Sign::Minus)
                }
                Some(c) if c.is_ascii_digit() => None,
                None => None,
                _ => panic!(
                    "Internal compiler error: This isn't a real literal constant, but it was \
                     tokenized as one"
                ),
            };

            let digit_string = DigitString(match consume_uint(&mut cont_str) {
                Either::Small(x) => Uint::Small(x),
                Either::Big(x) => Uint::Big(self.arena.big_uints.alloc(x)),
            });

            Some(ExponentPart {
                letter,
                exponent: Exponent(SignedDigitString { sign, digit_string }),
            })
        } else {
            None
        };

        let kind_param = if self.check(TokenKind::Underscore) {
            self.kind_param()
        } else {
            None
        };

        Some(Spanned::new(
            RealLiteralConstant {
                significand,
                exponent_part,
                kind_param: kind_param.map(|k| k.val),
            },
            kind_param.map_or(t.span, |k| t.span.concat(k.span)),
        ))
    }

    fn signed_real_literal_constant(
        &mut self,
    ) -> Option<Spanned<SignedRealLiteralConstant<'arena>>> {
        let sign = if self.check(TokenKind::Plus) {
            Some(Spanned::new(
                Sign::Plus,
                self.tokenizer.bump().unwrap().span,
            ))
        } else if self.check(TokenKind::Minus) {
            Some(Spanned::new(
                Sign::Minus,
                self.tokenizer.bump().unwrap().span,
            ))
        } else if self.check(TokenKind::RealLiteralConstant) {
            None
        } else {
            return None;
        };

        let real_literal_constant = self.real_literal_constant()?;

        Some(Spanned::new(
            SignedRealLiteralConstant {
                sign: sign.map(|s| s.val),
                real_literal_constant: real_literal_constant.val,
            },
            sign.map_or(real_literal_constant.span, |s| {
                s.span.concat(real_literal_constant.span)
            }),
        ))
    }

    /// R724: char-literal-constant
    pub(super) fn check_and_bump_char_literal_constant(
        &mut self,
        has_kind_param: bool,
    ) -> Option<Spanned<CharLiteralConstant<'arena>>> {
        let kind_param =
            if has_kind_param && (self.check(TokenKind::DigitString) || self.check_name()) {
                match (
                    self.tokenizer.peek_nth_kind(1),
                    self.tokenizer.peek_nth_kind(2),
                ) {
                    (Some(TokenKind::Underscore), Some(TokenKind::CharLiteralConstant)) => {
                        Some(self.reverse_kind_param()?)
                    }
                    (Some(TokenKind::CharLiteralConstant), _) if self.check_name() => {
                        Some(self.reverse_kind_param()?)
                    }
                    _ => None,
                }
            } else {
                None
            };

        if let Some(t) = self.check_and_bump(TokenKind::CharLiteralConstant) {
            let quote_char = self.text[t.span.start as usize..].chars().next().unwrap();

            let cont_str = ContinuationStr::new(
                &self.text[t.span.start as usize + 1..t.span.end as usize - 1],
            );

            let mut saw_quote = false;
            let iter = cont_str.chars().filter(|c| {
                if *c == quote_char {
                    if saw_quote {
                        saw_quote = false;
                        false
                    } else {
                        saw_quote = true;
                        true
                    }
                } else {
                    true
                }
            });

            Some(Spanned::new(
                CharLiteralConstant {
                    kind_param: kind_param.map(|k| k.val),
                    string: self.arena.string_literals.alloc(String::from_iter(iter)),
                },
                kind_param.map_or(t.span, |k| k.span.concat(t.span)),
            ))
        } else {
            None
        }
    }

    /// R725: logical-literal-constant
    fn logical_literal_constant(&mut self) -> Option<Spanned<LogicalLiteralConstant<'arena>>> {
        let (value, start_span) = if self.check(TokenKind::True) {
            (true, self.tokenizer.bump().unwrap().span)
        } else if self.check(TokenKind::False) {
            (false, self.tokenizer.bump().unwrap().span)
        } else {
            return None;
        };

        let kind_param = if self.check(TokenKind::Underscore) {
            Some(self.kind_param()?)
        } else {
            None
        };

        Some(Spanned::new(
            LogicalLiteralConstant {
                value,
                kind_param: kind_param.map(|k| k.val),
            },
            kind_param.map_or(start_span, |k| start_span.concat(k.span)),
        ))
    }
}
