//! Expression parsing for classifier

use num_bigint::BigUint;

use crate::num::Uint;
use crate::parser::lex::TokenKind;
use crate::string::CaseInsensitiveContinuationStr;

use super::statements::{
    BozLiteralConstant, ComplexLiteralConstant, ComplexLiteralPart, DigitString, Exponent,
    ExponentLetter, ExponentPart, Expr, IntLiteralConstant, KindParam, LiteralConstant, PrimaryRaw,
    RealLiteralConstant, Sign, SignedDigitString, SignedIntLiteralConstant,
    SignedRealLiteralConstant, Significand, Spanned, UnaryOp,
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
        let unary_op = match self.peek_kind() {
            Some(TokenKind::NotOp) => Some((UnaryOp::Not, self.bump().unwrap().span)),
            Some(TokenKind::Plus) => Some((UnaryOp::Plus, self.bump().unwrap().span)),
            Some(TokenKind::Minus) => Some((UnaryOp::Minus, self.bump().unwrap().span)),
            Some(TokenKind::DefinedOperator) => {
                let t = self.bump().unwrap();
                Some((
                    UnaryOp::Defined(
                        t.try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                    ),
                    t.span,
                ))
            }
            _ => None,
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

        let first_part = match self.peek_kind() {
            Some(TokenKind::LeftParen) => {
                let start_span = self.bump().unwrap().span;

                let parse_complex_literal_constant = match self.check_complex_literal_part(1) {
                    Some(next_idx) => match self.peek_nth_kind(next_idx) {
                        Some(TokenKind::Comma) => true,
                        _ => false,
                    },
                    _ => false,
                };

                if parse_complex_literal_constant {
                    let real_part = self.complex_literal_part()?.val;
                    assert_eq!(
                        TokenKind::Comma,
                        self.bump().unwrap().kind,
                        "Internal compiler error: We should not have tried to parse this \
                         complex-literal-constant unless there's a comma here."
                    );
                    let imag_part = self.complex_literal_part()?.val;

                    let end_span = match self.peek_kind() {
                        Some(TokenKind::RightParen) => self.bump().unwrap().span,
                        _ => {
                            self.emit_expected_token(&[TokenKind::RightParen]);
                            return None;
                        }
                    };

                    Some(Spanned::new(
                        Expr::Primary(PrimaryRaw::LiteralConstant(
                            LiteralConstant::ComplexLiteralConstant(ComplexLiteralConstant {
                                real_part,
                                imag_part,
                            }),
                        )),
                        start_span.concat(end_span),
                    ))
                } else {
                    let expr = self.expr()?.val;

                    let close_span = match self.peek_kind() {
                        Some(TokenKind::RightParen) => self.bump().unwrap().span,
                        _ => {
                            self.emit_expected_token(&[TokenKind::RightParen]);

                            return None;
                        }
                    };

                    Some(Spanned::new(expr, start_span.concat(close_span)))
                }
            }
            Some(TokenKind::DigitString) => {
                let int_literal_constant = self.int_literal_constant()?;

                Some(Spanned::new(
                    Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::IntLiteralConstant(int_literal_constant.val),
                    )),
                    int_literal_constant.span,
                ))
            }
            Some(TokenKind::BinaryConstant)
            | Some(TokenKind::OctalConstant)
            | Some(TokenKind::HexConstant) => {
                let t = self.bump().unwrap();
                let uint = t.try_into_uint(&self.text, &self.arena.big_uints).unwrap();

                let boz_constant = match t.kind {
                    TokenKind::BinaryConstant => BozLiteralConstant::BinaryConstant(uint),
                    TokenKind::OctalConstant => BozLiteralConstant::OctalConstant(uint),
                    TokenKind::HexConstant => BozLiteralConstant::HexConstant(uint),
                    k => panic!("Internal compiler error: unexpected token kind: {:?}", k),
                };

                Some(Spanned::new(
                    Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::BozLiteralConstant(boz_constant),
                    )),
                    t.span,
                ))
            }
            Some(TokenKind::RealLiteralConstant) => {
                let real_literal_constant = self.real_literal_constant()?;

                Some(Spanned::new(
                    Expr::Primary(PrimaryRaw::LiteralConstant(
                        LiteralConstant::RealLiteralConstant(real_literal_constant.val),
                    )),
                    real_literal_constant.span,
                ))
            }
            _ => unimplemented!(),
        };

        first_part
    }

    /// Parses a KindParam, starting with the `_`.
    fn kind_param(&mut self) -> Option<Spanned<KindParam<'arena>>> {
        let start_span = match self.peek_kind() {
            Some(TokenKind::Underscore) => self.bump().unwrap().span,
            _ => {
                self.emit_expected_token(&[TokenKind::Underscore]);
                return None;
            }
        };

        match self.peek_kind() {
            Some(t) if t.is_name() => {
                let t = self.bump().unwrap();
                Some(Spanned::new(
                    KindParam::ScalarIntConstantName(
                        t.try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                    ),
                    start_span.concat(t.span),
                ))
            }
            Some(TokenKind::DigitString) => {
                let t = self.bump().unwrap();
                Some(Spanned::new(
                    KindParam::DigitString(
                        t.try_into_uint(&self.text, &self.arena.big_uints).unwrap(),
                    ),
                    start_span.concat(t.span),
                ))
            }
            _ => {
                self.emit_expected_token(&[TokenKind::Name, TokenKind::DigitString]);
                None
            }
        }
    }

    /// Checks to see if we're looking at a real-part/imag-part without consuming any tokens.
    /// Returns index past real-part/imag-part, if it's possible that what we're looking at is a
    /// real-part or imag-part.
    fn check_complex_literal_part(&mut self, idx: usize) -> Option<usize> {
        let idx = match self.peek_nth_kind(idx)? {
            t if t.is_name() => return Some(idx + 1),
            t if t.is_sign() => idx + 1,
            TokenKind::DigitString | TokenKind::RealLiteralConstant => idx,
            _ => return None,
        };

        match self.peek_nth_kind(idx)? {
            TokenKind::DigitString | TokenKind::RealLiteralConstant => {
                match self.peek_nth_kind(idx + 1) {
                    Some(TokenKind::Underscore) => match self.peek_nth_kind(idx + 2)? {
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
        match self.peek_kind() {
            Some(t) if t.is_name() => {
                let t = self.bump().unwrap();
                Some(Spanned::new(
                    ComplexLiteralPart::NamedConstant(
                        t.try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                    ),
                    t.span,
                ))
            }
            Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
                match self.peek_nth_kind(1) {
                    Some(TokenKind::DigitString) => {
                        let signed_int_literal_constant = self.signed_int_literal_constant()?;

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
                        self.bump();
                        self.emit_expected_token(&[
                            TokenKind::DigitString,
                            TokenKind::RealLiteralConstant,
                        ]);
                        None
                    }
                }
            }
            Some(TokenKind::DigitString) => {
                let signed_int_literal_constant = self.signed_int_literal_constant()?;

                Some(Spanned::new(
                    ComplexLiteralPart::SignedIntLiteralConstant(signed_int_literal_constant.val),
                    signed_int_literal_constant.span,
                ))
            }
            Some(TokenKind::RealLiteralConstant) => {
                let signed_real_literal_constant = self.signed_real_literal_constant()?;

                Some(Spanned::new(
                    ComplexLiteralPart::SignedRealLiteralConstant(signed_real_literal_constant.val),
                    signed_real_literal_constant.span,
                ))
            }
            _ => {
                self.emit_expected_token(&[
                    TokenKind::Name,
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::DigitString,
                    TokenKind::RealLiteralConstant,
                ]);
                None
            }
        }
    }

    fn int_literal_constant(&mut self) -> Option<Spanned<IntLiteralConstant<'arena>>> {
        let t = match self.peek_kind() {
            Some(TokenKind::DigitString) => self.bump().unwrap(),
            _ => {
                self.emit_expected_token(&[TokenKind::DigitString]);
                return None;
            }
        };

        let uint = t.try_into_uint(&self.text, &self.arena.big_uints).unwrap();

        let (kind_param, span) = match self.peek_kind() {
            Some(TokenKind::Underscore) => {
                let Spanned {
                    val: kind_param,
                    span: kind_span,
                } = self.kind_param()?;

                (Some(kind_param), t.span.concat(kind_span))
            }
            _ => (None, t.span),
        };

        Some(Spanned::new(
            IntLiteralConstant {
                digit_string: uint,
                kind_param,
            },
            span,
        ))
    }

    fn signed_int_literal_constant(&mut self) -> Option<Spanned<SignedIntLiteralConstant<'arena>>> {
        let sign = match self.peek_kind() {
            Some(TokenKind::Plus) => Some(Spanned::new(Sign::Plus, self.bump().unwrap().span)),
            Some(TokenKind::Minus) => Some(Spanned::new(Sign::Minus, self.bump().unwrap().span)),
            Some(TokenKind::DigitString) => None,
            _ => {
                self.emit_expected_token(&[
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::DigitString,
                ]);
                return None;
            }
        };

        let int_literal_constant = self.int_literal_constant()?;

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
        if Some(TokenKind::RealLiteralConstant) != self.peek_kind() {
            self.emit_expected_token(&[TokenKind::RealLiteralConstant]);
            return None;
        }

        let t = self.bump().unwrap();

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

        let kind_param = match self.peek_kind() {
            Some(TokenKind::Underscore) => self.kind_param(),
            _ => None,
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
        let sign = match self.peek_kind() {
            Some(TokenKind::Plus) => Some(Spanned::new(Sign::Plus, self.bump().unwrap().span)),
            Some(TokenKind::Minus) => Some(Spanned::new(Sign::Minus, self.bump().unwrap().span)),
            Some(TokenKind::RealLiteralConstant) => None,
            _ => {
                self.emit_expected_token(&[
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::RealLiteralConstant,
                ]);
                return None;
            }
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
}
