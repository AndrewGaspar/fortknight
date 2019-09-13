use crate::parser::lex::{KeywordTokenKind, Token, TokenKind};

use super::statements::{
    DeclarationTypeSpec, DerivedTypeSpec, IntegerTypeSpec, IntrinsicTypeSpec, KindSelector,
    Spanned, TypeParamSpec, TypeParamValue,
};
use super::{declaration_type_spec_or, intrinsic_type_spec_or, Classifier, TakeUntil};

impl<'input, 'arena> Classifier<'input, 'arena> {
    /// R701: type-param-value
    ///
    /// Parse a type-param-value form start
    fn type_param_value(&mut self) -> Option<Spanned<TypeParamValue<'arena>>> {
        match self.peek_kind() {
            Some(TokenKind::Star) => Some(Spanned::new(
                TypeParamValue::Star,
                self.bump().unwrap().span,
            )),
            Some(TokenKind::Colon) => Some(Spanned::new(
                TypeParamValue::Colon,
                self.bump().unwrap().span,
            )),
            _ => {
                // TODO: Check for expression start and emit an "unexpected token" error if it
                // couldn't possibly be an expression.
                let expr = self.expr()?;
                Some(Spanned::new(
                    TypeParamValue::ScalarIntExpr(self.arena.expressions.alloc(expr.val)),
                    expr.span,
                ))
            }
        }
    }

    /// R703: declaration-type-spec
    ///
    /// Parses a declaration-type-spec from the start of the type. Returns None if the parse fails -
    /// does not consume the failure token.
    pub(super) fn declaration_type_spec(&mut self) -> Option<Spanned<DeclarationTypeSpec<'arena>>> {
        match self.peek().map(|t| t.kind) {
            Some(t) if t.is_intrinsic_type_spec_start() => {
                let spec = self.intrinsic_type_spec()?;
                Some(Spanned::new(
                    DeclarationTypeSpec::Intrinsic(spec.val),
                    spec.span,
                ))
            }
            Some(TokenKind::Keyword(KeywordTokenKind::Type)) => {
                let start_span = self.bump().unwrap().span;

                match self.peek().map(|t| t.kind) {
                    Some(TokenKind::LeftParen) => {
                        self.bump().unwrap();
                    }
                    _ => {
                        self.emit_expected_token(&[TokenKind::LeftParen]);
                        return None;
                    }
                };

                enum Either<'a> {
                    Intrinsic(Spanned<IntrinsicTypeSpec<'a>>),
                    Derived(DerivedTypeSpec<'a>),
                    Wildcard,
                };

                let type_spec = match self.peek().map(|t| t.kind) {
                    Some(t) if t.is_intrinsic_type_spec_start() => {
                        Either::Intrinsic(self.intrinsic_type_spec()?)
                    }
                    Some(t) if t.is_name() => Either::Derived(self.derived_type_spec()?.val),
                    Some(TokenKind::Star) => {
                        self.bump();
                        Either::Wildcard
                    }
                    _ => {
                        self.emit_expected_token(&intrinsic_type_spec_or(&[TokenKind::Name]));
                        return None;
                    }
                };

                let end_span = match self.peek().map(|t| t.kind) {
                    Some(TokenKind::RightParen) => self.bump().unwrap().span,
                    _ => {
                        self.emit_expected_token(&[TokenKind::RightParen]);
                        return None;
                    }
                };

                let span = start_span.concat(end_span);

                match type_spec {
                    Either::Intrinsic(intrinsic) => Some(Spanned::new(
                        DeclarationTypeSpec::TypeIntrinsic(intrinsic.val),
                        span,
                    )),
                    Either::Derived(derived) => Some(Spanned::new(
                        DeclarationTypeSpec::TypeDerived(derived),
                        span,
                    )),
                    Either::Wildcard => Some(Spanned::new(DeclarationTypeSpec::TypeWildcard, span)),
                }
            }
            Some(TokenKind::Keyword(KeywordTokenKind::Class)) => {
                let start_span = self.bump().unwrap().span;

                match self.peek().map(|t| t.kind) {
                    Some(TokenKind::LeftParen) => {
                        self.bump().unwrap();
                    }
                    _ => {
                        self.emit_expected_token(&[TokenKind::LeftParen]);
                        return None;
                    }
                };

                enum Either<'a> {
                    Derived(DerivedTypeSpec<'a>),
                    Wildcard,
                };

                let type_spec = match self.peek().map(|t| t.kind) {
                    Some(t) if t.is_name() => Either::Derived(self.derived_type_spec()?.val),
                    Some(TokenKind::Star) => {
                        self.bump();
                        Either::Wildcard
                    }
                    _ => {
                        self.emit_expected_token(&intrinsic_type_spec_or(&[TokenKind::Name]));
                        return None;
                    }
                };

                let end_span = match self.peek().map(|t| t.kind) {
                    Some(TokenKind::RightParen) => self.bump().unwrap().span,
                    _ => {
                        self.emit_expected_token(&[TokenKind::RightParen]);
                        return None;
                    }
                };

                let span = start_span.concat(end_span);

                match type_spec {
                    Either::Derived(derived) => Some(Spanned::new(
                        DeclarationTypeSpec::ClassDerived(derived),
                        span,
                    )),
                    Either::Wildcard => {
                        Some(Spanned::new(DeclarationTypeSpec::ClassWildcard, span))
                    }
                }
            }
            _ => {
                self.emit_expected_token(&declaration_type_spec_or(&[]));
                return None;
            }
        }
    }

    /// R704: intrinsic-type-spec
    ///
    /// Parses an intrinsic type spec from its start. Returns None if the parse fails and does not
    /// consume the failing token.
    fn intrinsic_type_spec(&mut self) -> Option<Spanned<IntrinsicTypeSpec<'arena>>> {
        match self.peek().map(|t| t.kind) {
            Some(TokenKind::Keyword(KeywordTokenKind::Integer)) => {
                self.type_spec(KeywordTokenKind::Integer)
            }
            Some(TokenKind::Keyword(KeywordTokenKind::Real)) => {
                self.type_spec(KeywordTokenKind::Real)
            }
            Some(TokenKind::Keyword(KeywordTokenKind::Complex)) => {
                self.type_spec(KeywordTokenKind::Complex)
            }
            Some(TokenKind::Keyword(KeywordTokenKind::Character)) => {
                self.type_spec(KeywordTokenKind::Character)
            }
            Some(TokenKind::Keyword(KeywordTokenKind::Logical)) => {
                self.type_spec(KeywordTokenKind::Logical)
            }
            Some(TokenKind::Keyword(KeywordTokenKind::Double)) => match self.peek_nth_kind(1) {
                Some(TokenKind::Keyword(KeywordTokenKind::Precision)) => {
                    let start_span = self.bump().unwrap().span;
                    let end_span = self.bump().unwrap().span;
                    Some(Spanned::new(
                        IntrinsicTypeSpec::DoublePrecision,
                        start_span.concat(end_span),
                    ))
                }
                _ => {
                    self.emit_expected_token(&[TokenKind::Keyword(KeywordTokenKind::Precision)]);
                    return None;
                }
            },
            Some(TokenKind::Keyword(KeywordTokenKind::DoublePrecision)) => Some(Spanned::new(
                IntrinsicTypeSpec::DoublePrecision,
                self.bump().unwrap().span,
            )),
            _ => {
                self.emit_expected_token(&intrinsic_type_spec_or(&[]));
                return None;
            }
        }
    }

    /// R704: intrinsic-type-spec for types with kind-selector
    fn type_spec(
        &mut self,
        expected_keyword: KeywordTokenKind,
    ) -> Option<Spanned<IntrinsicTypeSpec<'arena>>> {
        let span = match self.peek_kind() {
            Some(TokenKind::Keyword(k)) if k == expected_keyword => self.bump().unwrap().span,
            _ => {
                self.emit_expected_token(&[TokenKind::Keyword(expected_keyword)]);
                return None;
            }
        };

        let kind = match self.peek_kind() {
            Some(TokenKind::LeftParen) => Some(self.kind_selector()?),
            _ => None,
        };

        let span = kind.map_or(span, |k| span.concat(k.span));
        let kind = kind.map(|k| k.val);

        match expected_keyword {
            KeywordTokenKind::Integer => Some(Spanned::new(
                IntrinsicTypeSpec::Integer(IntegerTypeSpec(kind)),
                span,
            )),
            KeywordTokenKind::Real => Some(Spanned::new(IntrinsicTypeSpec::Real(kind), span)),
            KeywordTokenKind::Complex => Some(Spanned::new(IntrinsicTypeSpec::Complex(kind), span)),
            KeywordTokenKind::Character => {
                Some(Spanned::new(IntrinsicTypeSpec::Character(kind), span))
            }
            KeywordTokenKind::Logical => Some(Spanned::new(IntrinsicTypeSpec::Logical(kind), span)),
            k => panic!(
                "Internal compiler error: `{:?}` is not a intrinsic type with a kind selector",
                k
            ),
        }
    }

    /// R705: integer-type-spec
    ///
    /// Parses from INTEGER
    fn integer_type_spec(&mut self) -> Option<Spanned<IntegerTypeSpec<'arena>>> {
        let spec = self.type_spec(KeywordTokenKind::Integer)?;

        let (spec, span) = match spec {
            Spanned {
                val: IntrinsicTypeSpec::Integer(spec),
                span,
            } => (spec, span),
            _ => {
                panic!("Internal compiler error - integer type spec was not an integer type spec!")
            }
        };

        Some(Spanned::new(spec, span))
    }

    /// R706: kind-selector
    ///
    /// Parses the kind-selector of a type from the parentheses
    fn kind_selector(&mut self) -> Option<Spanned<KindSelector<'arena>>> {
        let begin_span = match self.peek().map(|t| t.kind) {
            Some(TokenKind::LeftParen) => self.bump().unwrap().span,
            _ => {
                self.emit_expected_token(&[TokenKind::LeftParen]);
                return None;
            }
        };

        match self.peek().map(|t| t.kind) {
            Some(TokenKind::Keyword(KeywordTokenKind::Kind)) => {
                match self.peek_nth(1).map(|t| t.kind) {
                    Some(TokenKind::Equals) => {
                        self.bump(); // consume KIND
                        self.bump(); // consume =
                    }
                    Some(TokenKind::LeftParen) => {
                        // assume we're in an expression and let the expression parsing take over
                    }
                    _ => {
                        self.emit_expected_token(&[TokenKind::Equals, TokenKind::LeftParen]);
                        return None;
                    }
                }
            }
            _ => {
                // try parsing as an expression
            }
        };

        let expr = self.expr()?;

        let end_span = match self.peek_kind() {
            Some(TokenKind::RightParen) => self.bump().unwrap().span,
            _ => {
                self.emit_expected_token(&[TokenKind::RightParen]);
                return None;
            }
        };

        // parse scalar-int-constant-expr
        Some(Spanned::new(
            KindSelector(self.arena.expressions.alloc(expr.val)),
            begin_span.concat(end_span),
        ))
    }

    /// R754: derived-type-spec
    ///
    /// Parses from the `type-name`
    fn derived_type_spec(&mut self) -> Option<Spanned<DerivedTypeSpec<'arena>>> {
        let (name, start_span) = match self.peek_kind() {
            Some(t) if t.is_name() => {
                let t = self.bump().unwrap();

                (
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span,
                )
            }
            _ => {
                self.emit_expected_token(&[TokenKind::Name]);
                return None;
            }
        };

        match self.peek_kind() {
            Some(TokenKind::LeftParen) => {
                self.bump();
            }
            _ => {
                return Some(Spanned::new(
                    DerivedTypeSpec {
                        name,
                        spec_list: &[],
                    },
                    start_span,
                ));
            }
        };

        let mut error_encountered = false;

        let spec_list = self.arena.type_param_specs.alloc_extend(
            std::iter::once(self.type_param_spec()?.val).chain(std::iter::from_fn(|| {
                match self.peek_kind() {
                    Some(TokenKind::Comma) => {
                        self.bump();
                    }
                    // End of list - return none
                    Some(TokenKind::RightParen) => return None,
                    _ => {
                        error_encountered = true;
                        self.emit_expected_token(&[TokenKind::Comma, TokenKind::RightParen]);
                        return None;
                    }
                };

                match self.type_param_spec() {
                    Some(spec) => Some(spec.val),
                    None => {
                        error_encountered = true;
                        return None;
                    }
                }
            })),
        );

        if error_encountered {
            // skip to the closing ), EOS, or EOF
            self.take_until(|lookahead| match lookahead {
                Some(Token {
                    kind: TokenKind::RightParen,
                    ..
                })
                | None => TakeUntil::Stop,
                Some(t) if Self::is_eos(t) => TakeUntil::Stop,
                _ => TakeUntil::Continue,
            })
            .unwrap();
        }

        let end_span = match self.peek_kind() {
            Some(TokenKind::RightParen) => self.bump().unwrap().span,
            _ => {
                self.emit_expected_token(&[TokenKind::RightParen]);
                return None;
            }
        };

        Some(Spanned::new(
            DerivedTypeSpec { name, spec_list },
            start_span.concat(end_span),
        ))
    }

    /// R755: type-param-spec
    ///
    /// Parses a type-param-spec
    fn type_param_spec(&mut self) -> Option<Spanned<TypeParamSpec<'arena>>> {
        let keyword_and_span = match self.peek_nth_kind(0) {
            Some(t) if t.is_name() => {
                // Might be a keyword, check for =
                match self.peek_nth_kind(1) {
                    Some(TokenKind::Equals) => {
                        let keyword = self.bump().unwrap(); // keyword
                        self.bump(); // =
                        Some((
                            keyword
                                .try_intern_contents(&mut self.interner, &self.text)
                                .unwrap(),
                            keyword.span,
                        ))
                    }
                    _ => {
                        // Might be an expression, back off
                        None
                    }
                }
            }
            _ => None,
        };

        let value = self.type_param_value()?;

        let span = keyword_and_span.map_or(value.span, |(_, span)| span.concat(value.span));

        Some(Spanned::new(
            TypeParamSpec {
                keyword: keyword_and_span.map(|(k, _)| k),
                value: value.val,
            },
            span,
        ))
    }
}
