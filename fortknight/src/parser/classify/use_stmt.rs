use crate::intern::InternedName;
use crate::parser::lex::{KeywordTokenKind, Token, TokenKind};
use crate::span::Span;

use super::statements::{self, DefinedOperator, Only, Rename, Spanned, Stmt, StmtKind};
use super::{eos_or, Classifier, Lookahead, TakeUntil};

impl<'input, 'arena> Classifier<'input, 'arena> {
    /// Called when there's an error parsing an only - attempt to skip to the next only in the list
    fn only_skip_to_next(&mut self) -> Option<()> {
        // advance to next `only` or EOS
        self.take_until(|lookahead| match lookahead {
            Lookahead::Token(&Token {
                kind: TokenKind::Comma,
                ..
            }) => TakeUntil::Stop,
            Lookahead::Token(t) if Self::is_eos(t) => TakeUntil::Stop,
            Lookahead::EOF => TakeUntil::Stop,
            _ => TakeUntil::Continue,
        })
        .unwrap();

        // bumps the final token so that we're either ready to parse the next only or the next
        // statement
        self.bump();

        match self.peek() {
            Some(t) if Self::is_eos(t) => None,
            None => None,
            // Consume the comma so we're ready to parse the next potential "only"
            _ => Some(()),
        }
    }

    /// Parses defined operator from everything after `OPERATOR (`
    fn rest_of_defined_operator(
        &mut self,
        start_span: Span,
    ) -> Result<Spanned<DefinedOperator>, TakeUntil> {
        use statements::{
            AddOp, AndOp, ConcatOp, EquivOp, IntrinsicOperator, MultOp, NotOp, OrOp, PowerOp, RelOp,
        };

        let defined_operator = match self.peek() {
            t @ Some(Token {
                kind: TokenKind::DefinedOperator,
                ..
            }) => {
                // let t = self.bump().unwrap();
                DefinedOperator::DefinedUnaryOrBinaryOp(
                    t.cloned()
                        .unwrap()
                        .try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                )
            }
            Some(Token {
                kind: TokenKind::StarStar,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::PowerOp(PowerOp)),
            Some(Token {
                kind: TokenKind::Star,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::MultOp(MultOp::Multiply)),
            Some(Token {
                kind: TokenKind::Slash,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::MultOp(MultOp::Divide)),
            Some(Token {
                kind: TokenKind::Plus,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::AddOp(AddOp::Plus)),
            Some(Token {
                kind: TokenKind::Minus,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::AddOp(AddOp::Minus)),
            Some(Token {
                kind: TokenKind::SlashSlash,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::ConcatOp(ConcatOp)),
            Some(Token {
                kind: TokenKind::EqualsOp,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::EqualNamed)),
            Some(Token {
                kind: TokenKind::NotEqualsOp,
                ..
            }) => {
                DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::NotEqualNamed))
            }
            Some(Token {
                kind: TokenKind::LessThanOp,
                ..
            }) => {
                DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::LessThanNamed))
            }
            Some(Token {
                kind: TokenKind::LessThanOrEqualsOp,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(
                RelOp::LessThanOrEqualNamed,
            )),
            Some(Token {
                kind: TokenKind::GreaterThanOp,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(
                RelOp::GreaterThanNamed,
            )),
            Some(Token {
                kind: TokenKind::GreaterThanOrEqualsOp,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(
                RelOp::GreaterThanOrEqualNamed,
            )),
            Some(Token {
                kind: TokenKind::EqualsEquals,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::Equal)),
            Some(Token {
                kind: TokenKind::SlashEquals,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::NotEqual)),
            Some(Token {
                kind: TokenKind::LeftAngle,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::LessThan)),
            Some(Token {
                kind: TokenKind::LeftAngleEquals,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(
                RelOp::LessThanOrEqual,
            )),
            Some(Token {
                kind: TokenKind::RightAngle,
                ..
            }) => {
                DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::GreaterThan))
            }
            Some(Token {
                kind: TokenKind::RightAngleEquals,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(
                RelOp::GreaterThanOrEqual,
            )),
            Some(Token {
                kind: TokenKind::NotOp,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::NotOp(NotOp)),
            Some(Token {
                kind: TokenKind::AndOp,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::AndOp(AndOp)),
            Some(Token {
                kind: TokenKind::OrOp,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::OrOp(OrOp)),
            Some(Token {
                kind: TokenKind::EquivalentOp,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::EquivOp(
                EquivOp::Equivalence,
            )),
            Some(Token {
                kind: TokenKind::NotEquivalentOp,
                ..
            }) => DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::EquivOp(
                EquivOp::NonEquivalence,
            )),
            _ => {
                self.emit_expected_token(&[
                    TokenKind::StarStar,
                    TokenKind::Star,
                    TokenKind::Slash,
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::SlashSlash,
                    TokenKind::EqualsOp,
                    TokenKind::NotEqualsOp,
                    TokenKind::LessThanOp,
                    TokenKind::LessThanOrEqualsOp,
                    TokenKind::GreaterThanOp,
                    TokenKind::GreaterThanOrEqualsOp,
                    TokenKind::EqualsEquals,
                    TokenKind::SlashEquals,
                    TokenKind::LeftAngle,
                    TokenKind::LeftAngleEquals,
                    TokenKind::RightAngle,
                    TokenKind::RightAngleEquals,
                    TokenKind::NotOp,
                    TokenKind::AndOp,
                    TokenKind::OrOp,
                    TokenKind::EquivalentOp,
                    TokenKind::NotEquivalentOp,
                ]);

                return Err(if self.only_skip_to_next().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        };

        // consume the defined-operator
        self.bump().unwrap();

        match self.peek() {
            Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) => {
                let end = self.bump().unwrap().span.end;
                Ok(Spanned::new(
                    defined_operator,
                    Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end,
                    },
                ))
            }
            _ => {
                self.emit_expected_token(&[TokenKind::RightParen]);

                Err(if self.only_skip_to_next().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                })
            }
        }
    }
    /// Parses a user-defined operator declaration (e.g. `OPERATOR ( .NAME. )`)
    fn user_defined_operator(&mut self) -> Result<Spanned<InternedName>, TakeUntil> {
        let start_span = match self.peek() {
            Some(Token {
                kind: TokenKind::Keyword(KeywordTokenKind::Operator),
                ..
            }) => self.bump().unwrap().span,
            _ => {
                self.emit_expected_token(&[TokenKind::Keyword(KeywordTokenKind::Operator)]);

                return Err(if self.only_skip_to_next().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        };

        match self.peek() {
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) => {
                self.bump();
            }
            _ => {
                self.emit_expected_token(&[TokenKind::LeftParen]);

                return Err(if self.only_skip_to_next().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        };

        let name = match self.peek() {
            Some(Token {
                kind: TokenKind::DefinedOperator,
                ..
            }) => self
                .bump()
                .unwrap()
                .try_intern_contents(&mut self.interner, &self.text)
                .unwrap(),
            _ => {
                self.emit_expected_token(&[TokenKind::DefinedOperator]);

                return Err(if self.only_skip_to_next().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        };

        match self.peek() {
            Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) => {
                let end = self.bump().unwrap().span.end;

                Ok(Spanned::new(
                    name,
                    Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end,
                    },
                ))
            }
            _ => {
                self.emit_expected_token(&[TokenKind::RightParen]);

                return Err(if self.only_skip_to_next().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        }
    }

    /// Expects that we're at an `only`, but have not yet consumed any tokens of it. Will consume
    /// the `only` all the way up to the comma or EOS
    fn only(&mut self) -> Option<Spanned<Only>> {
        use statements::{DefinedIoGenericSpec, GenericSpec};

        loop {
            // We need to look ahead to see if we're in a rename or generic-spec
            let t = match self.bump() {
                Some(t) if t.is_name() => t,
                _ => {
                    // On unexpected token, emit an error, but still return a statement
                    self.emit_expected_token(&[
                        TokenKind::Name,
                        TokenKind::Keyword(KeywordTokenKind::Operator),
                        TokenKind::Keyword(KeywordTokenKind::Assignment),
                        TokenKind::Keyword(KeywordTokenKind::Read),
                        TokenKind::Keyword(KeywordTokenKind::Write),
                    ]);

                    self.only_skip_to_next()?;
                    continue;
                }
            };

            let start_span = t.span;

            #[derive(Copy, Clone, PartialEq, Eq)]
            enum WhatIsIt {
                OperatorOrRename,
                Assignment,
                Read,
                Write,
                Rename,
            }

            let what_is_it = match self.peek() {
                Some(Token {
                    kind: TokenKind::Arrow,
                    ..
                }) => {
                    self.bump();
                    WhatIsIt::Rename
                }
                Some(Token {
                    kind: TokenKind::LeftParen,
                    ..
                }) => {
                    let what_is_it = match t.kind {
                        TokenKind::Keyword(keyword) => match keyword {
                            KeywordTokenKind::Operator => Some(WhatIsIt::OperatorOrRename),
                            KeywordTokenKind::Assignment => Some(WhatIsIt::Assignment),
                            KeywordTokenKind::Read => Some(WhatIsIt::Read),
                            KeywordTokenKind::Write => Some(WhatIsIt::Write),
                            _ => None,
                        },
                        TokenKind::Name => None,
                        _ => panic!("Internal error: This token was guaranteed to be a name"),
                    };

                    match what_is_it {
                        Some(what_is_it) => {
                            self.bump();
                            what_is_it
                        }
                        None => {
                            // If it's just a plain name, then it should be followed by a comma or
                            // arrow
                            self.emit_expected_token(&[TokenKind::Comma, TokenKind::Arrow]);

                            self.only_skip_to_next()?;
                            continue;
                        }
                    }
                }
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {
                    // We don't consume the comma, but this is just a plain name - return it!
                    let span = t.span;
                    return Some(Spanned::new(
                        Only::GenericOrOnlyUseName(
                            t.try_intern_contents(&mut self.interner, &self.text)
                                .unwrap(),
                        ),
                        span,
                    ));
                }
                Some(t2) if Self::is_eos(t2) => {
                    // We don't consume the EOS, but this is just a plain name - return it!
                    let span = t.span;
                    return Some(Spanned::new(
                        Only::GenericOrOnlyUseName(
                            t.try_intern_contents(&mut self.interner, &self.text)
                                .unwrap(),
                        ),
                        span,
                    ));
                }
                None => {
                    // We don't consume the EOS, but this is just a plain name - return it!
                    let span: Span = t.span;
                    return Some(Spanned::new(
                        Only::GenericOrOnlyUseName(
                            t.try_intern_contents(&mut self.interner, &self.text)
                                .unwrap(),
                        ),
                        span,
                    ));
                }
                _ => {
                    self.emit_expected_token(&[TokenKind::Arrow, TokenKind::Comma]);

                    self.only_skip_to_next()?;
                    continue;
                }
            };

            return Some(match what_is_it {
                WhatIsIt::OperatorOrRename => {
                    let defined_operator = match self.rest_of_defined_operator(start_span) {
                        Ok(defined_operator) => defined_operator,
                        Err(TakeUntil::Continue) => continue,
                        Err(TakeUntil::Stop) => return None,
                        _ => panic!(),
                    };

                    let user_defined_operator = match defined_operator.val {
                        DefinedOperator::DefinedUnaryOrBinaryOp(name) => Some(name),
                        _ => None,
                    };

                    match self.peek() {
                        Some(Token {
                            kind: TokenKind::Comma,
                            ..
                        }) => Spanned::new(
                            Only::GenericSpec(GenericSpec::Operator(defined_operator.val)),
                            defined_operator.span,
                        ),
                        Some(t) if Self::is_eos(t) => Spanned::new(
                            Only::GenericSpec(GenericSpec::Operator(defined_operator.val)),
                            defined_operator.span,
                        ),
                        None => Spanned::new(
                            Only::GenericSpec(GenericSpec::Operator(defined_operator.val)),
                            defined_operator.span,
                        ),
                        Some(Token {
                            kind: TokenKind::Arrow,
                            ..
                        }) if user_defined_operator.is_some() => {
                            // consume arrow
                            self.bump().unwrap();

                            let operator_name = match self.user_defined_operator() {
                                Ok(operator_name) => operator_name,
                                Err(TakeUntil::Continue) => continue,
                                Err(TakeUntil::Stop) => return None,
                                _ => panic!(),
                            };

                            Spanned::new(
                                Only::Rename(Rename::Operator {
                                    from: user_defined_operator.unwrap(),
                                    to: operator_name.val,
                                }),
                                Span {
                                    file_id: self.file_id,
                                    start: start_span.start,
                                    end: operator_name.span.end,
                                },
                            )
                        }
                        _ => {
                            if user_defined_operator.is_some() {
                                self.emit_expected_token(&eos_or(&[
                                    TokenKind::Comma,
                                    TokenKind::Arrow,
                                ]));
                            } else {
                                self.emit_expected_token(&eos_or(&[TokenKind::Comma]));
                            }

                            self.only_skip_to_next()?;
                            continue;
                        }
                    }
                }
                WhatIsIt::Assignment => {
                    match self.peek() {
                        Some(Token {
                            kind: TokenKind::Equals,
                            ..
                        }) => {
                            self.bump();
                        }
                        _ => {
                            self.emit_expected_token(&[TokenKind::Equals]);
                            self.only_skip_to_next()?;
                            continue;
                        }
                    };

                    match self.peek() {
                        Some(Token {
                            kind: TokenKind::RightParen,
                            ..
                        }) => {
                            let end = self.bump().unwrap().span.end;

                            Spanned::new(
                                Only::GenericSpec(GenericSpec::Assignment),
                                Span {
                                    file_id: self.file_id,
                                    start: start_span.start,
                                    end,
                                },
                            )
                        }
                        _ => {
                            self.emit_expected_token(&[TokenKind::RightParen]);

                            self.only_skip_to_next()?;
                            continue;
                        }
                    }
                }
                WhatIsIt::Read | WhatIsIt::Write => {
                    let defined_io_generic_spec = match self.peek() {
                        Some(Token {
                            kind: TokenKind::Keyword(KeywordTokenKind::Formatted),
                            ..
                        }) => {
                            if what_is_it == WhatIsIt::Read {
                                DefinedIoGenericSpec::ReadFormatted
                            } else {
                                DefinedIoGenericSpec::WriteFormatted
                            }
                        }
                        Some(Token {
                            kind: TokenKind::Keyword(KeywordTokenKind::Unformatted),
                            ..
                        }) => {
                            if what_is_it == WhatIsIt::Read {
                                DefinedIoGenericSpec::ReadUnformatted
                            } else {
                                DefinedIoGenericSpec::WriteUnformatted
                            }
                        }
                        _ => {
                            self.emit_expected_token(&[
                                TokenKind::Keyword(KeywordTokenKind::Formatted),
                                TokenKind::Keyword(KeywordTokenKind::Unformatted),
                            ]);

                            self.only_skip_to_next()?;
                            continue;
                        }
                    };

                    // consume formatted/unformatted
                    self.bump();

                    match self.peek() {
                        Some(Token {
                            kind: TokenKind::RightParen,
                            ..
                        }) => {
                            let end = self.bump().unwrap().span.end;

                            Spanned::new(
                                Only::GenericSpec(GenericSpec::DefinedIoGenericSpec(
                                    defined_io_generic_spec,
                                )),
                                Span {
                                    file_id: self.file_id,
                                    start: start_span.start,
                                    end,
                                },
                            )
                        }
                        _ => {
                            self.emit_expected_token(&[TokenKind::RightParen]);

                            self.only_skip_to_next()?;
                            continue;
                        }
                    }
                }
                WhatIsIt::Rename => match self.peek() {
                    Some(t2) if t2.is_name() => {
                        let t2 = self.bump().unwrap();

                        let from = t
                            .try_intern_contents(&mut self.interner, &self.text)
                            .unwrap();
                        let to = t2
                            .try_intern_contents(&mut self.interner, &self.text)
                            .unwrap();

                        let end = t2.span.end;

                        Spanned::new(
                            Only::Rename(Rename::Name { from, to }),
                            Span {
                                file_id: self.file_id,
                                start: start_span.start,
                                end,
                            },
                        )
                    }
                    _ => {
                        self.emit_expected_token(&[TokenKind::Name]);

                        self.only_skip_to_next()?;
                        continue;
                    }
                },
            });
        }
    }

    pub(crate) fn use_statement(&mut self, start_span: &Span) -> Stmt<'arena> {
        use statements::{ModuleImportList, ModuleNature};

        let (module_nature, name, end) = match self.peek() {
            Some(Token {
                kind: TokenKind::Comma,
                ..
            }) => {
                self.bump().unwrap();

                let module_nature = match self.peek() {
                    Some(Token {
                        kind: TokenKind::Keyword(KeywordTokenKind::Intrinsic),
                        ..
                    }) => ModuleNature::Intrinsic,
                    Some(Token {
                        kind: TokenKind::Keyword(KeywordTokenKind::Non_Intrinsic),
                        ..
                    }) => ModuleNature::NonIntrinsic,
                    _ => {
                        return self.expected_token(
                            &[
                                TokenKind::Keyword(KeywordTokenKind::Intrinsic),
                                TokenKind::Keyword(KeywordTokenKind::Non_Intrinsic),
                            ],
                            start_span,
                        );
                    }
                };

                // consume the intrinsic/non_intrinsic
                self.bump().unwrap();

                match self.peek() {
                    Some(Token {
                        kind: TokenKind::ColonColon,
                        ..
                    }) => {
                        self.bump().unwrap();
                    }
                    Some(t) if t.is_name() => {
                        // don't consume, we'll handle the name next
                    }
                    _ => {
                        return self.expected_token(
                            &[
                                TokenKind::ColonColon,
                                TokenKind::Keyword(KeywordTokenKind::Name),
                            ],
                            start_span,
                        );
                    }
                };

                match self.peek() {
                    Some(t) if t.is_name() => {
                        let end = t.span.end;
                        (
                            module_nature,
                            self.bump()
                                .unwrap()
                                .try_intern_contents(&mut self.interner, &self.text)
                                .unwrap(),
                            end,
                        )
                    }
                    _ => {
                        return self.expected_token(
                            &[TokenKind::Keyword(KeywordTokenKind::Name)],
                            start_span,
                        )
                    }
                }
            }
            Some(t) if t.is_name() => {
                let end = t.span.end;
                (
                    ModuleNature::Unspecified,
                    self.bump()
                        .unwrap()
                        .try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    end,
                )
            }
            _ => {
                return self.expected_token(&[TokenKind::Comma, TokenKind::Name], start_span);
            }
        };

        // If we reached EOS, then we have a complete USE statement. Otherwise parse
        // only-list/rename-list
        let is_at_end = match self.peek() {
            Some(t) if Self::is_eos(t) => {
                self.bump();
                true
            }
            None => true,
            Some(Token {
                kind: TokenKind::Comma,
                ..
            }) => {
                self.bump();
                false
            }
            _ => {
                // On unexpected token, emit an error, but still return a statement
                self.emit_expected_token(&eos_or(&[TokenKind::Comma]));

                // advance to end
                self.take_until_eos();

                true
            }
        };

        let unspecified_use = Stmt {
            kind: StmtKind::Use {
                module_nature,
                name,
                imports: ModuleImportList::Unspecified,
            },
            span: Span {
                file_id: self.file_id,
                start: start_span.start,
                end,
            },
        };

        if is_at_end {
            return unspecified_use;
        }

        // We need to look a couple steps ahead to figure out if we're looking at a rename-list or
        // an only-list. If we see `name =>`, then it's a rename list. If we see `only :`, then it's
        // an on-list. We need to look ahead since `only` is also a name - if only Fortran had
        // reserved the keywords!

        let t = match self.bump() {
            Some(t) if t.is_name() => t,
            _ => {
                // On unexpected token, emit an error, but still return a statement
                self.emit_expected_token(&[
                    TokenKind::Name,
                    TokenKind::Keyword(KeywordTokenKind::Only),
                    TokenKind::Keyword(KeywordTokenKind::Operator),
                ]);

                // advance to end
                self.take_until_eos();

                return unspecified_use;
            }
        };

        // Determine if we're in a rename list or an only list
        let is_rename = match self.peek() {
            // Can be either an arrow OR ( in cases when previous token is OPERATOR
            Some(Token {
                kind: TokenKind::Arrow,
                ..
            }) => true,
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) if t.kind == TokenKind::Keyword(KeywordTokenKind::Operator) => true,
            Some(Token {
                kind: TokenKind::Colon,
                ..
            }) if t.kind == TokenKind::Keyword(KeywordTokenKind::Only) => false,
            _ => {
                // On unexpected token, emit an error, but still return a statement
                if t.kind == TokenKind::Keyword(KeywordTokenKind::Operator) {
                    self.emit_expected_token(&[TokenKind::Arrow, TokenKind::LeftParen]);
                } else if t.kind == TokenKind::Keyword(KeywordTokenKind::Only) {
                    self.emit_expected_token(&[TokenKind::Arrow, TokenKind::Colon]);
                } else {
                    self.emit_expected_token(&[TokenKind::Arrow]);
                }

                // advance to end
                self.take_until_eos();

                return unspecified_use;
            }
        };

        if is_rename {
            panic!("Internal compiler error: rename list not implemented!");
            self.arena.renames.alloc_extend(std::iter::from_fn(|| None));

            self.unimplemented(start_span)
        } else {
            debug_assert_eq!(TokenKind::Colon, self.peek().unwrap().kind);

            // consume :
            self.bump().unwrap();

            let mut first = true;

            let onlys = self.arena.onlys.alloc_extend(std::iter::from_fn(|| {
                if !first {
                    match self.peek()? {
                        Token {
                            kind: TokenKind::Comma,
                            ..
                        } => {
                            self.bump();
                        }
                        t if Self::is_eos(t) => {
                            self.bump();
                            return None;
                        }
                        _ => panic!(
                            "Internal compiler error: `only` parsing didn't correctly proceed to \
                             the nearest comma or EOS"
                        ),
                    }
                }
                first = false;
                self.only()
            }));

            Stmt {
                kind: StmtKind::Use {
                    module_nature,
                    name,
                    imports: ModuleImportList::OnlyList(onlys),
                },
                span: Span {
                    file_id: self.file_id,
                    start: start_span.start,
                    end,
                },
            }
        }
    }
}
