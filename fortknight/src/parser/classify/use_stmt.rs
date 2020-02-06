use crate::intern::InternedName;
use crate::parser::lex::{KeywordTokenKind, Token, TokenKind};
use crate::span::Span;

use super::statements::{self, DefinedOperator, Only, Rename, Spanned, Stmt, StmtKind};
use super::{eos_or, Classifier, TakeUntil};

impl<'input, 'arena> Classifier<'input, 'arena> {
    /// Parses defined operator from everything after `OPERATOR (`
    fn rest_of_defined_operator(
        &mut self,
        start_span: Span,
    ) -> Result<Spanned<DefinedOperator>, TakeUntil> {
        use statements::{
            AddOp, AndOp, ConcatOp, EquivOp, IntrinsicOperator, MultOp, NotOp, OrOp, PowerOp, RelOp,
        };

        let defined_operator = match self.tokenizer.peek() {
            t
            @
            Some(Token {
                kind: TokenKind::DefinedOperator,
                ..
            }) => {
                // let t = self.tokenizer.bump().unwrap();
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

                return Err(if self.skip_to_comma_or_eos().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        };

        // consume the defined-operator
        self.tokenizer.bump().unwrap();

        match self.tokenizer.peek() {
            Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) => {
                let end = self.tokenizer.bump().unwrap().span.end;
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

                Err(if self.skip_to_comma_or_eos().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                })
            }
        }
    }
    /// Parses a user-defined operator declaration (e.g. `OPERATOR ( .NAME. )`)
    fn user_defined_operator(&mut self) -> Result<Spanned<InternedName>, TakeUntil> {
        let start_span = match self.tokenizer.peek() {
            Some(Token {
                kind: TokenKind::Keyword(KeywordTokenKind::Operator),
                ..
            }) => self.tokenizer.bump().unwrap().span,
            _ => {
                self.emit_expected_token(&[TokenKind::Keyword(KeywordTokenKind::Operator)]);

                return Err(if self.skip_to_comma_or_eos().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        };

        match self.tokenizer.peek() {
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) => {
                self.tokenizer.bump();
            }
            _ => {
                self.emit_expected_token(&[TokenKind::LeftParen]);

                return Err(if self.skip_to_comma_or_eos().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        };

        let name = match self.tokenizer.peek() {
            Some(Token {
                kind: TokenKind::DefinedOperator,
                ..
            }) => self
                .tokenizer
                .bump()
                .unwrap()
                .try_intern_contents(&mut self.interner, &self.text)
                .unwrap(),
            _ => {
                self.emit_expected_token(&[TokenKind::DefinedOperator]);

                return Err(if self.skip_to_comma_or_eos().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        };

        match self.tokenizer.peek() {
            Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) => {
                let end = self.tokenizer.bump().unwrap().span.end;

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

                return Err(if self.skip_to_comma_or_eos().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            }
        }
    }

    /// Parse the RHS of a non-operator rename (e.g. the "to" in "from => to"). Returns None and
    /// progresses to the next comma or EOS if the upcoming token isn't a name.
    fn rename_rhs(&mut self, from: InternedName, start_span: Span) -> Option<Spanned<Rename>> {
        match self.tokenizer.peek() {
            Some(to) if to.is_name() => {
                let to = self.tokenizer.bump().unwrap();

                let end = to.span.end;
                let to = to
                    .try_intern_contents(&mut self.interner, &self.text)
                    .unwrap();

                Some(Spanned::new(
                    Rename::Name { from, to },
                    Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end,
                    },
                ))
            }
            _ => {
                self.emit_expected_token(&[TokenKind::Name]);

                self.skip_to_comma_or_eos()?;
                None
            }
        }
    }

    /// Parse the RHS of an operator rename (e.g. the "operator(.to.)" in
    /// "operator(.from.) => operator(.to.)"). Returns None and progresses to the next comma or EOS
    /// if the upcoming token isn't a name.
    fn rename_operator_rhs(
        &mut self,
        from: InternedName,
        start_span: Span,
    ) -> Option<Spanned<Rename>> {
        // consume arrow
        self.tokenizer.bump().unwrap();

        let operator_name = self.user_defined_operator().ok()?;

        Some(Spanned::new(
            Rename::Operator {
                from,
                to: operator_name.val,
            },
            Span {
                file_id: self.file_id,
                start: start_span.start,
                end: operator_name.span.end,
            },
        ))
    }

    /// Expects that we're at a `rename` element.
    fn rename(&mut self) -> Option<Spanned<Rename>> {
        loop {
            let t = if self.check_name() {
                self.tokenizer.bump().unwrap()
            } else {
                self.emit_expected_token(&[
                    TokenKind::Name,
                    TokenKind::Keyword(KeywordTokenKind::Operator),
                ]);

                self.skip_to_comma_or_eos()?;
                continue;
            };

            let kind = t.kind;
            let start_span = t.span;

            if self.check(TokenKind::Arrow) {
                self.tokenizer.bump();

                let name = t
                    .try_intern_contents(&mut self.interner, &self.text)
                    .unwrap();

                match self.rename_rhs(name, start_span) {
                    Some(rename) => return Some(rename),
                    None => continue,
                }
            } else if kind == TokenKind::Keyword(KeywordTokenKind::Operator)
                && self.check(TokenKind::LeftParen)
            {
                self.tokenizer.bump();

                let name = if self.check(TokenKind::DefinedOperator) {
                    self.tokenizer
                        .bump()
                        .unwrap()
                        .try_intern_contents(&mut self.interner, &self.text)
                        .unwrap()
                } else {
                    self.emit_expected_token(&[TokenKind::DefinedOperator]);

                    self.skip_to_comma_or_eos()?;
                    continue;
                };

                if self.check(TokenKind::RightParen) {
                    self.tokenizer.bump();
                } else {
                    self.emit_unexpected_token();
                    self.skip_to_comma_or_eos()?;
                    continue;
                }

                if self.check(TokenKind::Arrow) {
                    // rename_operator_rhs consumes the arrow
                } else {
                    self.emit_unexpected_token();
                    self.skip_to_comma_or_eos()?;
                    continue;
                }

                match self.rename_operator_rhs(name, start_span) {
                    Some(rename) => return Some(rename),
                    _ => continue,
                }
            } else {
                self.emit_unexpected_token();
                self.skip_to_comma_or_eos()?;
                continue;
            }
        }
    }

    /// Expects that we're at an `only`, but have not yet consumed any tokens of it. Will consume
    /// the `only` all the way up to the comma or EOS
    fn only(&mut self) -> Option<Spanned<Only>> {
        use statements::{DefinedIoGenericSpec, GenericSpec};

        loop {
            // We need to look ahead to see if we're in a rename or generic-spec
            let t = match self.tokenizer.bump() {
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

                    self.skip_to_comma_or_eos()?;
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

            let what_is_it = match self.tokenizer.peek() {
                Some(Token {
                    kind: TokenKind::Arrow,
                    ..
                }) => {
                    self.tokenizer.bump();
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
                            self.tokenizer.bump();
                            what_is_it
                        }
                        None => {
                            // If it's just a plain name, then it should be followed by a comma or
                            // arrow
                            self.emit_expected_token(&[TokenKind::Comma, TokenKind::Arrow]);

                            self.skip_to_comma_or_eos()?;
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

                    self.skip_to_comma_or_eos()?;
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

                    if self.check(TokenKind::Comma) {
                        Spanned::new(
                            Only::GenericSpec(GenericSpec::Operator(defined_operator.val)),
                            defined_operator.span,
                        )
                    } else if self.check_eos() {
                        Spanned::new(
                            Only::GenericSpec(GenericSpec::Operator(defined_operator.val)),
                            defined_operator.span,
                        )
                    } else if self.tokenizer.peek().is_none() {
                        Spanned::new(
                            Only::GenericSpec(GenericSpec::Operator(defined_operator.val)),
                            defined_operator.span,
                        )
                    } else if user_defined_operator.is_some() && self.check(TokenKind::Arrow) {
                        let rename = match self
                            .rename_operator_rhs(user_defined_operator.unwrap(), start_span)
                        {
                            Some(rename) => rename,
                            _ => continue,
                        };
                        Spanned::new(Only::Rename(rename.val), rename.span)
                    } else {
                        if user_defined_operator.is_some() {
                            self.emit_expected_token(&eos_or(&[
                                TokenKind::Comma,
                                TokenKind::Arrow,
                            ]));
                        } else {
                            self.emit_expected_token(&eos_or(&[TokenKind::Comma]));
                        }

                        self.skip_to_comma_or_eos()?;
                        continue;
                    }
                }
                WhatIsIt::Assignment => {
                    if self.check(TokenKind::Equals) {
                        self.tokenizer.bump();
                    } else {
                        self.emit_unexpected_token();
                        self.skip_to_comma_or_eos()?;
                        continue;
                    }

                    if self.check(TokenKind::RightParen) {
                        let end = self.tokenizer.bump().unwrap().span.end;

                        Spanned::new(
                            Only::GenericSpec(GenericSpec::Assignment),
                            Span {
                                file_id: self.file_id,
                                start: start_span.start,
                                end,
                            },
                        )
                    } else {
                        self.emit_expected_token(&[TokenKind::RightParen]);

                        self.skip_to_comma_or_eos()?;
                        continue;
                    }
                }
                WhatIsIt::Read | WhatIsIt::Write => {
                    let defined_io_generic_spec =
                        if self.check(TokenKind::Keyword(KeywordTokenKind::Formatted)) {
                            if what_is_it == WhatIsIt::Read {
                                DefinedIoGenericSpec::ReadFormatted
                            } else {
                                DefinedIoGenericSpec::WriteFormatted
                            }
                        } else if self.check(TokenKind::Keyword(KeywordTokenKind::Unformatted)) {
                            if what_is_it == WhatIsIt::Read {
                                DefinedIoGenericSpec::ReadUnformatted
                            } else {
                                DefinedIoGenericSpec::WriteUnformatted
                            }
                        } else {
                            self.emit_expected_token(&[
                                TokenKind::Keyword(KeywordTokenKind::Formatted),
                                TokenKind::Keyword(KeywordTokenKind::Unformatted),
                            ]);

                            self.skip_to_comma_or_eos()?;
                            continue;
                        };

                    // consume formatted/unformatted
                    self.tokenizer.bump();

                    if self.check(TokenKind::RightParen) {
                        let end = self.tokenizer.bump().unwrap().span.end;

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
                    } else {
                        self.emit_expected_token(&[TokenKind::RightParen]);

                        self.skip_to_comma_or_eos()?;
                        continue;
                    }
                }
                WhatIsIt::Rename => {
                    let name = t
                        .try_intern_contents(&mut self.interner, &self.text)
                        .unwrap();
                    match self.rename_rhs(name, start_span) {
                        Some(rename) => Spanned::new(Only::Rename(rename.val), rename.span),
                        None => continue,
                    }
                }
            });
        }
    }

    pub(super) fn use_statement(&mut self, start_span: &Span) -> Stmt<'arena> {
        use statements::{ModuleImportList, ModuleNature};

        let (module_nature, name, end) = if self.check(TokenKind::Comma) {
            self.tokenizer.bump().unwrap();

            let module_nature = if self.check(TokenKind::Keyword(KeywordTokenKind::Intrinsic)) {
                ModuleNature::Intrinsic
            } else if self.check(TokenKind::Keyword(KeywordTokenKind::Non_Intrinsic)) {
                ModuleNature::NonIntrinsic
            } else {
                return self.unexpected_token(start_span);
            };

            // consume the intrinsic/non_intrinsic
            self.tokenizer.bump().unwrap();

            if self.check(TokenKind::ColonColon) {
                self.tokenizer.bump();
            } else if self.check_name() {
                // don't consume, we'll handle the name next
            } else {
                return self.unexpected_token(start_span);
            }

            if self.check_name() {
                let t = self.tokenizer.bump().unwrap();
                (
                    module_nature,
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span.end,
                )
            } else {
                return self.unexpected_token(start_span);
            }
        } else if self.check_name() {
            let t = self.tokenizer.bump().unwrap();
            (
                ModuleNature::Unspecified,
                t.try_intern_contents(&mut self.interner, &self.text)
                    .unwrap(),
                t.span.end,
            )
        } else {
            return self.unexpected_token(start_span);
        };

        // If we reached EOS, then we have a complete USE statement. Otherwise parse
        // only-list/rename-list
        let is_at_end = if self.check_eos() {
            self.tokenizer.bump();
            true
        } else if self.check(TokenKind::Comma) {
            self.tokenizer.bump();
            false
        } else if self.tokenizer.peek().is_none() {
            true
        } else {
            // On unexpected token, emit an error, but still return a statement
            self.emit_unexpected_token();

            // advance to end
            self.take_until_eos();

            true
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
        // an only-list. We need to look ahead since `only` is also a name - if only Fortran had
        // reserved the keywords!

        let t = if self.check_name() {
            self.tokenizer.bump().unwrap()
        } else {
            // On unexpected token, emit an error, but still return a statement
            self.emit_unexpected_token();

            // advance to end
            self.take_until_eos();

            return unspecified_use;
        };

        enum WhatIsIt {
            NameRename(InternedName),
            OperatorRename(InternedName),
            OnlyList,
        };

        // Determine if we're in a rename list or an only list
        let what_is_it = if self.check(TokenKind::Arrow) {
            self.tokenizer.bump();
            WhatIsIt::NameRename(
                t.try_intern_contents(&mut self.interner, &self.text)
                    .unwrap(),
            )
        } else if t.kind == TokenKind::Keyword(KeywordTokenKind::Operator)
            && self.check(TokenKind::LeftParen)
        {
            self.tokenizer.bump();

            let name = if self.check(TokenKind::DefinedOperator) {
                self.tokenizer
                    .bump()
                    .unwrap()
                    .try_intern_contents(&mut self.interner, &self.text)
                    .unwrap()
            } else {
                self.emit_unexpected_token();
                self.take_until_eos();
                return unspecified_use;
            };

            if self.check(TokenKind::RightParen) {
                self.tokenizer.bump();
            } else {
                self.emit_unexpected_token();
                self.take_until_eos();
                return unspecified_use;
            }

            WhatIsIt::OperatorRename(name)
        } else if t.kind == TokenKind::Keyword(KeywordTokenKind::Only)
            && self.check(TokenKind::Colon)
        {
            WhatIsIt::OnlyList
        } else {
            // On unexpected token, emit an error, but still return a statement
            self.emit_unexpected_token();

            // advance to end
            self.take_until_eos();

            return unspecified_use;
        };

        enum FirstRenameOrOnly {
            Rename(Option<Spanned<Rename>>),
            Only,
        };

        let first_rename_or_only = match what_is_it {
            WhatIsIt::NameRename(name) => FirstRenameOrOnly::Rename(self.rename_rhs(name, t.span)),
            WhatIsIt::OperatorRename(name) => {
                FirstRenameOrOnly::Rename(self.rename_operator_rhs(name, t.span))
            }
            WhatIsIt::OnlyList => FirstRenameOrOnly::Only,
        };

        match first_rename_or_only {
            FirstRenameOrOnly::Rename(first) => {
                let renames =
                    self.arena
                        .renames
                        .alloc_extend(first.into_iter().chain(std::iter::from_fn(|| {
                            if self.check(TokenKind::Comma) {
                                self.tokenizer.bump();
                            } else if self.check_eos() {
                                self.tokenizer.bump();
                                return None;
                            } else {
                                panic!(
                                    "Internal compiler error: `only` parsing didn't correctly \
                                     proceed to the nearest comma or EOS"
                                );
                            }

                            self.rename()
                        })));

                Stmt {
                    kind: StmtKind::Use {
                        module_nature,
                        name,
                        imports: ModuleImportList::RenameList(renames),
                    },
                    span: Span {
                        file_id: self.file_id,
                        start: start_span.start,
                        end,
                    },
                }
            }
            FirstRenameOrOnly::Only => {
                debug_assert_eq!(TokenKind::Colon, self.tokenizer.peek().unwrap().kind);

                // consume :
                self.tokenizer.bump().unwrap();

                let mut first = true;

                let onlys = self.arena.onlys.alloc_extend(std::iter::from_fn(|| {
                    if !first {
                        if self.check(TokenKind::Comma) {
                            self.tokenizer.bump();
                        } else if self.check_eos() {
                            self.tokenizer.bump();
                            return None;
                        } else {
                            panic!(
                                "Internal compiler error: `only` parsing didn't correctly proceed to \
                                the nearest comma or EOS"
                            );
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
}
