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

        let defined_operator = if let Some(t) = self.check_and_bump(TokenKind::DefinedOperator) {
            DefinedOperator::DefinedUnaryOrBinaryOp(
                t.try_intern_contents(&mut self.interner, &self.text)
                    .unwrap(),
            )
        } else if let Some(_) = self.check_and_bump(TokenKind::StarStar) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::PowerOp(PowerOp))
        } else if let Some(_) = self.check_and_bump(TokenKind::Star) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::MultOp(MultOp::Multiply))
        } else if let Some(_) = self.check_and_bump(TokenKind::Slash) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::MultOp(MultOp::Divide))
        } else if let Some(_) = self.check_and_bump(TokenKind::Plus) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::AddOp(AddOp::Plus))
        } else if let Some(_) = self.check_and_bump(TokenKind::Minus) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::AddOp(AddOp::Minus))
        } else if let Some(_) = self.check_and_bump(TokenKind::SlashSlash) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::ConcatOp(ConcatOp))
        } else if let Some(_) = self.check_and_bump(TokenKind::EqualsOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::EqualNamed))
        } else if let Some(_) = self.check_and_bump(TokenKind::NotEqualsOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::NotEqualNamed))
        } else if let Some(_) = self.check_and_bump(TokenKind::LessThanOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::LessThanNamed))
        } else if let Some(_) = self.check_and_bump(TokenKind::LessThanOrEqualsOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(
                RelOp::LessThanOrEqualNamed,
            ))
        } else if let Some(_) = self.check_and_bump(TokenKind::GreaterThanOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::GreaterThanNamed))
        } else if let Some(_) = self.check_and_bump(TokenKind::GreaterThanOrEqualsOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(
                RelOp::GreaterThanOrEqualNamed,
            ))
        } else if let Some(_) = self.check_and_bump(TokenKind::EqualsEquals) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::Equal))
        } else if let Some(_) = self.check_and_bump(TokenKind::SlashEquals) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::NotEqual))
        } else if let Some(_) = self.check_and_bump(TokenKind::LeftAngle) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::LessThan))
        } else if let Some(_) = self.check_and_bump(TokenKind::LeftAngleEquals) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::LessThanOrEqual))
        } else if let Some(_) = self.check_and_bump(TokenKind::RightAngle) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(RelOp::GreaterThan))
        } else if let Some(_) = self.check_and_bump(TokenKind::RightAngleEquals) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::RelOp(
                RelOp::GreaterThanOrEqual,
            ))
        } else if let Some(_) = self.check_and_bump(TokenKind::NotOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::NotOp(NotOp))
        } else if let Some(_) = self.check_and_bump(TokenKind::AndOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::AndOp(AndOp))
        } else if let Some(_) = self.check_and_bump(TokenKind::OrOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::OrOp(OrOp))
        } else if let Some(_) = self.check_and_bump(TokenKind::EquivalentOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::EquivOp(EquivOp::Equivalence))
        } else if let Some(_) = self.check_and_bump(TokenKind::NotEquivalentOp) {
            DefinedOperator::ExtendedIntrinsicOp(IntrinsicOperator::EquivOp(
                EquivOp::NonEquivalence,
            ))
        } else {
            self.emit_unexpected_token();

            return Err(if self.skip_to_comma_or_eos().is_none() {
                TakeUntil::Stop
            } else {
                TakeUntil::Continue
            });
        };

        if let Some(t) = self.check_and_bump(TokenKind::RightParen) {
            let end = t.span.end;
            Ok(Spanned::new(
                defined_operator,
                Span {
                    file_id: self.file_id,
                    start: start_span.start,
                    end,
                },
            ))
        } else {
            self.emit_unexpected_token();

            Err(if self.skip_to_comma_or_eos().is_none() {
                TakeUntil::Stop
            } else {
                TakeUntil::Continue
            })
        }
    }

    /// Parses a user-defined operator declaration (e.g. `OPERATOR ( .NAME. )`)
    fn user_defined_operator(&mut self) -> Result<Spanned<InternedName>, TakeUntil> {
        let start_span =
            if let Some(t) = self.check_and_bump(TokenKind::Keyword(KeywordTokenKind::Operator)) {
                t.span
            } else {
                self.emit_unexpected_token();

                return Err(if self.skip_to_comma_or_eos().is_none() {
                    TakeUntil::Stop
                } else {
                    TakeUntil::Continue
                });
            };

        if let Some(_) = self.check_and_bump(TokenKind::LeftParen) {
        } else {
            self.emit_unexpected_token();

            return Err(if self.skip_to_comma_or_eos().is_none() {
                TakeUntil::Stop
            } else {
                TakeUntil::Continue
            });
        };

        let name = if let Some(t) = self.check_and_bump(TokenKind::DefinedOperator) {
            t.try_intern_contents(&mut self.interner, &self.text)
                .unwrap()
        } else {
            self.emit_unexpected_token();

            return Err(if self.skip_to_comma_or_eos().is_none() {
                TakeUntil::Stop
            } else {
                TakeUntil::Continue
            });
        };

        if let Some(t) = self.check_and_bump(TokenKind::RightParen) {
            let end = t.span.end;

            Ok(Spanned::new(
                name,
                Span {
                    file_id: self.file_id,
                    start: start_span.start,
                    end,
                },
            ))
        } else {
            self.emit_unexpected_token();

            return Err(if self.skip_to_comma_or_eos().is_none() {
                TakeUntil::Stop
            } else {
                TakeUntil::Continue
            });
        }
    }

    /// Parse the RHS of a non-operator rename (e.g. the "to" in "from => to"). Returns None and
    /// progresses to the next comma or EOS if the upcoming token isn't a name.
    fn rename_rhs(&mut self, from: InternedName, start_span: Span) -> Option<Spanned<Rename>> {
        if let Some(t) = self.check_name_and_bump() {
            let end = t.span.end;
            let to = t
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
        } else {
            self.emit_unexpected_token();

            self.skip_to_comma_or_eos()?;
            None
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
        if let Some(_) = self.check_and_bump(TokenKind::Arrow) {
        } else {
            self.emit_unexpected_token();
            return None;
        }

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
                self.emit_unexpected_token();

                self.skip_to_comma_or_eos()?;
                continue;
            };

            let kind = t.kind;
            let start_span = t.span;

            if let Some(_) = self.check_and_bump(TokenKind::Arrow) {
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

                let name = if let Some(t) = self.check_and_bump(TokenKind::DefinedOperator) {
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap()
                } else {
                    self.emit_unexpected_token();

                    self.skip_to_comma_or_eos()?;
                    continue;
                };

                if let Some(_) = self.check_and_bump(TokenKind::RightParen) {
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
            let t = if let Some(t) = self.check_name_and_bump() {
                t
            } else {
                // On unexpected token, emit an error, but still return a statement
                self.emit_unexpected_token();

                self.skip_to_comma_or_eos()?;
                continue;
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

            let what_is_it = if let Some(_) = self.check_and_bump(TokenKind::Arrow) {
                WhatIsIt::Rename
            } else if self.check(TokenKind::Comma) {
                // We don't consume the comma, but this is just a plain name - return it!
                let span = t.span;
                return Some(Spanned::new(
                    Only::GenericOrOnlyUseName(
                        t.try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                    ),
                    span,
                ));
            } else if self.check_eos() {
                // We don't consume the EOS, but this is just a plain name - return it!
                let span = t.span;
                return Some(Spanned::new(
                    Only::GenericOrOnlyUseName(
                        t.try_intern_contents(&mut self.interner, &self.text)
                            .unwrap(),
                    ),
                    span,
                ));
            } else if let Some(_) = self.check_and_bump(TokenKind::LeftParen) {
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
                    Some(what_is_it) => what_is_it,
                    None => {
                        // If it's just a plain name, then it should be followed by a comma or
                        // arrow
                        self.emit_unexpected_token();
                        self.skip_to_comma_or_eos()?;
                        continue;
                    }
                }
            } else {
                self.emit_unexpected_token();
                self.skip_to_comma_or_eos()?;
                continue;
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
                        self.emit_unexpected_token();
                        self.skip_to_comma_or_eos()?;
                        continue;
                    }
                }
                WhatIsIt::Assignment => {
                    if let Some(_) = self.check_and_bump(TokenKind::Equals) {
                    } else {
                        self.emit_unexpected_token();
                        self.skip_to_comma_or_eos()?;
                        continue;
                    }

                    if let Some(t) = self.check_and_bump(TokenKind::RightParen) {
                        let end = t.span.end;

                        Spanned::new(
                            Only::GenericSpec(GenericSpec::Assignment),
                            Span {
                                file_id: self.file_id,
                                start: start_span.start,
                                end,
                            },
                        )
                    } else {
                        self.emit_unexpected_token();
                        self.skip_to_comma_or_eos()?;
                        continue;
                    }
                }
                WhatIsIt::Read | WhatIsIt::Write => {
                    let defined_io_generic_spec = if let Some(_) =
                        self.check_and_bump(TokenKind::Keyword(KeywordTokenKind::Formatted))
                    {
                        if what_is_it == WhatIsIt::Read {
                            DefinedIoGenericSpec::ReadFormatted
                        } else {
                            DefinedIoGenericSpec::WriteFormatted
                        }
                    } else if let Some(_) =
                        self.check_and_bump(TokenKind::Keyword(KeywordTokenKind::Unformatted))
                    {
                        if what_is_it == WhatIsIt::Read {
                            DefinedIoGenericSpec::ReadUnformatted
                        } else {
                            DefinedIoGenericSpec::WriteUnformatted
                        }
                    } else {
                        self.emit_unexpected_token();
                        self.skip_to_comma_or_eos()?;
                        continue;
                    };

                    if let Some(t) = self.check_and_bump(TokenKind::RightParen) {
                        let end = t.span.end;

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
                        self.emit_unexpected_token();
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

        let (module_nature, name, end) = if let Some(_) = self.check_and_bump(TokenKind::Comma) {
            let module_nature = if let Some(_) =
                self.check_and_bump(TokenKind::Keyword(KeywordTokenKind::Intrinsic))
            {
                ModuleNature::Intrinsic
            } else if let Some(_) =
                self.check_and_bump(TokenKind::Keyword(KeywordTokenKind::Non_Intrinsic))
            {
                ModuleNature::NonIntrinsic
            } else {
                return self.unexpected_token(start_span);
            };

            if let Some(_) = self.check_and_bump(TokenKind::ColonColon) {
            } else if self.check_name() {
                // don't consume, we'll handle the name next
            } else {
                return self.unexpected_token(start_span);
            }

            if let Some(t) = self.check_name_and_bump() {
                (
                    module_nature,
                    t.try_intern_contents(&mut self.interner, &self.text)
                        .unwrap(),
                    t.span.end,
                )
            } else {
                return self.unexpected_token(start_span);
            }
        } else if let Some(t) = self.check_name_and_bump() {
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
        let is_at_end = if let Some(_) = self.check_eos_and_bump() {
            true
        } else if let Some(_) = self.check_and_bump(TokenKind::Comma) {
            false
        } else {
            // On unexpected token, emit an error, but still return a statement
            self.emit_unexpected_token();
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

        let t = if let Some(t) = self.check_name_and_bump() {
            t
        } else {
            // On unexpected token, emit an error, but still return a statement
            self.emit_unexpected_token();
            self.take_until_eos();
            return unspecified_use;
        };

        enum WhatIsIt {
            NameRename(InternedName),
            OperatorRename(InternedName),
            OnlyList,
        };

        // Determine if we're in a rename list or an only list
        let what_is_it = if let Some(_) = self.check_and_bump(TokenKind::Arrow) {
            WhatIsIt::NameRename(
                t.try_intern_contents(&mut self.interner, &self.text)
                    .unwrap(),
            )
        } else if t.kind == TokenKind::Keyword(KeywordTokenKind::Operator)
            && self.check_and_bump(TokenKind::LeftParen).is_some()
        {
            let name = if let Some(t) = self.check_and_bump(TokenKind::DefinedOperator) {
                t.try_intern_contents(&mut self.interner, &self.text)
                    .unwrap()
            } else {
                self.emit_unexpected_token();
                self.take_until_eos();
                return unspecified_use;
            };

            if let Some(_) = self.check_and_bump(TokenKind::RightParen) {
            } else {
                self.emit_unexpected_token();
                self.take_until_eos();
                return unspecified_use;
            }

            WhatIsIt::OperatorRename(name)
        } else if t.kind == TokenKind::Keyword(KeywordTokenKind::Only)
            && self.check_and_bump(TokenKind::Colon).is_some()
        {
            WhatIsIt::OnlyList
        } else {
            // On unexpected token, emit an error, but still return a statement
            self.emit_unexpected_token();
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
                let mut first = true;

                let onlys = self.arena.onlys.alloc_extend(std::iter::from_fn(|| {
                    if !first {
                        if let Some(_) = self.check_and_bump(TokenKind::Comma) {
                        } else if let Some(_) = self.check_eos_and_bump() {
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
