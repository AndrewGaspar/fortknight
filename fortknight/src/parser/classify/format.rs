use crate::parser::lex::{FormatSpecifier, KeywordTokenKind, LexMode, TokenKind};
use crate::span::Span;

use super::statements::{
    self, DataEditDesc, FormatItem, FormatSpecification, FormatStmt, Stmt, StmtKind,
    UnlimitedFormatItem,
};
use super::Classifier;

impl<'input, 'arena> Classifier<'input, 'arena> {
    pub(super) fn format(&mut self) -> Stmt<'arena> {
        let end_span = Span {
            file_id: self.file_id,
            start: self.text_len(),
            end: self.text_len(),
        };
        let start_span = self.tokenizer.peek().map_or(end_span, |t| t.span);

        expect!(
            self,
            start_span,
            TokenKind::Keyword(KeywordTokenKind::Format)
        );

        expect!(self, start_span, TokenKind::LeftParen);
        self.tokenizer.set_lex_mode(LexMode::Format);
        let format_specification = self.format_specification();
        self.tokenizer.set_lex_mode(LexMode::Normal);
        let end_span = expect!(self, start_span, TokenKind::RightParen).span;
        expect_eos!(self, start_span);

        return Stmt {
            kind: StmtKind::Format(FormatStmt(format_specification)),
            span: start_span.concat(end_span),
        };
    }

    /// R1302: format-specification
    ///     is ( [ format-items ] )
    ///     or ( [ format-items , ] unlimited-format-item )
    fn format_specification(&mut self) -> FormatSpecification<'arena> {
        // early return
        if self.check(TokenKind::RightParen) {
            return FormatSpecification {
                format_items: &[],
                unlimited_format_item: None,
            };
        }

        let first_fi = if self.check(TokenKind::Star) {
            None
        } else {
            match self.format_item() {
                Some(fi) => Some(fi),
                None => {
                    self.skip_to_comma_or_eos_or_terminal(&[TokenKind::RightParen]);
                    None
                }
            }
        };

        let format_items = self.arena.format_items.alloc_extend(
            first_fi
                .into_iter()
                .chain(std::iter::from_fn(|| loop {
                    if let Some(_) = self.check_and_bump(TokenKind::Comma) {
                    } else if self.check(TokenKind::RightParen) {
                        break None;
                    } else {
                        self.emit_unexpected_token();
                        break None;
                    }

                    if self.check(TokenKind::Star) {
                        break None;
                    } else {
                        match self.format_item() {
                            Some(fi) => break Some(fi),
                            None => {
                                self.skip_to_comma_or_eos_or_terminal(&[TokenKind::RightParen]);

                                if self.check_eos() {
                                    break None;
                                }
                            }
                        }
                    }
                }))
                .fuse(),
        );

        let unlimited_format_item = if self.check(TokenKind::Star) {
            self.unlimited_format_item()
        } else {
            None
        };

        if !self.check(TokenKind::RightParen) {
            self.emit_unexpected_token();
        }

        FormatSpecification {
            format_items,
            unlimited_format_item,
        }
    }

    /// R1304: format-item
    ///     is [ r ] data-edit-desc
    ///     or control-edit-desc
    ///     or char-string-edit-desc
    ///     or [ r ] ( format-items )
    fn format_item(&mut self) -> Option<FormatItem<'arena>> {
        if let Some(r) = self.check_and_bump_r() {
            if let Some(d) = self.check_and_bump_data_edit_desc() {
                Some(FormatItem::DataEditDesc(Some(r), d))
            } else {
                todo!()
            }
        } else {
            if let Some(d) = self.check_and_bump_data_edit_desc() {
                Some(FormatItem::DataEditDesc(None, d))
            } else {
                todo!()
            }
        }
    }

    /// R1305: unlimited-format-item is * ( format-items )
    fn unlimited_format_item(&mut self) -> Option<UnlimitedFormatItem<'arena>> {
        None
    }

    /// R1306: r is int-literal-constant
    fn check_and_bump_r(&mut self) -> Option<statements::R<'arena>> {
        let i = self.check_and_bump_int_literal_constant(false)?;
        Some(statements::R(i.val))
    }

    /// R1307: data-edit-desc
    fn check_and_bump_data_edit_desc(&mut self) -> Option<DataEditDesc<'arena>> {
        macro_rules! check_ded {
            ($i:ident) => {
                self.check_and_bump(TokenKind::FormatSpecifier(FormatSpecifier::$i))
            };
        };

        if let Some(_) = check_ded!(I) {
            let w = self.expect_w()?;
            let m = if let Some(_) = self.check_and_bump(TokenKind::Dot) {
                self.expect_m()
            } else {
                None
            };
            Some(DataEditDesc::I(w, m))
        } else if let Some(_) = check_ded!(B) {
            let w = self.expect_w()?;
            let m = if let Some(_) = self.check_and_bump(TokenKind::Dot) {
                self.expect_m()
            } else {
                None
            };
            Some(DataEditDesc::B(w, m))
        } else if let Some(_) = check_ded!(O) {
            let w = self.expect_w()?;
            let m = if let Some(_) = self.check_and_bump(TokenKind::Dot) {
                self.expect_m()
            } else {
                None
            };
            Some(DataEditDesc::O(w, m))
        } else if let Some(_) = check_ded!(Z) {
            let w = self.expect_w()?;
            let m = if let Some(_) = self.check_and_bump(TokenKind::Dot) {
                self.expect_m()
            } else {
                None
            };
            Some(DataEditDesc::Z(w, m))
        } else if let Some(_) = check_ded!(F) {
            let w = self.expect_w()?;
            self.expect(TokenKind::Dot);
            let d = self.expect_d()?;
            Some(DataEditDesc::F(w, d))
        } else if let Some(_) = check_ded!(E) {
            let w = self.expect_w()?;
            self.expect(TokenKind::Dot);
            let d = self.expect_d()?;
            let e = if let Some(_) =
                self.check_and_bump(TokenKind::FormatSpecifier(FormatSpecifier::E))
            {
                self.expect_e()
            } else {
                None
            };
            Some(DataEditDesc::E(w, d, e))
        } else if let Some(_) = check_ded!(EN) {
            let w = self.expect_w()?;
            self.expect(TokenKind::Dot);
            let d = self.expect_d()?;
            let e = if let Some(_) =
                self.check_and_bump(TokenKind::FormatSpecifier(FormatSpecifier::E))
            {
                self.expect_e()
            } else {
                None
            };
            Some(DataEditDesc::EN(w, d, e))
        } else if let Some(_) = check_ded!(ES) {
            let w = self.expect_w()?;
            self.expect(TokenKind::Dot);
            let d = self.expect_d()?;
            let e = if let Some(_) =
                self.check_and_bump(TokenKind::FormatSpecifier(FormatSpecifier::E))
            {
                self.expect_e()
            } else {
                None
            };
            Some(DataEditDesc::ES(w, d, e))
        } else if let Some(_) = check_ded!(EX) {
            let w = self.expect_w()?;
            self.expect(TokenKind::Dot);
            let d = self.expect_d()?;
            let e = if let Some(_) =
                self.check_and_bump(TokenKind::FormatSpecifier(FormatSpecifier::E))
            {
                self.expect_e()
            } else {
                None
            };
            Some(DataEditDesc::EX(w, d, e))
        } else if let Some(_) = check_ded!(G) {
            let w = self.expect_w()?;
            let de = if let Some(_) = self.check_and_bump(TokenKind::Dot) {
                if let Some(d) = self.expect_d() {
                    let e = if let Some(_) =
                        self.check_and_bump(TokenKind::FormatSpecifier(FormatSpecifier::E))
                    {
                        self.expect_e()
                    } else {
                        None
                    };

                    Some((d, e))
                } else {
                    None
                }
            } else {
                None
            };
            Some(DataEditDesc::G(w, de))
        } else if let Some(_) = check_ded!(L) {
            let w = self.expect_w()?;
            Some(DataEditDesc::L(w))
        } else if let Some(_) = check_ded!(A) {
            Some(DataEditDesc::A(self.check_and_bump_w()))
        } else if let Some(_) = check_ded!(D) {
            let w = self.expect_w()?;
            self.expect(TokenKind::Dot)?;
            let d = self.expect_d()?;
            Some(DataEditDesc::D(w, d))
        } else if let Some(_) = check_ded!(DT) {
            let cli = self
                .check_and_bump_char_literal_constant(false)
                .map(|c| c.val);
            let vs: Option<&[_]> = if let Some(_) = self.check_and_bump(TokenKind::LeftParen) {
                let first = self.expect_v();
                Some(
                    self.arena
                        .vs
                        .alloc_extend(first.into_iter().chain(std::iter::from_fn(|| loop {
                            if let Some(_) = self.check_and_bump(TokenKind::Comma) {
                            } else if let Some(_) = self.check_and_bump(TokenKind::RightParen) {
                                return None;
                            } else {
                                self.emit_unexpected_token();
                                return None;
                            }

                            if let Some(v) = self.expect_v() {
                                break Some(v);
                            } else {
                                self.skip_to_comma_or_eos_or_terminal(&[TokenKind::RightParen]);
                                continue;
                            }
                        }))),
                )
            } else {
                None
            };
            Some(DataEditDesc::DT(cli, vs))
        } else {
            None
        }
    }

    /// R1308: w is int-literal-constant
    fn check_and_bump_w(&mut self) -> Option<statements::W<'arena>> {
        let i = self.check_and_bump_int_literal_constant(false)?;
        Some(statements::W(i.val))
    }

    /// R1308: w is int-literal-constant
    fn expect_w(&mut self) -> Option<statements::W<'arena>> {
        let i = self.expect_int_literal_constant(false)?;
        Some(statements::W(i.val))
    }

    /// R1309 m is int-literal-constant
    fn expect_m(&mut self) -> Option<statements::M<'arena>> {
        let i = self.expect_int_literal_constant(false)?;
        Some(statements::M(i.val))
    }

    /// R1310 d is int-literal-constant
    fn expect_d(&mut self) -> Option<statements::D<'arena>> {
        let i = self.expect_int_literal_constant(false)?;
        Some(statements::D(i.val))
    }

    /// R1310 e is int-literal-constant
    fn expect_e(&mut self) -> Option<statements::E<'arena>> {
        let i = self.expect_int_literal_constant(false)?;
        Some(statements::E(i.val))
    }

    /// R1310 v is signed-int-literal-constant
    fn expect_v(&mut self) -> Option<statements::V<'arena>> {
        let i = self.expect_signed_int_literal_constant(false)?;
        Some(statements::V(i.val))
    }
}
