use crate::parser::lex::{Token, TokenKind};

use super::{Classifier, TakeUntil};

pub enum CommaResult<T> {
    Some(T),
    Stop,
    Err,
}

impl<'input, 'arena> Classifier<'input, 'arena> {
    // pub(super) fn comma_list<T, F>(
    //     &mut self,
    //     terminals: &'static [TokenKind],
    //     f: F,
    // ) -> impl Iterator<Item = T>
    // where
    //     F: Fn(&mut Self) -> CommaResult<T>,
    // {
    //     let parse_value = move |c: &mut Self| -> Option<T> {
    //         loop {
    //             match f(c) {
    //                 CommaResult::Some(val) => break Some(val),
    //                 CommaResult::Stop => break None,
    //                 CommaResult::Err => {
    //                     c.skip_past_comma_or_eos_or_terminal(terminals);
    //                     if let Some(_) = c.check_and_bump(TokenKind::Comma) {
    //                         continue;
    //                     } else {
    //                         break None;
    //                     }
    //                 }
    //             }
    //         }
    //     };

    //     let first = parse_value(self);

    //     first
    //         .into_iter()
    //         .chain(std::iter::from_fn(move || parse_value(self)))
    //         .fuse()
    // }

    /// Called when there's an error parsing some comma separated list - attempt to skip to the next
    /// item in the list, or to the end. Returns Some(()) when reaching a comma, returns None when
    /// reaching any token in `terminal`.
    ///
    /// The terminal is not consumed
    pub(super) fn skip_to_comma_or_eos_or_terminal(&mut self, terminals: &[TokenKind]) {
        // advance to next `only` or EOS
        self.take_until(|lookahead| match lookahead {
            Some(&Token {
                kind: TokenKind::Comma,
                ..
            }) => TakeUntil::Stop,
            Some(t) if Self::is_eos(t) || terminals.contains(&t.kind) => TakeUntil::Stop,
            None => TakeUntil::Stop,
            _ => TakeUntil::Continue,
        })
        .unwrap();
    }

    /// Called when there's an error parsing some comma separated list - attempt to skip to the next
    /// item in the list, or to the end. Returns Some(()) when reaching a comma, returns None when
    /// reached EOS.
    ///
    /// The comma/EOS is consumed
    pub(super) fn skip_to_comma_or_eos(&mut self) {
        // advance to next `only` or EOS
        self.take_until(|lookahead| match lookahead {
            Some(&Token {
                kind: TokenKind::Comma,
                ..
            }) => TakeUntil::Stop,
            Some(t) if Self::is_eos(t) => TakeUntil::Stop,
            None => TakeUntil::Stop,
            _ => TakeUntil::Continue,
        })
        .unwrap();
    }

    /// Called when there's an error parsing some comma separated list - attempt to skip to the next
    /// item in the list, or to the end. Returns Some(()) when reaching a comma, returns None when
    /// reached EOS.
    ///
    /// The comma/EOS is consumed
    pub(super) fn skip_past_comma_or_eos(&mut self) -> Option<()> {
        // advance to next item or EOS
        self.skip_to_comma_or_eos();

        // bumps the final token so that we're either ready to parse the next only or the next
        // statement
        if let Some(_) = self.check_eos_and_bump() {
            None
        } else {
            self.tokenizer.bump();
            Some(())
        }
    }
}
