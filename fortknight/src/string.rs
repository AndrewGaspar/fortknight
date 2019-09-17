use std::fmt::{Display, Formatter, Write};

#[derive(Clone, Debug)]
pub struct ContinuationStr<'input> {
    string: &'input str,
}

impl<'input> ContinuationStr<'input> {
    pub fn new(string: &'input str) -> ContinuationStr {
        debug_assert!(string.is_ascii());

        ContinuationStr { string: string }
    }

    pub fn iter(&self) -> ContinuationStrChars<'input> {
        self.chars()
    }

    pub fn chars(&self) -> ContinuationStrChars<'input> {
        ContinuationStrChars::new(self.string.chars())
    }

    pub fn char_indices(&self) -> ContinuationStrCharIndices<'input> {
        ContinuationStrCharIndices::new(self.string.char_indices())
    }
}

impl<'input> PartialEq for ContinuationStr<'input> {
    fn eq(&self, other: &ContinuationStr) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<'input> Eq for ContinuationStr<'input> {}

impl<'input> Display for ContinuationStr<'input> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        for c in self.iter() {
            f.write_char(c)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct ContinuationStrChars<'input> {
    str_iter: std::str::Chars<'input>,
}

impl<'input> ContinuationStrChars<'input> {
    fn new(str_iter: std::str::Chars<'input>) -> ContinuationStrChars<'input> {
        ContinuationStrChars { str_iter: str_iter }
    }
}

// Iterator over a FortranUserStr. Ignores continuation. This allows us to
// tokenize the FORTRAN program without allocating any memory.
impl<'input> Iterator for ContinuationStrChars<'input> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        // if we're here, we can assume that the string is already a
        // valid identifier, which means the continuation is properly
        // terminated. Just continue until we see a closing ampersand.
        loop {
            return match self.str_iter.next() {
                Some(amp) if amp == '&' => {
                    while '&' != self.str_iter.next().unwrap() {}
                    continue;
                }
                Some(x) => Some(x),
                None => None,
            };
        }
    }
}

#[derive(Clone, Debug)]
pub struct ContinuationStrCharIndices<'input> {
    str_iter: std::str::CharIndices<'input>,
}

impl<'input> ContinuationStrCharIndices<'input> {
    fn new(str_iter: std::str::CharIndices<'input>) -> ContinuationStrCharIndices<'input> {
        ContinuationStrCharIndices { str_iter: str_iter }
    }
}

// Iterator over a FortranUserStr. Ignores continuation. This allows us to
// tokenize the FORTRAN program without allocating any memory.
impl<'input> Iterator for ContinuationStrCharIndices<'input> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        // if we're here, we can assume that the string is already a
        // valid identifier, which means the continuation is properly
        // terminated. Just continue until we see a closing ampersand.
        loop {
            return match self.str_iter.next() {
                Some(amp) if amp.1 == '&' => {
                    while '&' != self.str_iter.next().unwrap().1 {}
                    continue;
                }
                Some(x) => Some(x),
                None => None,
            };
        }
    }
}

#[derive(Clone, Debug)]
pub struct CaseInsensitiveContinuationStr<'input> {
    user_str: ContinuationStr<'input>,
}

impl<'input> CaseInsensitiveContinuationStr<'input> {
    pub fn new(string: &'input str) -> CaseInsensitiveContinuationStr {
        CaseInsensitiveContinuationStr {
            user_str: ContinuationStr::new(string),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = char> + 'input {
        self.chars()
    }

    pub fn chars(&self) -> impl Iterator<Item = char> + 'input {
        self.user_str.iter().map(|c: char| c.to_ascii_lowercase())
    }

    pub fn char_indices(&self) -> impl Iterator<Item = (usize, char)> + 'input {
        self.user_str
            .char_indices()
            .map(|(i, c)| (i, c.to_ascii_lowercase()))
    }
}

impl<'input> PartialEq for CaseInsensitiveContinuationStr<'input> {
    fn eq(&self, other: &CaseInsensitiveContinuationStr) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<'input> Eq for CaseInsensitiveContinuationStr<'input> {}

impl<'input> Display for CaseInsensitiveContinuationStr<'input> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        for c in self.iter() {
            f.write_char(c)?;
        }

        Ok(())
    }
}
