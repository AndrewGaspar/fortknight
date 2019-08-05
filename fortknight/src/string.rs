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
        ContinuationStrChars::new(self.string.chars())
    }
}

impl<'input> PartialEq for ContinuationStr<'input> {
    fn eq(&self, other: &ContinuationStr) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<'input> Eq for ContinuationStr<'input> {}

impl<'input> ToString for ContinuationStr<'input> {
    fn to_string(&self) -> String {
        use std::iter::FromIterator;
        String::from_iter(self.iter())
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
        self.user_str.iter().map(|c: char| c.to_ascii_lowercase())
    }
}

impl<'input> PartialEq for CaseInsensitiveContinuationStr<'input> {
    fn eq(&self, other: &CaseInsensitiveContinuationStr) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<'input> Eq for CaseInsensitiveContinuationStr<'input> {}

impl<'input> ToString for CaseInsensitiveContinuationStr<'input> {
    fn to_string(&self) -> String {
        use std::iter::FromIterator;
        String::from_iter(self.iter())
    }
}
