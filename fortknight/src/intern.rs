use std::collections::HashMap;

pub struct StringInterner {
    names: HashMap<&'static str, InternedString>,
    strings: Vec<String>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            strings: Vec::new(),
        }
    }

    pub fn intern(&mut self, string: &str) -> InternedString {
        if !self.names.contains_key(string) {
            self.strings.push(string.to_string());
            self.names.insert(
                unsafe { &*(self.strings.last().unwrap().as_str() as *const str) },
                InternedString(self.strings.len() - 1),
            );
        }

        *self.names.get(string).unwrap()
    }

    pub fn intern_string(&mut self, string: String) -> InternedString {
        if !self.names.contains_key(string.as_str()) {
            self.strings.push(string);
            self.names.insert(
                unsafe { &*(self.strings.last().unwrap().as_str() as *const str) },
                InternedString(self.strings.len() - 1),
            );
            InternedString(self.strings.len() - 1)
        } else {
            *self.names.get(string.as_str()).unwrap()
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InternedString(usize);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InternedName {
    pub id: InternedString,
    pub case_sensitive_id: InternedString,
}
