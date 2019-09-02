#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Location {
    pub file_id: crate::index::FileId,
    pub location: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub file_id: crate::index::FileId,
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn start_location(&self) -> Location {
        Location {
            file_id: self.file_id,
            location: self.start,
        }
    }

    pub fn end_location(&self) -> Location {
        Location {
            file_id: self.file_id,
            location: self.end,
        }
    }

    pub fn concat(self, next: Self) -> Span {
        assert_eq!(
            self.file_id, next.file_id,
            "Cannot concat spans across separate files"
        );

        assert!(
            next.start >= self.end,
            "Concatenated spans must not be overlapping or out of order"
        );

        Span {
            file_id: self.file_id,
            start: self.start,
            end: next.end,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LinCol {
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LinColLocation {
    pub file_id: crate::index::FileId,
    pub location: LinCol,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LinColSpan {
    pub file_id: crate::index::FileId,
    pub start: LinCol,
    pub end: LinCol,
}

impl LinColSpan {
    pub fn start_location(&self) -> LinColLocation {
        LinColLocation {
            file_id: self.file_id,
            location: self.start,
        }
    }

    pub fn end_location(&self) -> LinColLocation {
        LinColLocation {
            file_id: self.file_id,
            location: self.end,
        }
    }
}
