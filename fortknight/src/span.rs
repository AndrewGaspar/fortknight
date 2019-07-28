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
        Location { file_id: self.file_id, location: self.start }
    }
    
    pub fn end_location(&self) -> Location {
        Location { file_id: self.file_id, location: self.end }
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
        LinColLocation { file_id: self.file_id, location: self.start }
    }
    
    pub fn end_location(&self) -> LinColLocation {
        LinColLocation { file_id: self.file_id, location: self.end }
    }
}
