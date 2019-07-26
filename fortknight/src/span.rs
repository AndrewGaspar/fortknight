#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub file_id: crate::index::FileId,
    pub start: usize,
    pub end: usize,
}
