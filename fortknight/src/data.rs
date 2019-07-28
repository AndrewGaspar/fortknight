use std::path::PathBuf;

use crate::span::Span;

#[derive(Default)]
pub struct FileData {
    pub file_names: Vec<PathBuf>,
    pub contents: Vec<String>,
}

impl FileData {
    pub fn read_span(&self, span: &Span) -> &str {
        &self.contents[span.file_id.0 as usize][span.start..span.end]
    }
}

#[derive(Default)]
pub struct AnalysisData {
    // file info
    pub file_data: FileData,
}
