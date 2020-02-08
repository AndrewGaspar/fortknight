use std::convert::TryInto;
use std::path::PathBuf;

use crate::span::{LinCol, LinColLocation, LinColSpan, Location, Span};

#[derive(Default)]
pub struct FileData {
    pub file_names: Vec<PathBuf>,
    pub contents: Vec<String>,
    pub lines: Vec<Vec<u32>>,
}

impl FileData {
    pub fn read_span(&self, span: &Span) -> &str {
        &self.contents[span.file_id.0 as usize][span.start as usize..span.end as usize]
    }

    pub fn get_lin_col(&self, location: &Location) -> LinColLocation {
        let line =
            match self.lines[location.file_id.0 as usize].binary_search(&location.location) {
                Ok(idx) => idx,
                Err(next_idx) => next_idx - 1,
            }
            .try_into()
            .unwrap();

        let column = (location.location - self.lines[location.file_id.0 as usize][line as usize])
            .try_into()
            .unwrap();

        LinColLocation {
            file_id: location.file_id,
            location: LinCol { line, column },
        }
    }

    pub fn get_lin_col_span(&self, span: &Span) -> LinColSpan {
        LinColSpan {
            file_id: span.file_id,
            start: self.get_lin_col(&span.start_location()).location,
            end: self.get_lin_col(&span.end_location()).location,
        }
    }

    pub fn display_location(&self, span: &LinColLocation) -> String {
        format!(
            "{}:{}:{}",
            self.file_names[span.file_id.0 as usize].to_str().unwrap(),
            span.location.line + 1,
            span.location.column + 1
        )
    }
}

#[derive(Default)]
pub struct AnalysisData {
    // file info
    pub file_data: FileData,
}
