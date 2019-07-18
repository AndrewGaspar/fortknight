use std::{
    io::{BufReader, Read, Seek},
    path::PathBuf,
};

use crate::error::AnalysisErrorKind;

#[derive(Default)]
pub struct FileData {
    pub file_names: Vec<PathBuf>,
    pub contents: Vec<String>,
}

#[derive(Default)]
pub struct AnalysisData {
    // file info
    pub file_data: FileData,
}
