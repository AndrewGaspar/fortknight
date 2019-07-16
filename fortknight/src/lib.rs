use std::{default::Default, path::PathBuf};

mod data;
mod error;
mod lex;

use data::AnalysisData;
use error::AnalysisErrorKind;

/// Options configuring the AnalysisEngine
#[derive(Default)]
pub struct AnalysisOptions{
    pub files: Vec<PathBuf>,
}

impl AnalysisOptions {}

/// Represents a total analysis context. Any options applied to the context apply to all source
/// files analyzed in this context. It also shares all common resources between the modules, such as
/// includes and common module files.
pub struct AnalysisEngine {
    options: AnalysisOptions,
    data: AnalysisData,
}

impl AnalysisEngine {
    pub fn new(options: AnalysisOptions) -> Self {
        let mut engine = Self {
            options,
            data: AnalysisData::default(),
        };

        for path in engine.options.files.iter().cloned().collect::<Vec<_>>() {
            engine.add_file(path).unwrap();
        }

        engine
    }

    pub fn report_error(&mut self) {}

    fn add_file(&mut self, file_path: PathBuf) -> Result<(), AnalysisErrorKind> {
        let contents = match std::fs::read(&file_path) {
            Ok(contents) => contents,
            Err(err) => return Err(AnalysisErrorKind::Io(err)),
        };

        self.data.file_data.file_names.push(file_path);
        self.data.file_data.contents.push(contents);

        Ok(())
    }
}
