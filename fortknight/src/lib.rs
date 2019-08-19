use std::{convert::TryInto, default::Default, path::PathBuf};

pub mod data;
pub mod error;
mod index;
mod intern;
mod parser;
mod span;
pub mod string;

use data::AnalysisData;
use error::{AnalysisErrorKind, DiagnosticSink};
use intern::StringInterner;

/// Options configuring the AnalysisEngine
#[derive(Default)]
pub struct AnalysisOptions {
    pub files: Vec<PathBuf>,
    pub print_tokens: bool,
}

impl AnalysisOptions {}

/// Represents a total analysis context. Any options applied to the context apply to all source
/// files analyzed in this context. It also shares all common resources between the modules, such as
/// includes and common module files.
pub struct AnalysisEngine {
    options: AnalysisOptions,
    pub data: AnalysisData,
    pub diagnostics: DiagnosticSink,
    _interner: StringInterner,
}

impl AnalysisEngine {
    pub fn new(options: AnalysisOptions) -> Self {
        let mut engine = Self {
            options,
            data: AnalysisData::default(),
            diagnostics: DiagnosticSink::from_stderr(),
            _interner: StringInterner::new(),
        };

        for path in engine.options.files.iter().cloned().collect::<Vec<_>>() {
            engine.add_file(path).unwrap();
        }

        engine
    }

    pub fn report_error(&mut self) {}

    fn read_line_starts(contents: &str) -> Vec<u32> {
        let mut line_starts = vec![0u32];
        for (i, c) in contents.char_indices() {
            if c == '\n' {
                line_starts.push((i + 1) as u32);
            }
        }
        line_starts
    }

    fn add_file(&mut self, file_path: PathBuf) -> Result<(), AnalysisErrorKind> {
        let contents = match std::fs::read_to_string(&file_path) {
            Ok(contents) => contents,
            Err(err) => return Err(AnalysisErrorKind::Io(err)),
        };

        self.data.file_data.file_names.push(file_path);
        self.data
            .file_data
            .lines
            .push(Self::read_line_starts(&contents));
        self.data.file_data.contents.push(contents);

        let file_id = index::FileId(
            (self.data.file_data.file_names.len() - 1)
                .try_into()
                .unwrap(),
        );

        let tokenizer = parser::lex::Tokenizer::new(
            &parser::lex::TokenizerOptions::default(),
            file_id,
            &self.data.file_data.contents.last().unwrap(),
            &mut self.diagnostics,
        );
        self.data.file_data.tokens.push(tokenizer.collect());

        if self.options.print_tokens {
            println!("Tokens:");
            for t in &self.data.file_data.tokens[file_id.0 as usize] {
                let location = self
                    .data
                    .file_data
                    .display_location(&self.data.file_data.get_lin_col(&t.span.start_location()));

                println!("\t{:>23?} {}", t.kind, location);
            }
            println!();
        }

        Ok(())
    }
}
