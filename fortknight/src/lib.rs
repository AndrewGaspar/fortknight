// TODO: Remove this once this library is further along.
#![allow(dead_code)]

use std::cell::RefCell;
use std::{convert::TryInto, default::Default, path::PathBuf};

pub mod data;
pub mod error;
mod index;
mod intern;
pub mod num;
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
    pub diagnostics: RefCell<DiagnosticSink>,
    _interner: StringInterner,
}

impl AnalysisEngine {
    pub fn new(options: AnalysisOptions) -> Self {
        let mut engine = Self {
            options,
            data: AnalysisData::default(),
            diagnostics: RefCell::new(DiagnosticSink::from_stderr()),
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

        let mut interner = crate::intern::StringInterner::new();
        let mut arena = parser::classify::ClassifierArena::new();

        let mut classifier = parser::classify::Classifier::new(
            &parser::lex::TokenizerOptions::default(),
            file_id,
            &self.data.file_data.contents.last().unwrap(),
            &self.diagnostics,
            &mut interner,
            &mut arena,
        );

        while let Some(_) = classifier.next_stmt() {}

        if self.options.print_tokens {
            let tokenize_preprocessor = self.data.file_data.file_names[file_id.0 as usize]
                .extension()
                == Some("F90".as_ref());

            println!("{}", tokenize_preprocessor);

            let tokenizer = parser::lex::Tokenizer::new(
                &parser::lex::TokenizerOptions {
                    tokenize_preprocessor,
                },
                file_id,
                &self.data.file_data.contents.last().unwrap(),
                &self.diagnostics,
            );

            println!("Tokens:");
            for t in tokenizer {
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
