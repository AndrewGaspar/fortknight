use std::{convert::TryInto, default::Default, path::PathBuf};

mod data;
mod error;
mod index;
mod intern;
mod lex;
mod span;

use data::AnalysisData;
use error::AnalysisErrorKind;
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
    data: AnalysisData,
    interner: StringInterner,
}

impl AnalysisEngine {
    pub fn new(options: AnalysisOptions) -> Self {
        let mut engine = Self {
            options,
            data: AnalysisData::default(),
            interner: StringInterner::new(),
        };

        for path in engine.options.files.iter().cloned().collect::<Vec<_>>() {
            engine.add_file(path).unwrap();
        }

        engine
    }

    pub fn report_error(&mut self) {}

    fn add_file(&mut self, file_path: PathBuf) -> Result<(), AnalysisErrorKind> {
        let contents = match std::fs::read_to_string(&file_path) {
            Ok(contents) => contents,
            Err(err) => return Err(AnalysisErrorKind::Io(err)),
        };

        self.data.file_data.file_names.push(file_path);
        self.data.file_data.contents.push(contents);

        let tokenizer = lex::Tokenizer::new(
            index::FileId(
                (self.data.file_data.file_names.len() - 1)
                    .try_into()
                    .unwrap(),
            ),
            &self.data.file_data.contents.last().unwrap(),
        );

        if self.options.print_tokens {
            let tokens: Vec<_> = tokenizer.collect();
            println!(
                "{:?}",
                tokens
                    .iter()
                    .map(|x| x.as_ref().unwrap())
                    .collect::<Vec<_>>()
            );

            for t in tokens {
                let t = t.unwrap();
                if let Some(t) = t.try_into_identifierish() {
                    t.intern(&self.data.file_data, &mut self.interner);
                    t.intern_case_insensitive(&self.data.file_data, &mut self.interner);
                } else if let Some(t) = t.try_into_operator() {
                    t.intern(&self.data.file_data, &mut self.interner);
                    t.intern_case_insensitive(&self.data.file_data, &mut self.interner);
                }
            }
        }

        Ok(())
    }
}
