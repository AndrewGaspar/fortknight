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

    fn read_line_starts(contents: &str) -> Vec<usize> {
        let mut line_starts = vec![0];
        for (i, c) in contents.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
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

        let tokenizer = lex::Tokenizer::new(file_id, &self.data.file_data.contents.last().unwrap());
        if self.options.print_tokens {
            let tokens: Vec<_> = tokenizer.collect();
            for t in tokens
                .iter()
                .map(|x| x.as_ref().unwrap())
                .collect::<Vec<_>>()
            {
                let line = match self.data.file_data.lines[file_id.0 as usize]
                    .binary_search(&t.span.start)
                {
                    Ok(idx) => idx + 1,
                    Err(next_idx) => next_idx,
                };

                println!("{:?}\t{:?}", t.kind, line);
            }

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
