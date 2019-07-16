use std::{
    ffi::OsStr,
    io::{BufReader, Read, Seek},
};

pub struct FortranTokenizer {
    reader: BufReader<Box<dyn Read>>,
}

impl FortranTokenizer {
    pub fn from_reader(options: &crate::AnalysisOptions, reader: Box<dyn Read>) -> Self {
        FortranTokenizer {
            reader: BufReader::new(reader),
        }
    }

    pub fn from_file(options: &crate::AnalysisOptions, file: &OsStr) -> std::io::Result<Self> {
        let file = std::fs::File::open(std::path::PathBuf::from(file))?;
        Ok(Self::from_reader(options, Box::new(file)))
    }
}

