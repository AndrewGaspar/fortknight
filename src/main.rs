use std::default::Default;
use std::error::Error;
use std::io::Write;

use clap::Arg;
use fortknight::error::ParserErrorCode;
use fortknight::string::ContinuationStr;
use fortknight::{AnalysisEngine, AnalysisOptions};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

const FILES: &'static str = "files";
const PRINT_TOKENS: &'static str = "print_tokens";

fn main() -> Result<(), Box<Error>> {
    let matches = clap::App::new("Fortknight")
        .version(clap::crate_version!())
        .arg(
            Arg::with_name(FILES)
                .index(1)
                .multiple(true)
                .takes_value(true),
        )
        .arg(Arg::with_name(PRINT_TOKENS).long("print-tokens"))
        .get_matches();

    let files: Vec<_> = matches
        .values_of_os(FILES)
        .iter()
        .flat_map(|s: &clap::OsValues| s.clone().map(|s| s.to_owned()))
        .map(|f| f.into())
        .collect();

    let mut options = AnalysisOptions::default();
    options.files = files;
    options.print_tokens = matches.is_present(PRINT_TOKENS);

    let engine = AnalysisEngine::new(options);
    for file in 0..engine.data.file_data.file_names.len() {
        let mut stderr = StandardStream::stderr(ColorChoice::Always);
        if engine.data.file_data.lex_errors[file].len() > 0 {
            for err in &engine.data.file_data.lex_errors[file] {
                use ParserErrorCode::*;

                stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_intense(true))?;
                write!(&mut stderr, "error")?;
                stderr.set_color(
                    ColorSpec::new()
                        .set_fg(Some(Color::White))
                        .set_intense(true),
                )?;

                write!(&mut stderr, ": ")?;

                let text = ContinuationStr::new(engine.data.file_data.read_span(&err.span));
                match err.code {
                    UnrecognizedToken => write!(
                        stderr,
                        "`{}` is not a valid lexical token in Fortran",
                        text.to_string()
                    )?,
                    _ => {}
                }
                writeln!(stderr)?;

                stderr.reset()?;
            }
        }
    }

    Ok(())
}
