use std::default::Default;

use clap::Arg;
use fortknight::{AnalysisEngine, AnalysisOptions};

const FILES: &'static str = "files";

fn main() {
    let matches = clap::App::new("Fortknight")
        .version(clap::crate_version!())
        .arg(
            Arg::with_name(FILES)
                .index(1)
                .multiple(true)
                .takes_value(true),
        )
        .get_matches();

    let files: Vec<_> = matches
        .values_of_os(FILES)
        .iter()
        .flat_map(|s: &clap::OsValues| s.clone().map(|s| s.to_owned()))
        .map(|f| f.into())
        .collect();

    let mut options = AnalysisOptions::default();
    options.files = files;

    let engine = AnalysisEngine::new(options);
}
