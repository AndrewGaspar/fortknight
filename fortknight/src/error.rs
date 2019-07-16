#[derive(Debug)]
pub enum AnalysisErrorKind {
    // Lexer + Parser Errors: E0000-E0999

    // System Errors: E9000+
    Io(std::io::Error)
}
