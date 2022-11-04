use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Errors while running Lox code")]
#[diagnostic()]
pub struct BolloxErrors {
    #[source_code]
    pub src: String,
    #[related]
    pub scan_errors: Vec<ScanError>,
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum ScanError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedToken(UnexpectedToken),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnterminatedString(UnterminatedString),
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Unexpected token {} in input", found)]
#[diagnostic()]
pub struct UnexpectedToken {
    found: char,
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Unterminated string literatal in input")]
#[diagnostic()]
pub struct UnterminatedString {
    #[label("{}", self)]
    span: SourceSpan,
}

impl ScanError {
    pub fn unexpected_token(span: SourceSpan, found: char) -> Self {
        Self::from(UnexpectedToken { found, span })
    }

    pub fn unterminated_string(span: SourceSpan) -> Self {
        Self::from(UnterminatedString { span })
    }
}

impl From<UnexpectedToken> for ScanError {
    fn from(token: UnexpectedToken) -> Self {
        Self::UnexpectedToken(token)
    }
}

impl From<UnterminatedString> for ScanError {
    fn from(string: UnterminatedString) -> Self {
        Self::UnterminatedString(string)
    }
}
