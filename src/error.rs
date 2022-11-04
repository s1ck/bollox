use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::token::Token;

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Errors while running Lox code")]
#[diagnostic()]
pub struct BolloxErrors {
    #[source_code]
    pub src: String,
    #[related]
    pub nested: Vec<BolloxError>,
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum BolloxError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    ScanError(#[from] ScanError),
    #[error(transparent)]
    #[diagnostic(transparent)]
    SyntaxError(#[from] SyntaxError),
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

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum SyntaxError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    MissingClosingParenthesis(MissingClosingParenthesis),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnsupportedToken(UnsupportedToken),
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Missing closing parenthesis")]
#[diagnostic()]
pub struct MissingClosingParenthesis {
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Unsupported token {:?}.", found)]
#[diagnostic()]
pub struct UnsupportedToken {
    found: Token,
    #[label("{}", self)]
    span: SourceSpan,
}

impl SyntaxError {
    pub fn missing_closing_parenthesis(span: SourceSpan) -> Self {
        Self::from(MissingClosingParenthesis { span })
    }

    pub fn unsupported_token(token: Token, span: SourceSpan) -> Self {
        Self::from(UnsupportedToken { found: token, span })
    }
}

impl From<MissingClosingParenthesis> for SyntaxError {
    fn from(missing: MissingClosingParenthesis) -> Self {
        Self::MissingClosingParenthesis(missing)
    }
}

impl From<UnsupportedToken> for SyntaxError {
    fn from(unsupported_token: UnsupportedToken) -> Self {
        Self::UnsupportedToken(unsupported_token)
    }
}
