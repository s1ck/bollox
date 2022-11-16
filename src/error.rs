use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::token::{Span, TokenType};

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

    #[error(transparent)]
    #[diagnostic(transparent)]
    RuntimeError(#[from] RuntimeError),
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
    pub fn unexpected_token(span: SourceSpan, found: char) -> BolloxError {
        Self::UnexpectedToken(UnexpectedToken { found, span }).into()
    }

    pub fn unterminated_string(span: SourceSpan) -> BolloxError {
        Self::UnterminatedString(UnterminatedString { span }).into()
    }
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum SyntaxError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    MissingClosingParenthesis(MissingClosingParenthesis),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnsupportedToken(UnsupportedTokenType),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedEndOfInput(UnexpectedEndOfInput),
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Missing closing parenthesis")]
#[diagnostic()]
pub struct MissingClosingParenthesis {
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Unsupported token type {:?}.", found)]
#[diagnostic()]
pub struct UnsupportedTokenType {
    found: TokenType,
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Unsupported end of input")]
#[diagnostic()]
pub struct UnexpectedEndOfInput;

impl SyntaxError {
    pub fn missing_closing_parenthesis(span: Span) -> BolloxError {
        Self::MissingClosingParenthesis(MissingClosingParenthesis { span: span.into() }).into()
    }

    pub fn unsupported_token_type(token: TokenType, span: Span) -> BolloxError {
        Self::UnsupportedToken(UnsupportedTokenType {
            found: token,
            span: span.into(),
        })
        .into()
    }

    pub fn unexpected_eoi() -> BolloxError {
        Self::UnexpectedEndOfInput(UnexpectedEndOfInput).into()
    }
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum RuntimeError {}
