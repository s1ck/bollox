use std::fmt::Display;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    expr::BinaryOp,
    token::{Span, TokenType},
};

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Errors while running bollox code")]
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
    MissingSemicolon(MissingSemicolon),

    #[error(transparent)]
    #[diagnostic(transparent)]
    MissingVariableName(MissingVariableName),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedToken(UnexpectedTokenType),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnsupportedToken(UnsupportedTokenType),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnexpectedEndOfInput(UnexpectedEndOfInput),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UndefinedVariable(UndefinedVariable),

    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidAssignmentTarget(InvalidAssignmentTarget),

    #[error(transparent)]
    #[diagnostic(transparent)]
    TooManyArguments(TooManyArguments),
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Missing closing parenthesis")]
#[diagnostic()]
pub struct MissingClosingParenthesis {
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Missing semicolon after statement")]
#[diagnostic()]
pub struct MissingSemicolon {
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Missing variable name")]
#[diagnostic()]
pub struct MissingVariableName {
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Unexpected token, expected {:?}, found {:?}.", expected, found)]
#[diagnostic()]
pub struct UnexpectedTokenType {
    expected: TokenType,
    found: TokenType,
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

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Undefined variable `{}`", found)]
#[diagnostic()]
pub struct UndefinedVariable {
    found: String,
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Invalid assignment target")]
#[diagnostic()]
pub struct InvalidAssignmentTarget {
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Too many arguments. Expected at most 255 arguments.")]
#[diagnostic()]
pub struct TooManyArguments {
    #[label("{}", self)]
    span: SourceSpan,
}

impl SyntaxError {
    pub fn missing_closing_parenthesis(span: impl Into<SourceSpan>) -> BolloxError {
        Self::MissingClosingParenthesis(MissingClosingParenthesis { span: span.into() }).into()
    }

    pub fn missing_variable_name(span: impl Into<SourceSpan>) -> BolloxError {
        Self::MissingVariableName(MissingVariableName { span: span.into() }).into()
    }

    pub fn unexpected_token(
        expected: TokenType,
        found: TokenType,
        span: impl Into<SourceSpan>,
    ) -> BolloxError {
        Self::UnexpectedToken(UnexpectedTokenType {
            expected,
            found,
            span: span.into(),
        })
        .into()
    }

    pub fn unsupported_token_type(token: TokenType, span: impl Into<SourceSpan>) -> BolloxError {
        Self::UnsupportedToken(UnsupportedTokenType {
            found: token,
            span: span.into(),
        })
        .into()
    }

    pub fn unexpected_eoi() -> BolloxError {
        Self::UnexpectedEndOfInput(UnexpectedEndOfInput).into()
    }

    pub fn undefined_variable(name: impl Into<String>, span: Span) -> BolloxError {
        Self::UndefinedVariable(UndefinedVariable {
            found: name.into(),
            span: span.into(),
        })
        .into()
    }

    pub fn invalid_assignment_target(span: Span) -> BolloxError {
        Self::InvalidAssignmentTarget(InvalidAssignmentTarget { span: span.into() }).into()
    }

    pub fn too_many_arguments(span: Span) -> BolloxError {
        Self::TooManyArguments(TooManyArguments { span: span.into() }).into()
    }
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum RuntimeError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    NonNumber(NonNumber),

    #[error(transparent)]
    #[diagnostic(transparent)]
    NonStr(NonStr),

    #[error(transparent)]
    #[diagnostic(transparent)]
    IncompatibleTypes(IncompatibleTypes),
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Expected number literal, got {:?}.", found)]
#[diagnostic()]
pub struct NonNumber {
    found: String,
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Expected string literal, got {:?}.", found)]
#[diagnostic()]
pub struct NonStr {
    found: String,
    #[label("{}", self)]
    span: SourceSpan,
}

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Incompatible types for op `{}`, got {:?} and {:?} .", op, lhs, rhs)]
#[diagnostic()]
pub struct IncompatibleTypes {
    lhs: String,
    rhs: String,
    op: BinaryOp,
    #[label("{}", self)]
    span: SourceSpan,
}

impl RuntimeError {
    pub fn non_number(found: impl Display, span: Span) -> BolloxError {
        Self::NonNumber(NonNumber {
            found: found.to_string(),
            span: span.into(),
        })
        .into()
    }

    pub fn non_str(found: impl Display, span: Span) -> BolloxError {
        Self::NonStr(NonStr {
            found: found.to_string(),
            span: span.into(),
        })
        .into()
    }

    pub fn incompatible_types(
        op: BinaryOp,
        lhs: impl Display,
        rhs: impl Display,
        span: Span,
    ) -> BolloxError {
        Self::IncompatibleTypes(IncompatibleTypes {
            lhs: lhs.to_string(),
            rhs: rhs.to_string(),
            op,
            span: span.into(),
        })
        .into()
    }
}
