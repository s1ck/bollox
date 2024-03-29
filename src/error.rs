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
    ResolverError(#[from] ResolverError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    RuntimeError(#[from] RuntimeError),
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum ScanError {
    #[error("Unexpected token {} in input", found)]
    UnexpectedToken {
        found: char,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Unterminated string literatal in input")]
    UnterminatedString {
        #[label("{}", self)]
        span: SourceSpan,
    },
}

impl ScanError {
    pub fn unexpected_token(span: SourceSpan, found: char) -> BolloxError {
        Self::UnexpectedToken { found, span }.into()
    }

    pub fn unterminated_string(span: SourceSpan) -> BolloxError {
        Self::UnterminatedString { span }.into()
    }
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum SyntaxError {
    #[error("Missing closing parenthesis")]
    MissingClosingParenthesis {
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Missing semicolon after statement")]
    MissingSemicolon {
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Missing variable name")]
    MissingVariableName {
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Unexpected token, expected {:?}, found {:?}.", expected, found)]
    UnexpectedToken {
        expected: TokenType,
        found: TokenType,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Unsupported token {:?}.", found)]
    UnsupportedToken {
        found: TokenType,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Unsupported end of input")]
    UnexpectedEndOfInput,

    #[error("Undefined variable `{}`", found)]
    UndefinedVariable {
        found: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Invalid assignment target")]
    InvalidAssignmentTarget {
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Too many arguments. Expected at most 255 arguments.")]
    TooManyArguments {
        #[label("{}", self)]
        span: SourceSpan,
    },
}

impl SyntaxError {
    pub fn missing_closing_parenthesis(span: impl Into<SourceSpan>) -> BolloxError {
        Self::MissingClosingParenthesis { span: span.into() }.into()
    }

    pub fn missing_variable_name(span: impl Into<SourceSpan>) -> BolloxError {
        Self::MissingVariableName { span: span.into() }.into()
    }

    pub fn unexpected_token(
        expected: TokenType,
        found: TokenType,
        span: impl Into<SourceSpan>,
    ) -> BolloxError {
        Self::UnexpectedToken {
            expected,
            found,
            span: span.into(),
        }
        .into()
    }

    pub fn unsupported_token_type(token: TokenType, span: impl Into<SourceSpan>) -> BolloxError {
        Self::UnsupportedToken {
            found: token,
            span: span.into(),
        }
        .into()
    }

    pub fn unexpected_eoi() -> BolloxError {
        Self::UnexpectedEndOfInput.into()
    }

    pub fn undefined_variable(name: impl Into<String>, span: Span) -> BolloxError {
        Self::UndefinedVariable {
            found: name.into(),
            span: span.into(),
        }
        .into()
    }

    pub fn invalid_assignment_target(span: Span) -> BolloxError {
        Self::InvalidAssignmentTarget { span: span.into() }.into()
    }

    pub fn too_many_arguments(span: Span) -> BolloxError {
        Self::TooManyArguments { span: span.into() }.into()
    }
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum ResolverError {
    #[error("Variable is declared, but not yet defined: `{}`.", name)]
    UndefinedVariable {
        name: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Variable already defined in this scope: `{}`.", name)]
    RedefinedVariable {
        name: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Function already defined in this scope: `{}`.", name)]
    RedefinedFunction {
        name: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Can't return from top-level code.")]
    TopLevelReturn {
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("`this` cannot be called outside of class context.")]
    ThisOutsideClass {
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Cannot return a value from an initializer.")]
    ReturnInInit {
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("A class cannot inherit from itself.")]
    SubclassEqualsSuperclass {
        #[label("{}", self)]
        span: SourceSpan,
    },
}

impl ResolverError {
    pub fn undefined_variable(name: impl Into<String>, span: Span) -> BolloxError {
        Self::UndefinedVariable {
            name: name.into(),
            span: span.into(),
        }
        .into()
    }

    pub fn redefined_variable(name: impl Into<String>, span: Span) -> BolloxError {
        Self::RedefinedVariable {
            name: name.into(),
            span: span.into(),
        }
        .into()
    }

    pub fn redefined_function(name: impl Into<String>, span: Span) -> BolloxError {
        Self::RedefinedFunction {
            name: name.into(),
            span: span.into(),
        }
        .into()
    }

    pub fn top_level_return(span: Span) -> BolloxError {
        Self::TopLevelReturn { span: span.into() }.into()
    }

    pub fn this_outside_class(span: Span) -> BolloxError {
        Self::ThisOutsideClass { span: span.into() }.into()
    }

    pub fn return_in_init(span: Span) -> BolloxError {
        Self::ReturnInInit { span: span.into() }.into()
    }

    pub fn sub_eq_sup(span: Span) -> BolloxError {
        Self::SubclassEqualsSuperclass { span: span.into() }.into()
    }
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub enum RuntimeError {
    #[error("Expected number literal, got {:?}.", found)]
    NonNumber {
        found: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Expected string literal, got {:?}.", found)]
    NonStr {
        found: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Expected callable, got {:?}.", found)]
    NonCallable {
        found: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Expected {:?} arguments, got {:?}.", expected, found)]
    ArgsLengthMismatch {
        expected: String,
        found: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Incompatible types for op `{}`, got {:?} and {:?} .", op, lhs, rhs)]
    IncompatibleTypes {
        lhs: String,
        rhs: String,
        op: BinaryOp,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Properties are only available on instances.")]
    InvalidPropertyCall {
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Undefined property, got `{}`.", name)]
    UndefinedProperty {
        name: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Internal error: `{}`.", msg)]
    InternalError {
        msg: String,
        #[label("{}", self)]
        span: SourceSpan,
    },

    #[error("Superclass must be a class.")]
    SuperclassNoClass {
        #[label("{}", self)]
        span: SourceSpan,
    },
}

impl RuntimeError {
    pub fn non_number(found: impl Display, span: Span) -> BolloxError {
        Self::NonNumber {
            found: found.to_string(),
            span: span.into(),
        }
        .into()
    }

    pub fn non_str(found: impl Display, span: Span) -> BolloxError {
        Self::NonStr {
            found: found.to_string(),
            span: span.into(),
        }
        .into()
    }

    pub fn non_callable(found: impl Display, span: Span) -> BolloxError {
        Self::NonCallable {
            found: found.to_string(),
            span: span.into(),
        }
        .into()
    }

    pub fn args_len(expected: impl Display, found: impl Display, span: Span) -> BolloxError {
        Self::ArgsLengthMismatch {
            expected: expected.to_string(),
            found: found.to_string(),
            span: span.into(),
        }
        .into()
    }

    pub fn incompatible_types(
        op: BinaryOp,
        lhs: impl Display,
        rhs: impl Display,
        span: Span,
    ) -> BolloxError {
        Self::IncompatibleTypes {
            lhs: lhs.to_string(),
            rhs: rhs.to_string(),
            op,
            span: span.into(),
        }
        .into()
    }

    pub fn invalid_property_call(span: Span) -> BolloxError {
        Self::InvalidPropertyCall { span: span.into() }.into()
    }

    pub fn undefined_property(name: impl Display, span: Span) -> BolloxError {
        Self::UndefinedProperty {
            name: name.to_string(),
            span: span.into(),
        }
        .into()
    }

    pub fn internal(msg: impl Display, span: Span) -> BolloxError {
        Self::InternalError {
            msg: msg.to_string(),
            span: span.into(),
        }
        .into()
    }

    pub fn super_no_class(span: Span) -> BolloxError {
        Self::SuperclassNoClass { span: span.into() }.into()
    }
}
