use miette::Diagnostic;
use thiserror::Error;

use crate::scanner::ScanError;

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("Errors while running Lox code")]
#[diagnostic()]
pub struct BolloxErrors {
    #[source_code]
    pub src: String,
    #[related]
    pub scan_errors: Vec<ScanError>,
}
