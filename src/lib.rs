#![allow(dead_code)]

pub mod ast;
pub mod error;
pub mod parser;
pub mod scanner;
pub mod token;

use error::BolloxErrors;
use parser::{Parser, RecursiveDescent};
pub use scanner::Source;

pub type BolloxResult = Result<(), BolloxErrors>;

pub fn run<T>(code: T) -> BolloxResult
where
    T: AsRef<str> + std::fmt::Display,
{
    let source = scanner::Source::new(code.as_ref());
    let result = source.scan_all();

    let Ok(tokens) = result else {
        let scan_errors = result.unwrap_err();
        let bollox_errors = BolloxErrors {
            src: code.to_string(),
            nested: scan_errors.into_iter().map(Into::into).collect(),
        };
        return Err(bollox_errors);
    };

    let source = scanner::Source::new(code.as_ref());
    let parser = RecursiveDescent::new(source, tokens);
    let result = parser.parse();

    let Ok(expr) = result else {
        let syntax_error = result.unwrap_err();
        let bollox_errors = BolloxErrors {
            src: code.to_string(),
            nested: vec![syntax_error.into()],
        };
        return Err(bollox_errors);
    };

    println!("{expr:#?}");

    Ok(())
}
