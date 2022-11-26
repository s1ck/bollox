#![allow(dead_code)]

mod ast;
mod env;
pub mod error;
mod eval;
mod parser;
mod scanner;
pub mod token;

use std::cell::Cell;

use error::{BolloxError, BolloxErrors};

use eval::interpreter;
pub use scanner::Source;

use crate::parser::parser;

pub(crate) type Result<T> = std::result::Result<T, BolloxError>;

pub fn run<T>(code: T) -> std::result::Result<(), BolloxErrors>
where
    T: AsRef<str> + std::fmt::Display,
{
    let errors = Cell::new(Vec::new());
    let store_err = |e: BolloxError| {
        let mut errs = errors.take();
        errs.push(e);
        errors.set(errs);
    };

    let source = scanner::Source::new(code.as_ref());

    // scan (source -> tokens)
    let tokens = source.into_iter().filter_map(|t| match t {
        Ok(t) => Some(t),
        Err(e) => {
            store_err(e);
            None
        }
    });

    // parse (tokens -> statements)
    let statements = parser(source, tokens).filter_map(|stmt| match stmt {
        Ok(e) => Some(e),
        Err(e) => {
            store_err(e);
            None
        }
    });

    // evaluate (statements)
    interpreter(statements).for_each(|res| match res {
        Ok(_) => {}
        Err(e) => store_err(e),
    });

    let errors = errors.into_inner();

    if !errors.is_empty() {
        Err(BolloxErrors {
            src: code.to_string(),
            nested: errors.into_iter().map(Into::into).collect(),
        })
    } else {
        Ok(())
    }
}
