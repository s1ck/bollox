#![allow(dead_code)]

pub mod error;
pub mod token;

mod ast;
mod env;
mod interp;
mod parser;
mod scanner;
mod value;

use std::cell::Cell;

use error::{BolloxError, BolloxErrors};

use interp::interpreter;
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
    interpreter(statements).for_each(|res| {
        if let Err(e) = res {
            store_err(e)
        }
    });

    let errors = errors.into_inner();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(BolloxErrors {
            src: code.to_string(),
            nested: errors.into_iter().map(Into::into).collect(),
        })
    }
}
