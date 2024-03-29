pub mod error;
pub mod token;

mod callable;
mod env;
mod expr;
mod interp;
mod node;
mod parser;
mod resolver;
mod scanner;
mod stmt;
mod util;
mod value;

use crate::interp::{interpreter, InterpreterContext};
use crate::parser::parser;
use crate::resolver::resolver;
use error::{BolloxError, BolloxErrors};
use std::cell::Cell;

pub use scanner::Source;

pub(crate) type Result<T, E = BolloxError> = std::result::Result<T, E>;

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

    // resolver (statements -> statements)
    let interpreter_context = InterpreterContext::default();
    let mut resolver = resolver(statements, interpreter_context);
    let statements = resolver
        .by_ref()
        .filter_map(|stmt| match stmt {
            Ok(e) => Some(e),
            Err(e) => {
                store_err(e);
                None
            }
        })
        .collect::<Vec<_>>();

    let interpreter_context = resolver.interpreter_context();

    // evaluate (statements)
    interpreter(statements, interpreter_context).for_each(|res| {
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
