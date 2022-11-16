#![allow(dead_code)]

pub mod ast;
pub mod error;
pub mod eval;
pub mod parser;
pub mod scanner;
pub mod token;

use std::cell::Cell;

use error::{BolloxError, BolloxErrors};
use eval::Value;
pub use scanner::Source;

use crate::{ast::Expr, parser::parser};

pub(crate) type Result<T> = std::result::Result<T, BolloxError>;

pub fn run<T>(code: T) -> std::result::Result<Value, BolloxErrors>
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

    // scan
    let tokens = source.into_iter().filter_map(|t| match t {
        Ok(t) => Some(t),
        Err(e) => {
            store_err(e);
            None
        }
    });

    // parse
    let ast = parser(source, tokens)
        .filter_map(|e| match e {
            Ok(e) => Some(e),
            Err(e) => {
                store_err(e);
                None
            }
        })
        .collect::<Option<Expr>>();

    // eval
    let value = ast
        .map(crate::eval::eval)
        .map(|v| match v {
            Ok(v) => Some(v),
            Err(e) => {
                store_err(e);
                None
            }
        })
        .unwrap_or_default();

    let errors = errors.into_inner();

    if errors.is_empty() {
        Ok(value.unwrap())
    } else {
        Err(BolloxErrors {
            src: code.to_string(),
            nested: errors.into_iter().map(Into::into).collect(),
        })
    }
}
