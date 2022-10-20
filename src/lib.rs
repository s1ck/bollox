#![feature(test)]

mod scanner;
mod token;

#[cfg(test)]
extern crate test;

pub type BolloxError = Box<dyn std::error::Error>;
pub type BolloxResult = Result<(), BolloxError>;

pub fn run<T>(code: T) -> BolloxResult
where
    T: AsRef<str> + std::fmt::Display,
{
    let source = scanner::Source::new(code.as_ref());

    for token in source.into_iter() {
        println!("{}", token?);
    }
    Ok(())
}
