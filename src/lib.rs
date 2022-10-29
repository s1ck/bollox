mod ast;
mod error;
mod scanner;
mod token;

use error::BolloxErrors;
pub use scanner::Source;

pub type BolloxResult = Result<(), BolloxErrors>;

pub fn run<T>(code: T) -> BolloxResult
where
    T: AsRef<str> + std::fmt::Display,
{
    let source = scanner::Source::new(code.as_ref());

    let results = source.scan_all();

    match results {
        Ok(tokens) => tokens.iter().for_each(|t| println!("{:?}", t)),
        Err(scan_errors) => {
            let bollox_errors = BolloxErrors {
                src: code.to_string(),
                scan_errors,
            };
            return Err(bollox_errors);
        }
    }

    Ok(())
}
