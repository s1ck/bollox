use std::io::Write;
use std::path::Path;

type AppResult = Result<(), std::io::Error>;

fn main() -> AppResult {
    let args = std::env::args_os().collect::<Vec<_>>();

    match args.len() {
        1 => run_repl()?,
        2 => run_file(&args[1])?,
        _ => {
            println!("Usage: bollox [script]");
            std::process::exit(42);
        }
    }

    Ok(())
}

fn run_file(file: impl AsRef<Path>) -> AppResult {
    let code = std::fs::read_to_string(file)?;
    if let Err(bollox_errors) = bollox::run(code) {
        println!("{:?}", miette::Report::new(bollox_errors))
    };
    Ok(())
}

fn run_repl() -> AppResult {
    let stdin = std::io::stdin();
    let mut line = String::new();

    loop {
        let mut prompt = "> ";
        line.clear();

        loop {
            print!("{prompt}");
            std::io::stdout().flush()?;
            prompt = ". ";
            stdin.read_line(&mut line)?;
            line = line.trim().to_string();

            if !line.ends_with('\\') {
                break;
            }

            line = line.trim_end_matches('\\').to_string();
        }

        if line == "quit" {
            break;
        }

        if let Err(bollox_errors) = bollox::run(&line) {
            println!("{:?}", miette::Report::new(bollox_errors))
        }
    }
    Ok(())
}
