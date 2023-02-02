use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

use std::io::{stdin, stdout, Read, Write};
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
    let stdin = stdin();
    let mut stdout = stdout().into_raw_mode()?;

    let mut line = String::new();

    write!(
        stdout,
        "{}{}{}",
        termion::clear::All,
        termion::cursor::Goto(1, 1),
        termion::cursor::Hide,
    )?;

    loop {
        let stdin = stdin.lock();

        write!(stdout, ">{}", termion::cursor::Goto(3, 1))?;

        stdout.flush()?;

        for c in stdin.keys() {
            match c? {
                Key::Ctrl('c') => return Ok(()),
                Key::Char('\n') => {
                    line.push('\n');
                    break;
                }
                Key::Char(c) => {
                    line.push(c);
                    write!(stdout, "{}", c)?
                }
                _ => {}
            }
            stdout.flush()?;
        }

        stdout.flush()?;

        // loop {
        //     // print!("{prompt}");
        //     // stdout.flush()?;
        //     prompt = ". ";
        //     stdin.read_line(&mut line)?;
        //     line = line.trim().to_string();

        //     if !line.ends_with('\\') {
        //         break;
        //     }

        //     line = line.trim_end_matches('\\').to_string();
        // }

        if line == "quit" {
            break;
        }

        stdout.suspend_raw_mode()?;
        if let Err(bollox_errors) = bollox::run(&line) {
            write!(stdout, "{:?}", miette::Report::new(bollox_errors))?;
        }
        stdout.activate_raw_mode()?;

        line.clear();
    }
    Ok(())
}
