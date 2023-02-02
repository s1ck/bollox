use std::path::Path;

use reedline::{
    default_emacs_keybindings, DefaultPrompt, DefaultPromptSegment, EditCommand, Emacs, KeyCode,
    KeyModifiers, Prompt, Reedline, ReedlineEvent, Signal,
};

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
    let mut line_editor = line_editor();
    let prompt = prompt();

    loop {
        let sig = line_editor.read_line(&prompt);
        let line = match sig {
            Ok(Signal::Success(buffer)) => buffer,
            Ok(Signal::CtrlC) | Ok(Signal::CtrlD) => break,
            _ => todo!(),
        };

        if let Err(bollox_errors) = bollox::run(&line) {
            println!("{:?}", miette::Report::new(bollox_errors))
        }
    }
    Ok(())
}

fn line_editor() -> Reedline {
    let mut keybindings = default_emacs_keybindings();

    keybindings.add_binding(
        KeyModifiers::ALT,
        KeyCode::Enter,
        ReedlineEvent::Edit(vec![EditCommand::InsertNewline]),
    );

    Reedline::create().with_edit_mode(Box::new(Emacs::new(keybindings)))
}

fn prompt() -> impl Prompt {
    DefaultPrompt {
        left_prompt: DefaultPromptSegment::Empty,
        right_prompt: DefaultPromptSegment::Empty,
    }
}
