use std::{
    env,
    io::{self},
    path::Path,
};

use state::State;

mod bar;
mod state;
mod ui;

const HELP: &str = "Usage: kyutile --help|--version|<path>";

fn main() {
    let args: Vec<_> = env::args().skip(1).collect();
    if args.contains(&"--help".to_owned()) || args.contains(&"-h".to_owned()) {
        println!("{}", HELP)
    } else if args.contains(&"--version".to_owned()) | args.contains(&"-V".to_owned()) {
        println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
    } else {
        match launch(&args.first()) {
            Ok(_) => (),
            Err(err) => eprintln!("An IO error has occurred: {}.", err),
        }
    }
}

fn launch(arg: &Option<&String>) -> Result<(), io::Error> {
    let mut terminal = ratatui::init();
    let mut state: State = State::new()?;
    if let Some(path) = arg {
        if Path::new(path.as_str()).exists() {
            let _ = state.open(&[path]);
        } else {
            state.path = Some((*path).clone());
        }
    }

    while !state.exit {
        terminal.draw(|frame| state.draw(frame))?;
        state.handle_events()?
    }

    ratatui::restore();
    Ok(())
}
