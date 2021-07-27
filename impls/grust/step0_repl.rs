#[allow(non_snake_case)]
fn READ(input: &str) -> &str {
    input
}

#[allow(non_snake_case)]
fn EVAL(input: &str) -> &str {
    input
}

#[allow(non_snake_case)]
fn PRINT(input: &str) -> &str {
    input
}

fn rep(input: &str) -> &str {
    let ast = READ(input);
    let result = EVAL(ast);
    PRINT(result)
}

use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!("{}", rep(line.as_str()));
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
