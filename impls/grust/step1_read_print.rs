mod environment;
mod printer;
mod reader;
mod types;
mod utils;
use crate::printer::pr_str;
use crate::reader::read_str;

#[allow(non_snake_case)]
fn READ(input: &str) -> MalRes {
    // let mut reader = Reader::new();
    read_str(input.to_string())
}

#[allow(non_snake_case)]
fn EVAL(input: MalRes) -> MalRes {
    input
}

#[allow(non_snake_case)]
fn PRINT(input: MalRes) -> String {
    pr_str(input, true)
}

fn rep(input: &str) -> String {
    let ast = READ(input);
    let result = EVAL(ast);
    PRINT(result)
}

use rustyline::error::ReadlineError;
use rustyline::Editor;
use types::MalRes;

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
