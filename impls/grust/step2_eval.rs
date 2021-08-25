mod environment;
mod printer;
mod reader;
mod types;
mod utils;
use std::collections::HashMap;
use std::rc::Rc;

use crate::printer::pr_str;
use crate::reader::read_str;
use crate::types::{
    MalArgs, MalErr,
    MalType::{self, List, Vector},
};

pub type Env = HashMap<String, MalType>;

fn int_op(op: fn(isize, isize) -> isize, a: MalArgs) -> MalRes {
    match (a[0].clone(), a[1].clone()) {
        (MalType::Int(a), MalType::Int(b)) => Ok(MalType::Int(op(a, b))),
        _ => Err(MalErr::WrongTypeForOperation),
    }
}

#[allow(non_snake_case)]
fn READ(input: &str) -> MalRes {
    // let mut reader = Reader::new();
    read_str(input.to_string())
}

fn eval_ast(ast: MalType, env: Env) -> MalRes {
    match ast {
        MalType::Symbol(sym) => match env.get(&sym) {
            Some(value) => Ok(value.clone()),
            None => Err(MalErr::SymbolNotDefined(sym)),
        },
        MalType::List(list) => {
            let mut evaluated: Vec<MalType> = vec![];
            for value in list.iter() {
                evaluated.push(EVAL(Ok(value.clone()), env.clone())?);
            }
            Ok(list!(evaluated))
        }
        MalType::Vector(vector) => {
            let mut evaluated: Vec<MalType> = vec![];
            for value in vector.iter() {
                evaluated.push(EVAL(Ok(value.clone()), env.clone())?);
            }
            Ok(vector!(evaluated))
        }
        MalType::Hash(hash) => {
            let mut evaluated: HashMap<String, MalType> = HashMap::new();
            for (key, value) in hash.iter() {
                evaluated.insert(key.clone(), EVAL(Ok(value.clone()), env.clone())?);
            }
            Ok(MalType::Hash(Rc::new(evaluated)))
        }
        _ => Ok(ast),
    }
}

#[allow(non_snake_case)]
fn EVAL(ast: MalRes, env: Env) -> MalRes {
    match ast.clone() {
        Ok(MalType::List(list)) => {
            if list.is_empty() {
                ast
            } else if let MalType::List(list) = eval_ast(ast?, env)? {
                list[0].apply(list[1..].to_vec())
            } else {
                Err(MalErr::CalledNonFunctionType)
            }
        }
        _ => eval_ast(ast?, env),
    }
}

#[allow(non_snake_case)]
fn PRINT(input: MalRes) -> String {
    pr_str(input, true)
}

fn rep(input: &str, env: Env) -> String {
    let ast = READ(input);
    let result = EVAL(ast, env);
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
    let mut repl_env = Env::default();
    repl_env.insert(
        "+".to_string(),
        MalType::Func(|a: MalArgs| int_op(|i, j| i + j, a)),
    );
    repl_env.insert(
        "-".to_string(),
        MalType::Func(|a: MalArgs| int_op(|i, j| i - j, a)),
    );
    repl_env.insert(
        "*".to_string(),
        MalType::Func(|a: MalArgs| int_op(|i, j| i * j, a)),
    );
    repl_env.insert(
        "/".to_string(),
        MalType::Func(|a: MalArgs| int_op(|i, j| i / j, a)),
    );
    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!("{}", rep(line.as_str(), repl_env.clone()));
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
