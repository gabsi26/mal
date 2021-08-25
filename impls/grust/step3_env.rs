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

use crate::environment::{env_get, env_new, env_set, Env};

fn int_op(op: fn(isize, isize) -> isize, a: MalArgs) -> MalRes {
    match (a[0].clone(), a[1].clone()) {
        (MalType::Int(a), MalType::Int(b)) => Ok(MalType::Int(op(a, b))),
        _ => Err(MalErr::ErrStr("Non Int type found".to_string())),
    }
}

#[allow(non_snake_case)]
fn READ(input: &str) -> MalRes {
    read_str(input.to_string())
}

fn eval_ast(ast: &MalType, env: &Env) -> MalRes {
    match ast {
        MalType::Symbol(_) => match env_get(env, ast) {
            Ok(value) => Ok(value),
            Err(err) => Err(err),
        },
        MalType::List(list) => {
            let mut evaluated: Vec<MalType> = vec![];
            for value in list.iter() {
                evaluated.push(EVAL(value.clone(), env.clone())?);
            }
            Ok(list!(evaluated))
        }
        MalType::Vector(vector) => {
            let mut evaluated: Vec<MalType> = vec![];
            for value in vector.iter() {
                evaluated.push(EVAL(value.clone(), env.clone())?);
            }
            Ok(vector!(evaluated))
        }
        MalType::Hash(hash) => {
            let mut evaluated: HashMap<String, MalType> = HashMap::new();
            for (key, value) in hash.iter() {
                evaluated.insert(key.clone(), EVAL(value.clone(), env.clone())?);
            }
            Ok(MalType::Hash(Rc::new(evaluated)))
        }
        _ => Ok(ast.clone()),
    }
}

#[allow(non_snake_case)]
fn EVAL(ast: MalType, env: Env) -> MalRes {
    match ast.clone() {
        MalType::List(list) => {
            if list.is_empty() {
                Ok(ast)
            } else if let MalType::Symbol(command) = list[0].clone() {
                match command.as_str() {
                    "def!" => {
                        if list.len() != 3 {
                            Err(MalErr::ErrStr("Wrong number of arguments".to_string()))
                        } else if let MalType::Symbol(_) = list[1].clone() {
                            env_set(&env, list[1].clone(), EVAL(list[2].clone(), env.clone())?)
                        } else {
                            Err(MalErr::ErrStr("Unknown error".to_string()))
                        }
                    }
                    "let*" => {
                        let let_env = env_new(Some(env));
                        let (a1, a2) = (list[1].clone(), list[2].clone());
                        match a1 {
                            MalType::List(binds) | MalType::Vector(binds) => {
                                for (b, e) in binds.iter().tuples() {
                                    match b {
                                        MalType::Symbol(_) => {
                                            let _ = env_set(
                                                &let_env,
                                                b.clone(),
                                                EVAL(e.clone(), let_env.clone())?,
                                            );
                                        }
                                        _ => {
                                            return Err(MalErr::ErrStr(
                                                "Expected Symbol".to_string(),
                                            ))
                                        }
                                    }
                                }
                            }
                            _ => return Err(MalErr::ErrStr("Expected list type".to_string())),
                        }
                        EVAL(a2, let_env)
                    }
                    _ => {
                        if let MalType::List(list) = eval_ast(&ast, &env)? {
                            list[0].apply(list[1..].to_vec())
                        } else {
                            Err(MalErr::ErrStr(
                                "Tried to call non function type".to_string(),
                            ))
                        }
                    }
                }
            } else if let MalType::List(list) = eval_ast(&ast, &env)? {
                list[0].apply(list[1..].to_vec())
            } else {
                Err(MalErr::ErrStr(
                    "Tried to call non function type".to_string(),
                ))
            }
        }

        _ => eval_ast(&ast, &env),
    }
}

#[allow(non_snake_case)]
fn PRINT(input: MalRes) -> String {
    pr_str(input, true)
}

fn rep(input: &str, env: &Env) -> MalRes {
    let ast = READ(input)?;
    EVAL(ast, env.clone())
}

use itertools::Itertools;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use types::MalRes;

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let repl_env = env_new(None);
    let _ = env_set(
        &repl_env,
        MalType::Symbol("+".to_string()),
        MalType::Func(|a: MalArgs| int_op(|i, j| i + j, a)),
    );
    let _ = env_set(
        &repl_env,
        MalType::Symbol("-".to_string()),
        MalType::Func(|a: MalArgs| int_op(|i, j| i - j, a)),
    );
    let _ = env_set(
        &repl_env,
        MalType::Symbol("*".to_string()),
        MalType::Func(|a: MalArgs| int_op(|i, j| i * j, a)),
    );
    let _ = env_set(
        &repl_env,
        MalType::Symbol("/".to_string()),
        MalType::Func(|a: MalArgs| int_op(|i, j| i / j, a)),
    );

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!("{}", PRINT(rep(line.as_str(), &repl_env)));
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
