mod env;
mod printer;
mod reader;
mod types;
mod utils;
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::rc::Rc;

use crate::printer::pr_str;
use crate::reader::read_str;
use crate::types::{MalArgs, MalErr, MalType};

use crate::env::Env;

fn int_op(op: fn(isize, isize) -> isize, a: MalArgs) -> MalRes {
    match (a[0].clone(), a[1].clone()) {
        (MalType::Int(a), MalType::Int(b)) => Ok(MalType::Int(op(a, b))),
        _ => Err(MalErr::WrongTypeForOperation),
    }
}

#[allow(non_snake_case)]
fn READ(input: &str) -> MalRes {
    read_str(input)
}

fn eval_ast(ast: MalType, env: &mut Env) -> MalRes {
    match ast {
        MalType::Symbol(sym) => match env.get(&sym) {
            Ok(value) => Ok(value),
            Err(err) => Err(err),
        },
        MalType::List(list) => {
            let mut evaluated: Vec<MalType> = vec![];
            for value in list {
                evaluated.push(EVAL(Ok(value), env)?);
            }
            Ok(MalType::List(evaluated))
        }
        MalType::Vector(vector) => {
            let mut evaluated: Vec<MalType> = vec![];
            for value in vector {
                evaluated.push(EVAL(Ok(value), env)?);
            }
            Ok(MalType::Vector(evaluated))
        }
        MalType::Hash(hash) => {
            let mut evaluated: HashMap<String, MalType> = HashMap::new();
            for (key, value) in hash.iter() {
                evaluated.insert(key.clone(), EVAL(Ok(value.clone()), env)?);
            }
            Ok(MalType::Hash(evaluated))
        }
        _ => Ok(ast),
    }
}

#[allow(non_snake_case)]
fn EVAL(ast: MalRes, env: &mut Env) -> MalRes {
    match ast.clone() {
        Ok(MalType::List(list)) => {
            if list.is_empty() {
                ast
            } else if let MalType::Symbol(command) = list[0].clone() {
                match command.as_str() {
                    "def!" => {
                        if list.len() != 3 {
                            Err(MalErr::WrongNumberOfArguments)
                        } else if let MalType::Symbol(sym) = list[1].clone() {
                            let temp = EVAL(Ok(list[2].clone()), env /* .borrow_mut()*/)?;
                            env.set(sym, temp.clone());
                            Ok(temp)
                        } else {
                            Err(MalErr::UnknownError)
                        }
                    }
                    "let*" => {
                        let mut let_env = Env::new(Some(Rc::new(env.clone())));
                        let (a1, a2) = (list[1].clone(), list[2].clone());
                        match a1 {
                            MalType::List(binds) | MalType::Vector(binds) => {
                                for (b, e) in binds.iter().tuples() {
                                    match b {
                                        MalType::Symbol(sym) => {
                                            let temp = EVAL(Ok(e.clone()), let_env.borrow_mut())?;
                                            let_env.set(sym.clone(), temp);
                                        }
                                        _ => return Err(MalErr::WrongTypeForOperation),
                                    }
                                }
                            }
                            _ => return Err(MalErr::WrongTypeForOperation),
                        }
                        EVAL(Ok(a2), let_env.borrow_mut())
                    }
                    _ => {
                        if let MalType::List(list) = eval_ast(ast?, env)? {
                            list[0].apply(list[1..].to_vec())
                        } else {
                            Err(MalErr::CalledNonFunctionType)
                        }
                    }
                }
            } else if let MalType::List(list) = eval_ast(ast?, env)? {
                list[0].apply(list[1..].to_vec())
            } else {
                Err(MalErr::CalledNonFunctionType)
            }
        }
        // Ok(MalType::List(list)) => {
        //     if list.is_empty() {
        //         ast
        //     } else if let MalType::List(list) = eval_ast(ast?, env)? {
        //         list[0].apply(list[1..].to_vec())
        //     } else {
        //         Err(MalErr::CalledNonFunctionType)
        //     }
        // }
        _ => eval_ast(ast?, env),
    }
}

#[allow(non_snake_case)]
fn PRINT(input: MalRes) -> String {
    pr_str(input, true)
}

fn rep(input: &str, env: &mut Env) -> String {
    let ast = READ(input);
    let result = EVAL(ast, env);
    PRINT(result)
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
    let mut repl_env = Env::new(None);
    repl_env.set(
        "+".to_string(),
        MalType::Func(|a: MalArgs| int_op(|i, j| i + j, a)),
    );
    repl_env.set(
        "-".to_string(),
        MalType::Func(|a: MalArgs| int_op(|i, j| i - j, a)),
    );
    repl_env.set(
        "*".to_string(),
        MalType::Func(|a: MalArgs| int_op(|i, j| i * j, a)),
    );
    repl_env.set(
        "/".to_string(),
        MalType::Func(|a: MalArgs| int_op(|i, j| i / j, a)),
    );
    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!("{}", rep(line.as_str(), repl_env.borrow_mut()));
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
