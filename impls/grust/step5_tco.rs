mod core;
mod types;

mod printer;
mod reader;

mod utils;
use std::collections::HashMap;
use std::rc::Rc;

use crate::core::ns;
use crate::printer::pr_str;
use crate::reader::read_str;
use crate::types::MalType::{List, Nil};
pub mod environment;
use crate::types::{MalErr, MalType};

use crate::environment::{env_get, env_new, env_set, env_sets, Env};

macro_rules! list {
    ($seq:expr) => {{
      List($seq)
    }};
    [$($args:expr),*] => {{
      let v: Vec<MalType> = vec![$($args),*];
      List(Rc::new(v),Rc::new(Nil))
    }}
  }

#[allow(non_snake_case)]
fn READ(input: &str) -> MalRes {
    read_str(input)
}

fn eval_ast(ast: &MalType, env: &Env) -> MalRes {
    match ast {
        MalType::Symbol(_) => match env_get(env, ast) {
            Ok(value) => Ok(value),
            Err(err) => Err(err),
        },
        MalType::List(list) => {
            let mut evaluated: Vec<MalType> = vec![];
            for value in list {
                evaluated.push(EVAL(value.clone(), env.clone())?);
            }
            Ok(MalType::List(evaluated))
        }
        MalType::Vector(vector) => {
            let mut evaluated: Vec<MalType> = vec![];
            for value in vector {
                evaluated.push(EVAL(value.clone(), env.clone())?);
            }
            Ok(MalType::Vector(evaluated))
        }
        MalType::Hash(hash) => {
            let mut evaluated: HashMap<String, MalType> = HashMap::new();
            for (key, value) in hash.iter() {
                evaluated.insert(key.clone(), EVAL(value.clone(), env.clone())?);
            }
            Ok(MalType::Hash(evaluated))
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
                            Err(MalErr::WrongNumberOfArguments)
                        } else {
                            env_set(&env, list[1].clone(), EVAL(list[2].clone(), env.clone())?)
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
                                        _ => return Err(MalErr::WrongTypeForOperation),
                                    }
                                }
                            }
                            _ => return Err(MalErr::WrongTypeForOperation),
                        }
                        EVAL(a2, let_env)
                    }
                    "do" => match eval_ast(&list!(list[1..].to_vec()), &env)? {
                        MalType::List(el) => Ok(el.last().unwrap_or(&Nil).clone()),
                        _ => Err(MalErr::WrongTypeForOperation),
                    },
                    "if" => {
                        let temp = EVAL(list[1].clone(), env.clone())?;
                        if temp != MalType::Nil && temp != MalType::False {
                            EVAL(list[2].clone(), env)
                        } else if list.len() > 3 {
                            EVAL(list[3].clone(), env)
                        } else {
                            Ok(MalType::Nil)
                        }
                    }
                    "fn*" => Ok(MalType::MalFunc {
                        eval: EVAL,
                        ast: Rc::new(list[2].clone()),
                        env,
                        params: Rc::new(list[1].clone()),
                    }),
                    _ => {
                        if let MalType::List(list) = eval_ast(&ast, &env)? {
                            list[0].apply(list[1..].to_vec())
                        } else {
                            Err(MalErr::CalledNonFunctionType)
                        }
                    }
                }
            } else if let MalType::List(list) = eval_ast(&ast, &env)? {
                list[0].apply(list[1..].to_vec())
            } else {
                Err(MalErr::CalledNonFunctionType)
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
    for (key, val) in ns() {
        env_sets(&repl_env, key, val);
    }
    let _ = rep("(def! not (fn* (a) (if a false true)))", &repl_env);
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
