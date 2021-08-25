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

use crate::environment::{env_bind, env_get, env_new, env_set, env_sets, Env};

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
fn EVAL(mut ast: MalType, mut env: Env) -> MalRes {
    let res: MalRes;
    'tco: loop {
        res = match ast.clone() {
            MalType::List(list) => {
                if list.is_empty() {
                    return Ok(ast);
                }
                let a0 = &list[0];
                match a0 {
                    MalType::Symbol(ref sym) if sym == "def!" => {
                        if list.len() != 3 {
                            Err(MalErr::WrongNumberOfArguments)
                        } else {
                            env_set(&env, list[1].clone(), EVAL(list[2].clone(), env.clone())?)
                        }
                    }
                    MalType::Symbol(ref sym) if sym == "let*" => {
                        let env = env_new(Some(env.clone()));
                        let (a1, a2) = (list[1].clone(), list[2].clone());
                        match a1 {
                            MalType::List(binds) | MalType::Vector(binds) => {
                                for (b, e) in binds.iter().tuples() {
                                    match b {
                                        MalType::Symbol(_) => {
                                            let _ = env_set(
                                                &env,
                                                b.clone(),
                                                EVAL(e.clone(), env.clone())?,
                                            );
                                        }
                                        _ => return Err(MalErr::WrongTypeForOperation),
                                    }
                                }
                            }
                            _ => return Err(MalErr::WrongTypeForOperation),
                        }
                        ast = a2;
                        continue 'tco;
                    }
                    MalType::Symbol(ref sym) if sym == "do" => {
                        match eval_ast(&list!(list[1..list.len() - 1].to_vec()), &env)? {
                            MalType::List(_) => {
                                ast = list.last().unwrap_or(&Nil).clone();
                                continue 'tco;
                            }
                            _ => Err(MalErr::WrongTypeForOperation),
                        }
                    }
                    MalType::Symbol(ref sym) if sym == "if" => {
                        let cond = EVAL(list[1].clone(), env.clone())?;
                        match cond {
                            MalType::False | MalType::Nil if list.len() >= 4 => {
                                ast = list[3].clone();
                                continue 'tco;
                            }
                            MalType::False | MalType::Nil => Ok(MalType::Nil),
                            _ if list.len() >= 3 => {
                                ast = list[2].clone();
                                continue 'tco;
                            }
                            _ => Ok(Nil),
                        }
                    }
                    MalType::Symbol(ref sym) if sym == "fn*" => Ok(MalType::MalFunc {
                        eval: EVAL,
                        ast: Rc::new(list[2].clone()),
                        env,
                        params: Rc::new(list[1].clone()),
                    }),
                    _ => match eval_ast(&ast, &env)? {
                        MalType::List(ref list) => {
                            let f = &list[0].clone();
                            let args = list[1..].to_vec();
                            match f {
                                MalType::Func(_) => list[0].apply(list[1..].to_vec()),
                                MalType::MalFunc {
                                    ast: mast,
                                    env: menv,
                                    params,
                                    ..
                                } => {
                                    let a = &**mast;
                                    let p = &**params;
                                    env = env_bind(Some(menv.clone()), p.clone(), args)?;
                                    ast = a.clone();
                                    continue 'tco;
                                }
                                _ => Err(MalErr::CalledNonFunctionType),
                            }
                        }
                        _ => Err(MalErr::WrongTypeForOperation),
                    },
                }
            }
            _ => eval_ast(&ast, &env),
        };
        break;
    }
    res
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
