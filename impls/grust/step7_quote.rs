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
use crate::types::MalType::{List, Nil, Vector};
pub mod environment;
use crate::types::{MalArgs, MalErr, MalType};

use crate::environment::{env_bind, env_get, env_new, env_set, env_sets, Env};

#[allow(non_snake_case)]
fn READ(input: &str) -> MalRes {
    read_str(input.to_string())
}

fn qq_iter(elts: &MalArgs) -> MalType {
    let mut acc = list![];
    for elt in elts.iter().rev() {
        if let List(v) = elt {
            if v.len() == 2 {
                if let MalType::Symbol(ref s) = v[0] {
                    if s == "splice-unquote" {
                        acc = list![MalType::Symbol("concat".to_string()), v[1].clone(), acc];
                        continue;
                    }
                }
            }
        }
        acc = list![MalType::Symbol("cons".to_string()), quasiquote(&elt), acc];
    }
    return acc;
}

fn quasiquote(ast: &MalType) -> MalType {
    match ast {
        List(v) => {
            if v.len() == 2 {
                if let MalType::Symbol(ref s) = v[0] {
                    if s == "unquote" {
                        return v[1].clone();
                    }
                }
            }
            return qq_iter(&v);
        }
        MalType::Vector(v) => return list![MalType::Symbol("vec".to_string()), qq_iter(&v)],
        MalType::Hash(_) | MalType::Symbol(_) => {
            return list![MalType::Symbol("quote".to_string()), ast.clone()]
        }
        _ => ast.clone(),
    }
}

fn eval_ast(ast: &MalType, env: &Env) -> MalRes {
    match ast {
        MalType::Symbol(_) => Ok(env_get(&env, &ast)?),
        MalType::List(list) => {
            let mut evaluated: MalArgs = vec![];
            for value in list.iter() {
                evaluated.push(EVAL(value.clone(), env.clone())?);
            }
            Ok(list!(evaluated))
        }
        MalType::Vector(vector) => {
            let mut evaluated: MalArgs = vec![];
            for value in vector.iter() {
                evaluated.push(EVAL(value.clone(), env.clone())?);
            }
            Ok(vector!(evaluated))
        }
        MalType::Hash(hash) => {
            let mut evaluated: HashMap<String, MalType> = HashMap::new();
            for (key, value) in hash.iter() {
                evaluated.insert(key.to_string(), EVAL(value.clone(), env.clone())?);
            }
            Ok(MalType::Hash(Rc::new(evaluated)))
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
                        env_set(&env, list[1].clone(), EVAL(list[2].clone(), env.clone())?)
                    }
                    MalType::Symbol(ref sym) if sym == "let*" => {
                        env = env_new(Some(env.clone()));
                        let (a1, a2) = (list[1].clone(), list[2].clone());
                        match a1 {
                            MalType::List(ref binds) | MalType::Vector(ref binds) => {
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
                    MalType::Symbol(ref sym) if sym == "eval" => {
                        ast = EVAL(list[1].clone(), env.clone())?;
                        while let Some(ref e) = env.clone().outer {
                            env = e.clone()
                        }
                        continue 'tco;
                    }
                    MalType::Symbol(ref sym) if sym == "quote" => Ok(list[1].clone()),
                    MalType::Symbol(ref a0sym) if a0sym == "quasiquoteexpand" => {
                        Ok(quasiquote(&list[1]))
                    }
                    MalType::Symbol(ref sym) if sym == "quasiquote" => {
                        ast = quasiquote(&list[1]);
                        continue 'tco;
                    }
                    _ => match eval_ast(&ast, &env)? {
                        MalType::List(ref list) => {
                            let f = &list[0].clone();
                            let args = list[1..].to_vec();
                            match f {
                                MalType::Func(_) => f.apply(args),
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
    let mut args = std::env::args();
    let arg1 = args.nth(1);

    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let repl_env = env_new(None);
    for (key, val) in ns() {
        env_sets(&repl_env, key, val);
    }
    env_sets(&repl_env, "*ARGV*", list!(args.map(MalType::Str).collect()));

    let _ = rep("(def! not (fn* (a) (if a false true)))", &repl_env);
    let _ = rep(
        r#"(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))"#,
        &repl_env,
    );
    if let Some(f) = arg1 {
        match rep(&format!("(load-file \"{}\")", f), &repl_env) {
            Ok(_) => std::process::exit(0),
            Err(e) => {
                println!("Error: {}", pr_str(Err(e), true));
                std::process::exit(1);
            }
        }
    }
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
