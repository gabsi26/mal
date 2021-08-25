use crate::types::{MalErr, MalRes, MalType};

use std::cell::RefCell;
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct EnvStruct {
    outer: Option<Rc<EnvStruct>>,
    data: RefCell<HashMap<String, MalType>>,
}

pub type Env = Rc<EnvStruct>;

pub fn env_new(outer: Option<Env>) -> Env {
    Rc::new(EnvStruct {
        data: RefCell::new(HashMap::default()),
        outer,
    })
}

pub fn env_bind(outer: Option<Env>, mbinds: MalType, exprs: Vec<MalType>) -> Result<Env, MalErr> {
    let env = env_new(outer);
    match mbinds {
        MalType::List(binds) | MalType::Vector(binds) => {
            for (i, b) in binds.iter().enumerate() {
                match b {
                    MalType::Symbol(s) if s == "&" => {
                        env_set(
                            &env,
                            binds[i + 1].clone(),
                            MalType::List(exprs[i..].to_vec()),
                        )?;
                        break;
                    }
                    _ => {
                        env_set(&env, b.clone(), exprs[i].clone())?;
                    }
                }
            }
            Ok(env)
        }
        _ => Err(MalErr::WrongTypeForOperation),
    }
}

pub fn env_find(env: &Env, key: &str) -> Option<Env> {
    match (env.data.borrow().contains_key(key), env.outer.clone()) {
        (true, _) => Some(env.clone()),
        (false, Some(o)) => env_find(&o, key),
        _ => None,
    }
}

pub fn env_get(env: &Env, key: &MalType) -> MalRes {
    match key {
        MalType::Symbol(ref s) => match env_find(env, s) {
            Some(e) => Ok(e
                .data
                .borrow()
                .get(s)
                .ok_or(MalErr::SymbolNotDefined(s.clone()))?
                .clone()),
            _ => Err(MalErr::SymbolNotDefined(s.clone())),
        },
        _ => Err(MalErr::WrongTypeForOperation),
    }
}

pub fn env_set(env: &EnvStruct, key: MalType, val: MalType) -> MalRes {
    match key {
        MalType::Symbol(ref s) => {
            env.data.borrow_mut().insert(s.to_string(), val.clone());
            Ok(val)
        }
        _ => Err(MalErr::WrongTypeForOperation),
    }
}

pub fn env_sets(env: &Env, key: &str, val: MalType) {
    env.data.borrow_mut().insert(key.to_string(), val);
}
