use crate::types::{MalErr, MalRes, MalType};

use crate::list;
use crate::types::MalType::{List, Nil};
use std::cell::RefCell;
use std::{collections::HashMap, rc::Rc};
#[derive(Debug, Clone, PartialEq)]
pub struct EnvStruct {
    pub outer: Option<Rc<EnvStruct>>,
    data: RefCell<HashMap<String, MalType>>,
}

pub type Env = Rc<EnvStruct>;

#[allow(dead_code)]
pub fn env_new(outer: Option<Env>) -> Env {
    Rc::new(EnvStruct {
        data: RefCell::new(HashMap::default()),
        outer,
    })
}

#[allow(dead_code)]
pub fn env_bind(outer: Option<Env>, mbinds: MalType, exprs: Vec<MalType>) -> Result<Env, MalErr> {
    let env = env_new(outer);
    match mbinds {
        MalType::List(binds, _) | MalType::Vector(binds, _) => {
            for (i, b) in binds.iter().enumerate() {
                match b {
                    MalType::Symbol(s) if s == "&" => {
                        env_set(&env, binds[i + 1].clone(), list!(exprs[i..].to_vec()))?;
                        break;
                    }
                    _ => {
                        env_set(&env, b.clone(), exprs[i].clone())?;
                    }
                }
            }
            Ok(env)
        }
        _ => Err(MalErr::ErrStr("Non-list type found".to_string())),
    }
}

#[allow(dead_code)]
pub fn env_find(env: &Env, key: &str) -> Option<Env> {
    match (env.data.borrow().contains_key(key), env.outer.clone()) {
        (true, _) => Some(env.clone()),
        (false, Some(o)) => env_find(&o, key),
        _ => None,
    }
}

#[allow(dead_code)]
pub fn env_get(env: &Env, key: &MalType) -> MalRes {
    match key {
        MalType::Symbol(ref s) => match env_find(env, s) {
            Some(e) => Ok(e
                .data
                .borrow()
                .get(s)
                .ok_or_else(|| MalErr::ErrStr(format!("'{}' not found", s.clone())))?
                .clone()),
            _ => Err(MalErr::ErrStr(format!("'{}' not found", s.clone()))),
        },
        _ => Err(MalErr::ErrStr("Non Symbol type found".to_string())),
    }
}

#[allow(dead_code)]
pub fn env_set(env: &EnvStruct, key: MalType, val: MalType) -> MalRes {
    match key {
        MalType::Symbol(ref s) => {
            env.data.borrow_mut().insert(s.to_string(), val.clone());
            Ok(val)
        }
        _ => Err(MalErr::ErrStr("Non Symbol type found".to_string())),
    }
}

#[allow(dead_code)]
pub fn env_sets(env: &Env, key: &str, val: MalType) {
    env.data.borrow_mut().insert(key.to_string(), val);
}
