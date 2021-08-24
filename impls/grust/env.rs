use crate::types::{MalErr, MalRes, MalType};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    outer: Option<Rc<Env>>,
    data: HashMap<String, MalType>,
}

impl Env {
    pub fn new(outer: Option<Rc<Env>>) -> Self {
        Env {
            outer,
            data: HashMap::new(),
        }
    }

    pub fn set(&mut self, key: String, value: MalType) {
        self.data.insert(key.clone(), value);
    }

    pub fn find(&self, key: String) -> Option<Rc<Self>> {
        if self.data.contains_key(&key) {
            Some(Rc::new(self.clone()))
        } else if let Some(parent) = self.outer.clone() {
            parent.find(key)
        } else {
            None
        }
    }

    pub fn get(&self, key: &String) -> MalRes {
        if let Some(env) = self.find(key.clone()) {
            if let Some(value) = env.data.get(key) {
                Ok(value.clone())
            } else {
                Err(MalErr::UnknownError)
            }
        } else {
            Err(MalErr::SymbolNotDefined(key.clone()))
        }
    }
}
