use crate::environment::{env_bind, Env};
use crate::types::MalType::{
    False, Func, Hash, Int, Keyword, List, MalFunc, Nil, Str, Symbol, True, Vector,
};
use itertools::Itertools;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[macro_export]
macro_rules! list {
    ($seq:expr) => {{
      List(Rc::new($seq),Rc::new(Nil))
    }};
    [$($args:expr),*] => {{
      let v: Vec<MalType> = vec![$($args),*];
      List(Rc::new(v),Rc::new(Nil))
    }}
  }

#[macro_export]
macro_rules! vector {
    ($seq:expr) => {{
      Vector(Rc::new($seq),Rc::new(Nil))
    }};
    [$($args:expr),*] => {{
      let v: Vec<MalType> = vec![$($args),*];
      Vector(Rc::new(v),Rc::new(Nil))
    }}
  }

#[derive(Debug, Clone)]
pub enum MalType {
    Nil,
    True,
    False,
    List(Rc<Vec<MalType>>, Rc<MalType>),
    Vector(Rc<Vec<MalType>>, Rc<MalType>),
    Hash(Rc<HashMap<String, MalType>>, Rc<MalType>),
    Int(isize),
    Symbol(String),
    Str(String),
    Keyword(String),
    Func(fn(MalArgs) -> MalRes, Rc<MalType>),
    #[allow(dead_code)]
    MalFunc {
        eval: fn(ast: MalType, env: Env) -> MalRes,
        ast: Rc<MalType>,
        env: Env,
        params: Rc<MalType>,
        is_macro: bool,
        meta: Rc<MalType>,
    },
    Atom(Rc<RefCell<MalType>>),
}

impl MalType {
    pub fn apply(&self, args: MalArgs) -> MalRes {
        match *self {
            Func(f, _) => f(args),
            MalFunc {
                eval,
                ref ast,
                ref env,
                ref params,
                ..
            } => {
                let a = &**ast;
                let p = &**params;
                let fn_env = env_bind(Some(env.clone()), p.clone(), args)?;
                Ok(eval(a.clone(), fn_env)?)
            }
            _ => Err(MalErr::ErrStr(
                "Error: Tried to call non function type".to_string(),
            )),
        }
    }
    #[allow(dead_code)]
    pub fn is_atom(&self) -> MalType {
        if let MalType::Atom(_) = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_symbol(&self) -> MalType {
        if let MalType::Symbol(_) = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_nil(&self) -> MalType {
        if let MalType::Nil = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_true(&self) -> MalType {
        if let MalType::True = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_false(&self) -> MalType {
        if let MalType::False = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_keyword(&self) -> MalType {
        if let MalType::Keyword(_) = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_vector(&self) -> MalType {
        if let MalType::Vector(_, _) = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_sequential(&self) -> MalType {
        if let MalType::Vector(_, _) | MalType::List(_, _) = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_map(&self) -> MalType {
        if let MalType::Hash(_, _) = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_string(&self) -> MalType {
        if let MalType::Str(_) = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_number(&self) -> MalType {
        if let MalType::Int(__) = self {
            MalType::True
        } else {
            MalType::False
        }
    }
    #[allow(dead_code)]
    pub fn is_fn(&self) -> MalType {
        match self {
            MalType::Func(_, _) => MalType::True,
            MalType::MalFunc { is_macro: m, .. } if !m => MalType::True,
            _ => MalType::False,
        }
    }
    #[allow(dead_code)]
    pub fn is_macro(&self) -> MalType {
        match self {
            MalType::MalFunc { is_macro: m, .. } if *m => MalType::True,
            MalType::MalFunc { .. } => MalType::False,
            _ => MalType::False,
        }
    }
    #[allow(dead_code)]
    pub fn deref(&self) -> MalRes {
        match self {
            MalType::Atom(a) => Ok(a.borrow().clone()),
            _ => Err(MalErr::ErrStr(
                "Error: Tried to deref non atom type".to_string(),
            )),
        }
    }
    #[allow(dead_code)]
    pub fn reset_bang(&self, new: &MalType) -> MalRes {
        match self {
            MalType::Atom(a) => {
                *a.borrow_mut() = new.clone();
                Ok(new.clone())
            }
            _ => Err(MalErr::ErrStr(
                "Error: Tried to reset non atom type".to_string(),
            )),
        }
    }
    #[allow(dead_code)]
    pub fn swap_bang(&self, args: &[MalType]) -> MalRes {
        match self {
            MalType::Atom(a) => {
                let f = &args[0];
                let mut fargs = args[1..].to_vec();
                fargs.insert(0, a.borrow().clone());
                *a.borrow_mut() = f.apply(fargs)?;
                Ok(a.borrow().clone())
            }
            _ => Err(MalErr::ErrStr(
                "Error: Tried to swap non atom type".to_string(),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MalErr {
    #[allow(dead_code)]
    ErrVal(MalType),
    ErrStr(String),
}

pub type MalRes = Result<MalType, MalErr>;
pub type MalArgs = Vec<MalType>;

pub fn hash(seq: Vec<MalType>) -> MalRes {
    let mut hash: HashMap<String, MalType> = HashMap::new();
    if seq.len() % 2 != 0 {
        return Err(MalErr::ErrStr(
            "Error: Odd number of elements in hash".to_string(),
        ));
    }
    for (k, v) in seq.iter().tuples() {
        let key = match k {
            Keyword(kw) => kw,
            Str(str) => str,
            _ => {
                return Err(MalErr::ErrStr(
                    "Error: Wrong datatype used as hashkey".to_string(),
                ))
            }
        };
        hash.insert(key.to_owned(), v.to_owned());
    }
    Ok(MalType::Hash(Rc::new(hash), Rc::new(MalType::Nil)))
}
#[allow(dead_code)]
pub fn func(f: fn(MalArgs) -> MalRes) -> MalType {
    MalType::Func(f, Rc::new(MalType::Nil))
}
#[allow(dead_code)]
pub fn atom(mv: &MalType) -> MalType {
    MalType::Atom(Rc::new(RefCell::new(mv.clone())))
}
#[allow(dead_code)]
pub fn symbol(a: &MalType) -> MalRes {
    match a {
        MalType::Str(s) => Ok(MalType::Symbol(s.to_string())),
        _ => Err(MalErr::ErrStr(
            "Tried to convert non string into symbol".to_string(),
        )),
    }
}
#[allow(dead_code)]
pub fn keyword(a: &MalType) -> MalRes {
    match a {
        MalType::Str(s) => Ok(MalType::Keyword(format!("\u{029e}{}", s))),
        MalType::Keyword(_) => Ok(a.clone()),
        _ => Err(MalErr::ErrStr(
            "Tried to convert non string into keyword".to_string(),
        )),
    }
}
#[allow(dead_code)]
pub fn vect(a: MalArgs) -> MalRes {
    let mut vec: MalArgs = vec![];
    for arg in a.iter() {
        vec.push(arg.clone());
    }
    Ok(vector!(vec))
}
#[allow(dead_code)]
pub fn hash_map(a: MalArgs) -> MalRes {
    let mut hm: HashMap<String, MalType> = HashMap::new();
    for (key, val) in a.iter().tuples() {
        match key {
            MalType::Symbol(s) => {
                hm.insert(s.clone(), val.clone());
            }
            MalType::Keyword(s) => {
                hm.insert(s.clone(), val.clone());
            }
            MalType::Str(s) => {
                hm.insert(s.clone(), val.clone());
            }
            _ => return Err(MalErr::ErrStr("Wrong key type".to_string())),
        }
    }
    Ok(Hash(Rc::new(hm), Rc::new(MalType::Nil)))
}
#[allow(dead_code)]
pub fn assoc(a: MalArgs) -> MalRes {
    match a[0].clone() {
        MalType::Hash(ohm, _) => {
            let mut hm: HashMap<String, MalType> = HashMap::new();
            for (k, v) in ohm.iter() {
                hm.insert(k.clone(), v.clone());
            }
            for (key, val) in a[1..].iter().tuples() {
                match key {
                    MalType::Symbol(s) => {
                        hm.insert(s.clone(), val.clone());
                    }
                    MalType::Keyword(s) => {
                        hm.insert(s.clone(), val.clone());
                    }
                    MalType::Str(s) => {
                        hm.insert(s.clone(), val.clone());
                    }
                    _ => return Err(MalErr::ErrStr("Wrong key type".to_string())),
                }
            }
            Ok(Hash(Rc::new(hm), Rc::new(MalType::Nil)))
        }
        _ => Err(MalErr::ErrStr("Expected hash-map".to_string())),
    }
}
#[allow(dead_code)]
pub fn dissoc(a: MalArgs) -> MalRes {
    match a[0].clone() {
        MalType::Hash(ohm, _) => {
            let mut hm = (*ohm).clone();
            for key in a[1..].iter() {
                match key {
                    MalType::Symbol(s) => {
                        hm.remove(s);
                    }
                    MalType::Keyword(s) => {
                        hm.remove(s);
                    }
                    MalType::Str(s) => {
                        hm.remove(s);
                    }
                    _ => return Err(MalErr::ErrStr("Wrong key type".to_string())),
                }
            }
            Ok(Hash(Rc::new(hm), Rc::new(MalType::Nil)))
        }
        _ => Err(MalErr::ErrStr("Expected hash-map".to_string())),
    }
}

impl PartialEq for MalType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(ref a), Int(ref b)) => a == b,
            (Str(ref a), Str(ref b)) => a == b,
            (Symbol(ref a), Symbol(ref b)) => a == b,
            (Keyword(ref a), Keyword(ref b)) => a == b,
            (List(ref a, _), List(ref b, _))
            | (List(ref a, _), Vector(ref b, _))
            | (Vector(ref a, _), List(ref b, _))
            | (Vector(ref a, _), Vector(ref b, _)) => a == b,
            (Hash(ref a, _), Hash(ref b, _)) => a == b,
            _ => false,
        }
    }
}
