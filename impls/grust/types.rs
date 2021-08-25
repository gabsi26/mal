use crate::environment::{env_bind, env_new, Env};
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
      List(Rc::new($seq))
    }};
    [$($args:expr),*] => {{
      let v: Vec<MalVal> = vec![$($args),*];
      List(Rc::new(v))
    }}
  }

#[macro_export]
macro_rules! vector {
    ($seq:expr) => {{
      Vector(Rc::new($seq))
    }};
    [$($args:expr),*] => {{
      let v: Vec<MalType> = vec![$($args),*];
      Vector(Rc::new(v))
    }}
  }

#[derive(Debug, Clone)]
pub enum MalType {
    Nil,
    True,
    False,
    List(Rc<Vec<MalType>>),
    Vector(Rc<Vec<MalType>>),
    Hash(Rc<HashMap<String, MalType>>),
    Int(isize),
    Symbol(String),
    Str(String),
    Keyword(String),
    Quote(Rc<MalType>),
    Quasiquote(Rc<MalType>),
    Unquote(Rc<MalType>),
    SpliceUnquote(Rc<MalType>),
    Meta(Rc<MalType>, Rc<MalType>),
    Func(fn(MalArgs) -> MalRes),
    MalFunc {
        eval: fn(ast: MalType, env: Env) -> MalRes,
        ast: Rc<MalType>,
        env: Env,
        params: Rc<MalType>,
    },
    Atom(Rc<RefCell<MalType>>),
}

impl MalType {
    pub fn apply(&self, args: MalArgs) -> MalRes {
        match *self {
            Func(f) => f(args),
            MalFunc {
                eval,
                ref ast,
                ref env,
                ref params,
            } => {
                let a = &**ast;
                let p = &**params;
                let fn_env = env_bind(Some(env.clone()), p.clone(), args)?;
                Ok(eval(a.clone(), fn_env)?)
            }
            _ => Err(MalErr::CalledNonFunctionType),
        }
    }
    pub fn is_atom(&self) -> MalType {
        if let &MalType::Atom(_) = self {
            MalType::True
        } else {
            MalType::False
        }
    }

    pub fn deref(&self) -> MalRes {
        match self {
            MalType::Atom(a) => Ok(a.borrow().clone()),
            _ => Err(MalErr::WrongTypeForOperation),
        }
    }

    pub fn reset_bang(&self, new: &MalType) -> MalRes {
        match self {
            MalType::Atom(a) => {
                *a.borrow_mut() = new.clone();
                Ok(new.clone())
            }
            _ => Err(MalErr::WrongTypeForOperation),
        }
    }

    pub fn swap_bang(&self, args: &MalArgs) -> MalRes {
        match self {
            MalType::Atom(a) => {
                let f = &args[0];
                let mut fargs = args[1..].to_vec();
                fargs.insert(0, a.borrow().clone());
                *a.borrow_mut() = f.apply(fargs)?;
                Ok(a.borrow().clone())
            }
            _ => Err(MalErr::WrongTypeForOperation),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum MalErr {
    NotImplemented,
    EndOfFile,
    UnmatchedDoubleQuote,
    UnknownSequenceEnd,
    OddNumOfElems,
    WrongTypeForOperation,
    CalledNonFunctionType,
    SymbolNotDefined(String),
    UnknownError,
    WrongNumberOfArguments,
    ReadError,
}

pub type MalRes = Result<MalType, MalErr>;
pub type MalArgs = Vec<MalType>;

pub fn hash(seq: Vec<MalType>) -> MalRes {
    let mut hash: HashMap<String, MalType> = HashMap::new();
    if seq.len() % 2 != 0 {
        return Err(MalErr::OddNumOfElems);
    }
    for (k, v) in seq.iter().tuples() {
        let key = match k {
            Keyword(kw) => kw,
            Str(str) => str,
            _ => return Err(MalErr::WrongTypeForOperation),
        };
        hash.insert(key.to_owned(), v.to_owned());
    }
    Ok(MalType::Hash(Rc::new(hash)))
}

pub fn func(f: fn(MalArgs) -> MalRes) -> MalType {
    MalType::Func(f)
}
pub fn atom(mv: &MalType) -> MalType {
    MalType::Atom(Rc::new(RefCell::new(mv.clone())))
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
            (List(ref a), List(ref b))
            | (List(ref a), Vector(ref b))
            | (Vector(ref a), List(ref b))
            | (Vector(ref a), Vector(ref b)) => a == b,
            (Hash(ref a), Hash(ref b)) => a == b,
            _ => false,
        }
    }
}
