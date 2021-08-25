use crate::env::{env_bind, env_new, Env};
use crate::types::MalType::{
    False, Func, Hash, Int, Keyword, List, MalFunc, Nil, Str, Symbol, True, Vector,
};
use itertools::Itertools;

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum MalType {
    Nil,
    True,
    False,
    List(Vec<MalType>),
    Vector(Vec<MalType>),
    Hash(HashMap<String, MalType>),
    Int(isize),
    Symbol(String),
    Str(String),
    Keyword(String),
    Quote(Rc<MalType>),
    Quasiquote(Rc<MalType>),
    Unquote(Rc<MalType>),
    SpliceUnquote(Rc<MalType>),
    Deref(Rc<MalType>),
    Meta(Rc<MalType>, Rc<MalType>),
    Func(fn(MalArgs) -> MalRes),
    MalFunc {
        eval: fn(ast: MalType, env: Env) -> MalRes,
        ast: Rc<MalType>,
        env: Env,
        params: Rc<MalType>,
    },
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
    Ok(MalType::Hash(hash))
}

pub fn func(f: fn(MalArgs) -> MalRes) -> MalType {
    MalType::Func(f)
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
