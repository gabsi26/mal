use crate::types::MalType::{
    False, Func, Hash, Int, Keyword, List, Nil, Str, Symbol, True, Vector,
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
}

impl MalType {
    pub fn apply(&self, args: MalArgs) -> MalRes {
        if let Func(f) = *self {
            f(args)
        } else {
            Err(MalErr::CalledNonFunctionType)
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

impl PartialEq for MalType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(ref a), Int(ref b)) => a == b,
            (Str(ref a), Str(ref b)) => a == b,
            (Symbol(ref a), Symbol(ref b)) => a == b,
            (List(ref a), List(ref b))
            | (List(ref a), Vector(ref b))
            | (Vector(ref a), List(ref b))
            | (Vector(ref a), Vector(ref b)) => a == b,
            (Hash(ref a), Hash(ref b)) => a == b,
            _ => false,
        }
    }
}
