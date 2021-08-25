use std::borrow::BorrowMut;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use crate::printer::pr_seq;
use crate::read_str;
use crate::types::MalType::{
    False, Func, Hash, Int, Keyword, List, MalFunc, Nil, Str, Symbol, True, Vector,
};
use crate::types::{
    atom, func, MalArgs, MalErr, MalRes,
    MalType::{self},
};
use crate::{list, vector};

macro_rules! fn_t_int_int {
    ($ret:ident, $fn:expr) => {{
        |a: MalArgs| match (a[0].clone(), a[1].clone()) {
            (Int(a0), Int(a1)) => Ok($ret($fn(a0, a1))),
            _ => Err(MalErr::WrongTypeForOperation),
        }
    }};
}

macro_rules! fn_str {
    ($fn:expr) => {{
        |a: MalArgs| match a[0].clone() {
            Str(a0) => $fn(a0),
            _ => Err(MalErr::WrongTypeForOperation),
        }
    }};
}

fn slurp(f: String) -> MalRes {
    let mut s = String::new();
    match File::open(f).and_then(|mut f| f.read_to_string(&mut s)) {
        Ok(_) => Ok(MalType::Str(s)),
        Err(e) => Err(MalErr::ReadError),
    }
}

pub fn ns() -> Vec<(&'static str, MalType)> {
    vec![
        ("+", func(fn_t_int_int!(Int, |i, j| { i + j }))),
        ("-", func(fn_t_int_int!(Int, |i, j| { i - j }))),
        ("*", func(fn_t_int_int!(Int, |i, j| { i * j }))),
        ("/", func(fn_t_int_int!(Int, |i, j| { i / j }))),
        (
            "=",
            func(|c| {
                if c[0] == c[1] {
                    Ok(MalType::True)
                } else {
                    Ok(MalType::False)
                }
            }),
        ),
        (
            ">",
            func(|c| {
                if let (MalType::Int(a), MalType::Int(b)) = (c[0].clone(), c[1].clone()) {
                    if a > b {
                        Ok(MalType::True)
                    } else {
                        Ok(MalType::False)
                    }
                } else {
                    Ok(MalType::False)
                }
            }),
        ),
        (
            "<",
            func(|c| {
                if let (MalType::Int(a), MalType::Int(b)) = (c[0].clone(), c[1].clone()) {
                    if a < b {
                        Ok(MalType::True)
                    } else {
                        Ok(MalType::False)
                    }
                } else {
                    Ok(MalType::False)
                }
            }),
        ),
        (
            ">=",
            func(|c| {
                if let (MalType::Int(a), MalType::Int(b)) = (c[0].clone(), c[1].clone()) {
                    if a >= b {
                        Ok(MalType::True)
                    } else {
                        Ok(MalType::False)
                    }
                } else {
                    Ok(MalType::False)
                }
            }),
        ),
        (
            "<=",
            func(|c| {
                if let (MalType::Int(a), MalType::Int(b)) = (c[0].clone(), c[1].clone()) {
                    if a <= b {
                        Ok(MalType::True)
                    } else {
                        Ok(MalType::False)
                    }
                } else {
                    Ok(MalType::False)
                }
            }),
        ),
        ("list", func(|c| Ok(list!(c)))),
        (
            "list?",
            func(|c| {
                if let MalType::List(_) = c[0] {
                    Ok(MalType::True)
                } else {
                    Ok(MalType::False)
                }
            }),
        ),
        (
            "empty?",
            func(|c| match c[0].clone() {
                MalType::List(list) => {
                    if list.is_empty() {
                        Ok(MalType::True)
                    } else {
                        Ok(MalType::False)
                    }
                }
                MalType::Vector(vector) => {
                    if vector.is_empty() {
                        Ok(MalType::True)
                    } else {
                        Ok(MalType::False)
                    }
                }
                _ => Ok(MalType::Nil),
            }),
        ),
        (
            "count",
            func(|c| match c[0].clone() {
                MalType::List(list) => Ok(MalType::Int(list.len() as isize)),
                MalType::Vector(vector) => Ok(MalType::Int(vector.len() as isize)),
                _ => Ok(MalType::Int(0)),
            }),
        ),
        (
            "prn",
            func(|c| {
                println!("{}", pr_seq(&c, true, "", "", " "));
                Ok(MalType::Nil)
            }),
        ),
        (
            "pr-str",
            func(|c| Ok(MalType::Str(pr_seq(&c, true, "", "", " ")))),
        ),
        (
            "str",
            func(|c| Ok(MalType::Str(pr_seq(&c, false, "", "", "")))),
        ),
        (
            "println",
            func(|c| {
                println!("{}", pr_seq(&c, false, "", "", " "));
                Ok(MalType::Nil)
            }),
        ),
        ("read-string", func(fn_str!(|s| { read_str(s) }))),
        ("slurp", func(fn_str!(|s| { slurp(s) }))),
        ("atom", func(|c| Ok(atom(&c[0])))),
        ("atom?", func(|c| Ok(c[0].is_atom()))),
        ("deref", func(|c| c[0].deref())),
        ("reset!", func(|c| c[0].reset_bang(&c[1].clone()))),
        ("swap!", func(|c| c[0].swap_bang(&c[1..].to_vec()))),
    ]
}
