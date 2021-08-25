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
            _ => Err(MalErr::ErrStr("Non numeric arguments".to_string())),
        }
    }};
}

macro_rules! fn_str {
    ($fn:expr) => {{
        |a: MalArgs| match a[0].clone() {
            Str(a0) => $fn(a0),
            _ => Err(MalErr::ErrStr("Expected String".to_string())),
        }
    }};
}

fn slurp(f: String) -> MalRes {
    let mut s = String::new();
    match File::open(f).and_then(|mut f| f.read_to_string(&mut s)) {
        Ok(_) => Ok(MalType::Str(s)),
        Err(e) => Err(MalErr::ErrStr(format!("{}", e))),
    }
}

fn cons(args: MalArgs) -> MalRes {
    match args[1].clone() {
        List(list) | Vector(list) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend_from_slice(&list);
            Ok(list!(new_list.to_vec()))
        }
        _ => Err(MalErr::ErrStr("Non list type found".to_string())),
    }
}

fn concat(args: MalArgs) -> MalRes {
    if args.is_empty() {
        return Ok(list!(vec![]));
    }
    let mut new_list: MalArgs = vec![];
    for list in args {
        match list {
            MalType::List(l) | MalType::Vector(l) => {
                new_list.extend_from_slice(&l);
            }
            _ => return Err(MalErr::ErrStr("Non list type found".to_string())),
        }
    }
    Ok(list!(new_list.to_vec()))
}

fn vec(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v) | Vector(ref v) => Ok(vector!(v.to_vec())),
        _ => Err(MalErr::ErrStr("Non list type found".to_string())),
    }
}

fn nth(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v) | Vector(ref v) => match a[1] {
            MalType::Int(index) => match v.get(index as usize) {
                Some(val) => Ok(val.clone()),
                _ => Err(MalErr::ErrStr("Index out of bounds".to_string())),
            },
            _ => Err(MalErr::ErrStr("Expected Int".to_string())),
        },
        _ => Err(MalErr::ErrStr("Non list type found".to_string())),
    }
}

fn first(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v) | Vector(ref v) => {
            if v.is_empty() {
                Ok(Nil)
            } else {
                Ok(v[0].clone())
            }
        }
        Nil => Ok(Nil),
        _ => Err(MalErr::ErrStr("Non-list or non-nil type found".to_string())),
    }
}

fn rest(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v) | Vector(ref v) => {
            if v.is_empty() {
                Ok(list!(vec![]))
            } else {
                Ok(list!(v[1..].to_vec()))
            }
        }
        Nil => Ok(list!(vec![])),
        _ => Err(MalErr::ErrStr("Non-list or non-nil type found".to_string())),
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
        ("cons", func(cons)),
        ("concat", func(concat)),
        ("vec", func(vec)),
        ("nth", func(nth)),
        ("first", func(first)),
        ("rest", func(rest)),
    ]
}
