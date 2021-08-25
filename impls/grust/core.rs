use crate::printer::pr_seq;
use crate::types::MalType::{
    False, Func, Hash, Int, Keyword, List, MalFunc, Nil, Str, Symbol, True, Vector,
};
use crate::types::{
    func, MalArgs, MalErr,
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
    ]
}
