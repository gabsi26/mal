use lazy_static::lazy_static;
use rustyline::Editor;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::printer::pr_seq;
use crate::read_str;
use crate::types::MalType::{
    False, Func, Hash, Int, Keyword, List, MalFunc, Nil, Str, Symbol, True, Vector,
};
use crate::types::{
    assoc, atom, dissoc, func, hash_map, keyword, symbol, vect, MalArgs, MalErr, MalRes,
    MalType::{self},
};
use crate::{list, vector};

macro_rules! fn_t_int_int {
    ($ret:ident, $fn:expr) => {{
        |a: MalArgs| match (a[0].clone(), a[1].clone()) {
            (Int(a0), Int(a1)) => Ok($ret($fn(a0, a1))),
            _ => Err(MalErr::ErrStr("Error: Non numeric arguments".to_string())),
        }
    }};
}

macro_rules! fn_str {
    ($fn:expr) => {{
        |a: MalArgs| match a[0].clone() {
            Str(a0) => $fn(a0),
            _ => Err(MalErr::ErrStr("Error: Expected String".to_string())),
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
        List(list, _) | Vector(list, _) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend_from_slice(&list);
            Ok(list!(new_list.to_vec()))
        }
        _ => Err(MalErr::ErrStr("Error: Non list type found".to_string())),
    }
}

fn concat(args: MalArgs) -> MalRes {
    if args.is_empty() {
        return Ok(list!(vec![]));
    }
    let mut new_list: MalArgs = vec![];
    for list in args {
        match list {
            MalType::List(l, _) | MalType::Vector(l, _) => {
                new_list.extend_from_slice(&l);
            }
            _ => return Err(MalErr::ErrStr("Error: Non list type found".to_string())),
        }
    }
    Ok(list!(new_list.to_vec()))
}

fn vec(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v, _) | Vector(ref v, _) => Ok(vector!(v.to_vec())),
        _ => Err(MalErr::ErrStr("Error: Non list type found".to_string())),
    }
}

fn nth(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v, _) | Vector(ref v, _) => match a[1] {
            MalType::Int(index) => match v.get(index as usize) {
                Some(val) => Ok(val.clone()),
                _ => Err(MalErr::ErrStr("Error: Index out of bounds".to_string())),
            },
            _ => Err(MalErr::ErrStr("Error: Expected Int".to_string())),
        },
        _ => Err(MalErr::ErrStr("Error: Non list type found".to_string())),
    }
}

fn first(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v, _) | Vector(ref v, _) => {
            if v.is_empty() {
                Ok(Nil)
            } else {
                Ok(v[0].clone())
            }
        }
        Nil => Ok(Nil),
        _ => Err(MalErr::ErrStr(
            "Error: Non-list or non-nil type found".to_string(),
        )),
    }
}

fn rest(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v, _) | Vector(ref v, _) => {
            if v.is_empty() {
                Ok(list!(vec![]))
            } else {
                Ok(list!(v[1..].to_vec()))
            }
        }
        Nil => Ok(list!(vec![])),
        _ => Err(MalErr::ErrStr(
            "Error: Non-list or non-nil type found".to_string(),
        )),
    }
}

fn apply(a: MalArgs) -> MalRes {
    match a[a.len() - 1] {
        List(ref v, _) | Vector(ref v, _) => {
            let f = &a[0];
            let mut fargs = a[1..a.len() - 1].to_vec();
            fargs.extend_from_slice(&v);
            f.apply(fargs)
        }
        _ => Err(MalErr::ErrStr(
            "Error: Apply called with non list type".to_string(),
        )),
    }
}

fn map(a: MalArgs) -> MalRes {
    match a[1] {
        List(ref v, _) | Vector(ref v, _) => {
            let mut res = vec![];
            for mv in v.iter() {
                res.push(a[0].apply(vec![mv.clone()])?)
            }
            Ok(list!(res))
        }
        _ => Err(MalErr::ErrStr(
            "Error: map called with non list type".to_string(),
        )),
    }
}

fn get(a: MalArgs) -> MalRes {
    match a[0].clone() {
        Hash(hm, _) => match a[1].clone() {
            MalType::Symbol(s) => {
                if let Some(mv) = hm.get(&s) {
                    Ok(mv.clone())
                } else {
                    Ok(MalType::Nil)
                }
            }
            MalType::Keyword(s) => {
                if let Some(mv) = hm.get(&s) {
                    Ok(mv.clone())
                } else {
                    Ok(MalType::Nil)
                }
            }
            MalType::Str(s) => {
                if let Some(mv) = hm.get(&s) {
                    Ok(mv.clone())
                } else {
                    Ok(MalType::Nil)
                }
            }
            _ => Err(MalErr::ErrStr("Wrong key type".to_string())),
        },
        _ => Ok(MalType::Nil),
    }
}

fn contains(a: MalArgs) -> MalRes {
    match a[0].clone() {
        Hash(hm, _) => match a[1].clone() {
            MalType::Symbol(s) => {
                if hm.contains_key(&s) {
                    Ok(MalType::True)
                } else {
                    Ok(MalType::False)
                }
            }
            MalType::Keyword(s) => {
                if hm.contains_key(&s) {
                    Ok(MalType::True)
                } else {
                    Ok(MalType::False)
                }
            }
            MalType::Str(s) => {
                if hm.contains_key(&s) {
                    Ok(MalType::True)
                } else {
                    Ok(MalType::False)
                }
            }
            _ => Err(MalErr::ErrStr("Wrong key type".to_string())),
        },
        _ => Ok(MalType::False),
    }
}

fn readline(a: MalArgs) -> MalRes {
    lazy_static! {
        static ref RL: Mutex<Editor<()>> = Mutex::new(Editor::<()>::new());
    }
    match a[0] {
        Str(ref p) => match RL.lock().unwrap().readline(p) {
            Ok(mut line) => {
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Ok(Str(line))
            }
            Err(rustyline::error::ReadlineError::Eof) => Ok(MalType::Nil),
            Err(e) => Err(MalErr::ErrStr(format!("{:?}", e))),
        },
        _ => Err(MalErr::ErrStr("Prompt is not Str".to_string())),
    }
}

fn keys(a: MalArgs) -> MalRes {
    match a[0].clone() {
        Hash(hm, _) => {
            let mut list: MalArgs = vec![];
            for key in hm.keys() {
                if key.starts_with('\u{029e}') {
                    list.push(MalType::Keyword(key.to_string()))
                } else {
                    list.push(MalType::Str(key.to_string()))
                }
            }
            Ok(list!(list))
        }
        _ => Ok(MalType::False),
    }
}

fn vals(a: MalArgs) -> MalRes {
    match a[0].clone() {
        Hash(hm, _) => {
            let mut list: MalArgs = vec![];
            for val in hm.values() {
                list.push(val.clone());
            }
            Ok(list!(list))
        }
        _ => Ok(MalType::False),
    }
}

fn meta(a: MalArgs) -> MalRes {
    match a[0].clone() {
        List(_, m) | Vector(_, m) | Hash(_, m) | Func(_, m) | MalFunc { meta: m, .. } => {
            Ok((*m).clone())
        }
        _ => Err(MalErr::ErrStr("Type has no meta-data".to_string())),
    }
}
fn with_meta(a: MalArgs) -> MalRes {
    let new_meta = a[1].clone();
    match a[0].clone() {
        List(l, _) => Ok(List(l, Rc::new(new_meta))),
        Vector(l, _) => Ok(Vector(l, Rc::new(new_meta))),
        Hash(l, _) => Ok(Hash(l, Rc::new(new_meta))),
        Func(l, _) => Ok(Func(l, Rc::new(new_meta))),
        MalFunc {
            eval,
            env,
            ast,
            params,
            is_macro,
            ..
        } => Ok(MalFunc {
            eval,
            env,
            ast,
            params,
            is_macro,
            meta: Rc::new(new_meta),
        }),
        _ => Err(MalErr::ErrStr("Type has no meta-data".to_string())),
    }
}

fn time_ms(_a: MalArgs) -> MalRes {
    let ms_e = match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => d,
        Err(e) => return Err(MalErr::ErrStr(format!("{:?}", e))),
    };
    Ok(Int(
        ms_e.as_secs() as isize * 1000 + ms_e.subsec_nanos() as isize / 1_000_000
    ))
}

fn conj(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v, _) => {
            let sl = a[1..].iter().rev().cloned().collect::<Vec<MalType>>();
            Ok(list!([&sl[..], v].concat()))
        }
        Vector(ref v, _) => Ok(vector!([v, &a[1..]].concat())),
        _ => Err(MalErr::ErrStr("conj called with non sequence".to_string())),
    }
}

fn seq(a: MalArgs) -> MalRes {
    match a[0] {
        List(ref v, _) | Vector(ref v, _) if v.len() == 0 => Ok(Nil),
        List(ref v, _) | Vector(ref v, _) => Ok(list!(v.to_vec())),
        Str(ref s) if s.is_empty() => Ok(Nil),
        Str(ref s) if a[0].is_keyword() == MalType::False => {
            Ok(list!(s.chars().map(|c| { Str(c.to_string()) }).collect()))
        }
        Nil => Ok(Nil),
        _ => Err(MalErr::ErrStr("seq called with non sequence".to_string())),
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
                if let MalType::List(_, _) = c[0] {
                    Ok(MalType::True)
                } else {
                    Ok(MalType::False)
                }
            }),
        ),
        (
            "empty?",
            func(|c| match c[0].clone() {
                MalType::List(list, _) => {
                    if list.is_empty() {
                        Ok(MalType::True)
                    } else {
                        Ok(MalType::False)
                    }
                }
                MalType::Vector(vector, _) => {
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
                MalType::List(list, _) => Ok(MalType::Int(list.len() as isize)),
                MalType::Vector(vector, _) => Ok(MalType::Int(vector.len() as isize)),
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
        ("symbol", func(|c| symbol(&c[0]))),
        ("symbol?", func(|c| Ok(c[0].is_symbol()))),
        ("nil?", func(|c| Ok(c[0].is_nil()))),
        ("true?", func(|c| Ok(c[0].is_true()))),
        ("false?", func(|c| Ok(c[0].is_false()))),
        ("keyword", func(|c| keyword(&c[0]))),
        ("keyword?", func(|c| Ok(c[0].is_keyword()))),
        ("vector", func(vect)),
        ("vector?", func(|c| Ok(c[0].is_vector()))),
        ("sequential?", func(|c| Ok(c[0].is_sequential()))),
        ("hash-map", func(hash_map)),
        ("get", func(get)),
        ("contains?", func(contains)),
        ("keys", func(keys)),
        ("vals", func(vals)),
        ("assoc", func(assoc)),
        ("dissoc", func(dissoc)),
        ("map?", func(|c| Ok(c[0].is_map()))),
        ("deref", func(|c| c[0].deref())),
        ("reset!", func(|c| c[0].reset_bang(&c[1].clone()))),
        ("swap!", func(|c| c[0].swap_bang(&c[1..].to_vec()))),
        ("cons", func(cons)),
        ("concat", func(concat)),
        ("vec", func(vec)),
        ("nth", func(nth)),
        ("first", func(first)),
        ("rest", func(rest)),
        ("throw", func(|c| Err(MalErr::ErrVal(c[0].clone())))),
        ("apply", func(apply)),
        ("map", func(map)),
        ("readline", func(readline)),
        ("meta", func(meta)),
        ("with-meta", func(with_meta)),
        ("time-ms", func(time_ms)),
        ("fn?", func(|c| Ok(c[0].is_fn()))),
        ("string?", func(|c| Ok(c[0].is_string()))),
        ("number?", func(|c| Ok(c[0].is_number()))),
        ("macro?", func(|c| Ok(c[0].is_macro()))),
        ("seq", func(seq)),
        ("conj", func(conj)),
    ]
}
