use std::rc::Rc;

use crate::types::{MalErr, MalRes, MalType};
use crate::utils::escape;

pub fn pr_seq(seq: &[MalType], print_readably: bool, start: &str, end: &str, join: &str) -> String {
    let strs: Vec<String> = seq
        .iter()
        .map(|x| pr_str(Ok(x.clone()), print_readably))
        .collect();
    format!("{}{}{}", start, strs.join(join), end)
}

pub fn pr_str(value: MalRes, print_readably: bool) -> String {
    let mut result = String::new();
    match value {
        Ok(MalType::List(list)) => {
            result.push_str(pr_seq(&list, print_readably, "(", ")", " ").as_str())
        }
        Ok(MalType::Vector(vector)) => {
            result.push_str(pr_seq(&vector, print_readably, "[", "]", " ").as_str())
        }
        Ok(MalType::Hash(hash)) => {
            let h: Vec<MalType> = hash
                .iter()
                .flat_map(|(key, val)| vec![MalType::Str(key.to_string()), val.clone()])
                .collect();
            result.push_str(pr_seq(&h, print_readably, "{", "}", " ").as_str());
        }
        Ok(MalType::Int(num)) => result.push_str(num.to_string().as_str()),
        Ok(MalType::Symbol(symbol)) => result.push_str(symbol.as_str()),
        Ok(MalType::Keyword(keyword)) => {
            result.push_str(format!(":{}", &keyword[2..]).as_str());
        }
        Ok(MalType::Str(string)) => {
            if string.starts_with("\u{29e}") {
                result.push_str(format!(":{}", &string[2..]).as_str())
            } else if print_readably {
                result.push_str(format!("\"{}\"", escape(string)).as_str())
            } else {
                result.push_str(string.as_str())
            }
        }
        Ok(MalType::Quote(quoted)) => {
            result.push_str("(quote ");
            result.push_str(pr_str(Ok(Rc::try_unwrap(quoted).unwrap()), print_readably).as_str());
            result.push(')');
        }
        Ok(MalType::Quasiquote(quoted)) => {
            result.push_str("(quasiquote ");
            result.push_str(pr_str(Ok(Rc::try_unwrap(quoted).unwrap()), print_readably).as_str());
            result.push(')');
        }
        Ok(MalType::Unquote(quoted)) => {
            result.push_str("(unquote ");
            result.push_str(pr_str(Ok(Rc::try_unwrap(quoted).unwrap()), print_readably).as_str());
            result.push(')');
        }
        Ok(MalType::SpliceUnquote(quoted)) => {
            result.push_str("(splice-unquote ");
            result.push_str(pr_str(Ok(Rc::try_unwrap(quoted).unwrap()), print_readably).as_str());
            result.push(')');
        }
        Ok(MalType::Deref(quoted)) => {
            result.push_str("(deref ");
            result.push_str(pr_str(Ok(Rc::try_unwrap(quoted).unwrap()), print_readably).as_str());
            result.push(')');
        }
        Ok(MalType::Meta(hash, vec)) => {
            result.push_str("(with-meta ");
            result.push_str(pr_str(Ok(Rc::try_unwrap(vec).unwrap()), print_readably).as_str());
            result.push(' ');
            result.push_str(pr_str(Ok(Rc::try_unwrap(hash).unwrap()), print_readably).as_str());
            result.push(')');
        }
        Ok(MalType::Func(_)) => result.push_str("#<function>"),
        Ok(MalType::MalFunc {
            eval: _,
            ast: _,
            env: _,
            params: _,
        }) => result.push_str("#<function>"),
        Ok(MalType::Nil) => result.push_str("nil"),
        Ok(MalType::True) => result.push_str("true"),
        Ok(MalType::False) => result.push_str("false"),
        Err(MalErr::EndOfFile) => print!("EOF"),
        Err(MalErr::NotImplemented) => print!("Not implemented"),
        Err(MalErr::UnmatchedDoubleQuote) => print!("unbalanced"),
        Err(MalErr::UnknownSequenceEnd) => print!("unknown sequence end"),
        Err(MalErr::OddNumOfElems) => print!("Odd number of elements"),
        Err(MalErr::WrongTypeForOperation) => print!("Wrong data type for operation"),
        Err(MalErr::CalledNonFunctionType) => print!("Called non function type"),
        Err(MalErr::SymbolNotDefined(sym)) => print!("{} not found", sym),
        Err(MalErr::UnknownError) => print!("Unknown error"),
        Err(MalErr::WrongNumberOfArguments) => print!("Wrong number of arguments"),
        _ => (),
    }
    result
}
