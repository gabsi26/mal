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
        Ok(MalType::List(list, _)) => {
            result.push_str(pr_seq(&list, print_readably, "(", ")", " ").as_str())
        }
        Ok(MalType::Vector(vector, _)) => {
            result.push_str(pr_seq(&vector, print_readably, "[", "]", " ").as_str())
        }
        Ok(MalType::Hash(hash, _)) => {
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
            if let Some(stripped) = string.strip_prefix("\u{029e}") {
                result.push_str(format!(":{}", stripped).as_str());
            } else if print_readably {
                result.push_str(format!("\"{}\"", escape(string)).as_str())
            } else {
                result.push_str(string.as_str())
            }
        }
        Ok(MalType::Func(_, _)) => result.push_str("#<function>"),
        Ok(MalType::MalFunc { .. }) => result.push_str("#<function>"),
        Ok(MalType::Atom(a)) => {
            result.push_str(format!("(atom {})", pr_str(Ok(a.borrow().to_owned()), true)).as_str())
        }
        Ok(MalType::Nil) => result.push_str("nil"),
        Ok(MalType::True) => result.push_str("true"),
        Ok(MalType::False) => result.push_str("false"),
        Err(MalErr::ErrStr(s)) => result.push_str(s.as_str()),
        Err(MalErr::ErrVal(val)) => {
            result.push_str(format!("Error: {}", pr_str(Ok(val), print_readably).as_str()).as_str())
        }
    }
    result
}
