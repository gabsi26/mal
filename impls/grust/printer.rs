use std::rc::Rc;

use crate::types::{MalErr, MalRes, MalType};
use crate::utils::escape;

pub fn pr_str(value: MalRes, print_readably: bool) -> String {
    let mut result = String::new();
    match value {
        Ok(MalType::List(list)) => {
            result.push('(');
            for item in list {
                result.push_str(pr_str(Ok(item), print_readably).as_str());
                result.push(' ');
            }
            if result.len() > 1 {
                result.remove(result.len() - 1);
            }

            result.push(')');
        }
        Ok(MalType::Vector(vector)) => {
            result.push('[');
            for item in vector {
                result.push_str(pr_str(Ok(item), print_readably).as_str());
                result.push(' ');
            }
            if result.len() > 1 {
                result.remove(result.len() - 1);
            }

            result.push(']');
        }
        Ok(MalType::Hash(hash)) => {
            result.push('{');
            for (key, value) in hash.iter() {
                if key.starts_with('\u{029e}') {
                    result.push_str(format!(":{}", &key[2..]).as_str());
                } else {
                    result.push('"');
                    result.push_str(key);
                    result.push('"');
                }

                result.push(' ');
                result.push_str(pr_str(Ok(value.to_owned()), print_readably).as_str());
                result.push(' ');
            }
            if result.len() > 1 {
                result.remove(result.len() - 1);
            }

            result.push('}');
        }
        Ok(MalType::Int(num)) => result.push_str(num.to_string().as_str()),
        Ok(MalType::Symbol(symbol)) => result.push_str(symbol.as_str()),
        Ok(MalType::Keyword(keyword)) => {
            result.push_str(format!(":{}", &keyword[2..]).as_str());
        }
        Ok(MalType::Str(string)) => {
            result.push('"');
            if print_readably {
                result.push_str(escape(string).as_str());
            } else {
                result.push_str(string.as_str());
            }

            result.push('"');
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
