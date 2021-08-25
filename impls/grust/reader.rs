use crate::utils::unescape;
use lazy_static::lazy_static;
use regex::Regex;
use std::rc::Rc;

use crate::types::{hash, MalErr, MalRes, MalType};

lazy_static! {
    static ref TOKEN_RE: Regex =
        Regex::new(r##"[\s,]*((?P<tat>~@)|(?P<spe>[\[\]{}()'`~^@])|(?P<str>"(?:\\.|[^\\"])*"?)|(?P<sem>;.*)|(?P<cha>[^\s\[\]{}('"`,;)]*))"##)
            .unwrap();

}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Reader {
    tokens: Vec<Token>,
    position: usize,
}

impl Reader {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    pub fn peek(&self) -> Option<Token> {
        self.tokens.get(self.position).map(|token| token.to_owned())
    }

    pub fn next(&mut self) -> Option<Token> {
        let temp = self.tokens.get(self.position).map(|token| token.to_owned());
        self.position += 1;
        temp
    }

    pub fn read_form(&mut self) -> MalRes {
        match self.peek() {
            Some(Token::Special('(')) => {
                self.next();
                self.read_sequence(')')
            }
            Some(Token::Special('[')) => {
                self.next();
                self.read_sequence(']')
            }
            Some(Token::Special('{')) => {
                self.next();
                self.read_sequence('}')
            }
            _ => self.read_atom(),
        }
    }

    pub fn read_sequence(&mut self, end: char) -> MalRes {
        let mut seq: Vec<MalType> = vec![];
        while self.peek() != Some(Token::Special(end)) && self.peek() != None {
            seq.push(self.read_form()?);
        }
        if self.peek() == Some(Token::Special(end)) {
            self.next();
            match end {
                ')' => Ok(MalType::List(seq)),
                ']' => Ok(MalType::Vector(seq)),
                '}' => hash(seq),
                _ => Err(MalErr::UnknownSequenceEnd),
            }
        } else {
            Err(MalErr::EndOfFile)
        }
    }

    pub fn read_atom(&mut self) -> MalRes {
        match self.next() {
            Some(Token::Chars(chars)) => {
                if let Ok(num) = chars.parse::<isize>() {
                    Ok(MalType::Int(num))
                } else if chars.starts_with(':') {
                    println!("{}", chars);
                    Ok(MalType::Keyword(format!("\u{029e}{}", &chars[1..])))
                } else {
                    match chars.as_str() {
                        "nil" => Ok(MalType::Nil),
                        "true" => Ok(MalType::True),
                        "false" => Ok(MalType::False),
                        _ => Ok(MalType::Symbol(chars)),
                    }
                }
            }
            Some(Token::String(string)) => Ok(MalType::Str(string)),
            Some(Token::Special(c)) => match c.to_string().as_str() {
                "'" => Ok(MalType::Quote(Rc::new(self.read_form()?))),
                "`" => Ok(MalType::Quasiquote(Rc::new(self.read_form()?))),
                "~" => Ok(MalType::Unquote(Rc::new(self.read_form()?))),
                "@" => Ok(MalType::Deref(Rc::new(self.read_form()?))),
                "^" => Ok(MalType::Meta(
                    Rc::new(self.read_form()?),
                    Rc::new(self.read_form()?),
                )),
                _ => Ok(MalType::Symbol(c.to_string())),
            },
            Some(Token::TildeAt) => Ok(MalType::SpliceUnquote(Rc::new(self.read_form()?))),
            Some(_) => Err(MalErr::NotImplemented),
            None => Err(MalErr::EndOfFile),
        }
    }
}

pub fn read_str(input: &str) -> MalRes {
    let maybe_reader = tokenize(input);
    if let Ok(mut reader) = maybe_reader {
        reader.read_form()
    } else {
        Err(MalErr::UnmatchedDoubleQuote)
    }
}

pub fn tokenize(input: &str) -> Result<Reader, MalErr> {
    let mut tokens: Vec<Token> = vec![];
    let captures = TOKEN_RE.captures_iter(input);
    lazy_static! {
        static ref STR_RE: Regex = Regex::new(r#""(?:\\.|[^\\"])*""#).unwrap();
    }
    for m in captures {
        if m.name("tat").is_some() {
            tokens.push(Token::TildeAt);
        }
        if let Some(special) = m.name("spe") {
            tokens.push(Token::Special(special.as_str().chars().next().unwrap()));
        }
        if let Some(string) = m.name("str") {
            if string.as_str().len() > 1 {
                if STR_RE.is_match(string.as_str()) {
                    tokens.push(Token::String(
                        unescape(&string.as_str()[1..string.as_str().len() - 1]).to_string(),
                    ));
                } else {
                    return Err(MalErr::UnmatchedDoubleQuote);
                }
            } else {
                return Err(MalErr::UnmatchedDoubleQuote);
            }
        }
        if let Some(semi_start) = m.name("sem") {
            tokens.push(Token::Semi(semi_start.as_str().to_string()));
        }
        if let Some(chars) = m.name("cha") {
            tokens.push(Token::Chars(chars.as_str().to_string()));
        }
    }

    Ok(Reader::new(tokens))
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Token {
    TildeAt,
    Special(char),
    String(String),
    Semi(String),
    Chars(String),
}
