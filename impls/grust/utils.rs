use lazy_static::lazy_static;
use regex::{Captures, Regex};

pub fn escape(input: String) -> String {
    input
        .chars()
        .map(|c| match c {
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\\' => "\\\\".to_string(),
            _ => c.to_string(),
        })
        .collect::<Vec<String>>()
        .join("")
}

pub fn unescape(s: &str) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r#"\\(.)"#).unwrap();
    }
    RE.replace_all(&s, |caps: &Captures| {
        (if &caps[1] == "n" { "\n" } else { &caps[1] }).to_string()
    })
    .to_string()
}
