use std::io;
use std::collections::HashMap;

pub mod parser;

type Fqid = Vec<String>;

#[derive(Debug, PartialEq)]
enum Exp {
    Var(Fqid),
    Call(Fqid, Vec<Exp>),
    Str(String),
}

#[derive(Debug, PartialEq)]
enum TemplateElem {
    #[allow(unused)]
    Text(String),
    #[allow(unused)]
    Exp(Exp),
}

type Template = Vec<TemplateElem>;

#[derive(Debug, PartialEq)]
enum Expand {
    #[allow(unused)]
    Array(Exp),
    #[allow(unused)]
    String(Exp),
}

#[derive(Debug)]
struct Rule {
    in_single: HashMap<String, Option<Exp>>,
    in_multi: HashMap<String, Vec<Exp>>,
    in_match: regex::Regex,
    out: Vec<Exp>,
    command: Template,
}

#[derive(Debug)]
pub struct Config {
    rules: Vec<Rule>,
}

pub enum Error {
    ConfigLoadError(io::Error),
    ConfigScanError(serde_yaml::Error),
    SyntaxError(pest::error::LineColLocation),
}
