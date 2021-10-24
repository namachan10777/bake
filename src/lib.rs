use std::collections::HashMap;
use std::io;

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
struct Task {
    deps: HashMap<String, Expand>,
    command: Template,
}

#[derive(Debug)]
pub struct Config {
    tasks: Vec<Task>,
}

pub enum Error {
    ConfigLoadError(io::Error),
    ConfigScanError(serde_yaml::Error),
    SyntaxError(pest::error::LineColLocation),
}
