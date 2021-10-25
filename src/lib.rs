use std::collections::HashMap;
use std::io;

pub mod parser;

type Fqid = Vec<String>;

#[derive(Debug, PartialEq)]
pub enum Exp {
    Var(Fqid),
    Call(Fqid, Vec<Exp>),
    Str(String),
    Template(Template),
    Array(Vec<Exp>),
    Undefined,
}

impl std::string::ToString for Exp {
    fn to_string(&self) -> String {
        match self {
            Self::Var(fqid) => fqid.join("."),
            Self::Call(fqid, args) => format!(
                "{}({})",
                fqid.join("."),
                args.iter()
                    .map(|exp| exp.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Str(s) => s.replace("\\", "\\\\").replace("\"", "\\\""),
            Self::Template(template) => template
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(""),
            Self::Array(arr) => format!(
                "[{}]",
                arr.iter()
                    .map(|exp| exp.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Undefined => "<undefined>".to_owned(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TemplateElem {
    #[allow(unused)]
    Text(String),
    #[allow(unused)]
    Exp(Exp),
}

impl std::string::ToString for TemplateElem {
    fn to_string(&self) -> String {
        match self {
            Self::Text(txt) => txt.to_owned(),
            Self::Exp(exp) => format!("{{{{ {} }}}}", exp.to_string()),
        }
    }
}

type Template = Vec<TemplateElem>;

#[derive(Debug, PartialEq)]
enum Input {
    #[allow(unused)]
    Single(Exp),
    #[allow(unused)]
    Obj(HashMap<String, Exp>),
}

#[derive(Debug, PartialEq)]
struct Task {
    input: Input,
    out: Exp,
    command: Exp,
}

#[derive(Debug, PartialEq)]
struct Rule {
    source: Exp,
    task: Task,
}

#[derive(Debug, PartialEq)]
pub struct Config {
    rules: HashMap<String, Rule>,
}

#[derive(Debug)]
pub enum Error {
    ConfigLoadError(io::Error),
    ConfigScanError(serde_yaml::Error),
    SyntaxError(pest::error::LineColLocation),
}
