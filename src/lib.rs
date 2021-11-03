use std::collections::HashMap;
use std::fmt::write;
use std::io;

pub mod engine;
pub mod parser;
pub mod util;

#[derive(PartialEq, Clone)]
pub struct Fqid {
    pub body: Vec<String>,
}

impl Fqid {
    pub fn new(fqid: &[&str]) -> Self {
        Self {
            body: fqid.iter().map(|s| s.to_string()).collect::<Vec<_>>(),
        }
    }
}

impl std::fmt::Debug for Fqid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write(f, format_args!("Fqid({})", self.body.join(".")))
    }
}

#[derive(Debug, PartialEq)]
pub enum Exp {
    Var(Fqid),
    Call(Fqid, Vec<Exp>),
    Str(String),
    Int(i64),
    Float(f64),
    Template(Template),
    Array(Vec<Exp>),
    Symbol(String),
    Undefined,
}

impl std::string::ToString for Exp {
    fn to_string(&self) -> String {
        match self {
            Self::Var(fqid) => fqid.body.join("."),
            Self::Call(fqid, args) => format!(
                "{}({})",
                fqid.body.join("."),
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
            Self::Symbol(s) => format!(":{}", s),
            Self::Int(i) => i.to_string(),
            Self::Float(f) => f.to_string(),
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
type TemplateRef<'a> = &'a [TemplateElem];

#[derive(Debug, PartialEq)]
enum Input {
    #[allow(unused)]
    Single(Exp),
    #[allow(unused)]
    Obj(HashMap<String, Exp>),
}

#[derive(Debug, PartialEq)]

enum Task {
    Produce {
        input: Input,
        out: Exp,
        command: Exp,
    },
    Execute {
        input: Input,
        command: Exp,
    },
    Phony {
        input: Input,
    },
}

#[derive(Debug, PartialEq)]
enum Rule {
    Map { source: Exp, task: Task },
    Single(Task),
}

#[derive(Debug, PartialEq)]
pub struct Config {
    rules: Vec<(String, Rule)>,
}

#[derive(Debug)]
pub enum Error {
    ConfigLoadError(io::Error),
    ConfigScanError(yaml_rust::ScanError),
    ConfigError(String),
    SyntaxError(pest::error::LineColLocation),
    RuntimeError(String),
}
