use std::collections::HashMap;
use std::io;
use yaml_rust::Yaml;

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

fn inside_out<T, E>(x: Option<Result<T, E>>) -> Result<Option<T>, E> {
    match x {
        Some(Ok(x)) => Ok(Some(x)),
        Some(Err(e)) => Err(e),
        None => Ok(None),
    }
}

impl Task {
    fn from_yaml(ctx: &str, yaml: &Yaml) -> Result<Self, Error> {
        let hash = yaml
            .as_hash()
            .ok_or_else(|| Error::ConfigError(format!("rules.{} must be hash", ctx)))?;
        check_keys(hash, &["in", "out", "command"])?;
        let input = match hash
            .get(&Yaml::String("in".to_owned()))
            .ok_or_else(|| Error::ConfigError(format!("rules.{}.in is required", ctx)))?
        {
            Yaml::String(s) => Input::Single(
                parser::parse(s.as_str())
                    .map_err(|_| Error::ConfigError(format!("Syntax error on rules.{}.in", ctx)))?,
            ),
            _ => return Err(Error::ConfigError("rules.*.in must be string".to_owned())),
        };
        let command = hash.get(&Yaml::String("command".to_owned())).map(|y| {
            y.as_str()
                .ok_or_else(|| Error::ConfigError(format!("rules.{}.command must be string", ctx)))
        });
        let command = inside_out(command)?;
        let command = command.map(|command| parser::parse(command));
        let command = inside_out(command)
            .map_err(|_| Error::ConfigError(format!("Syntax error on rules.{}.command", ctx)))?;
        let out =
            match hash.get(&Yaml::String("out".to_owned())) {
                Some(Yaml::String(s)) => Some(parser::parse(s.as_str()).map_err(|_| {
                    Error::ConfigError(format!("Syntax error on rules.{}.out", ctx))
                })?),
                None => None,
                _ => return Err(Error::ConfigError("rules.*.out must be string".to_owned())),
            };
        match (out, command) {
            (Some(out), Some(command)) => Ok(Self::Produce {
                input,
                out,
                command,
            }),
            (None, Some(command)) => Ok(Self::Execute { input, command }),
            (None, None) => Ok(Self::Phony { input }),
            (Some(_), None) => Err(Error::ConfigError(format!(
                "rule.{}.task: out requires command",
                ctx
            ))),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Rule {
    Map { source: Exp, task: Task },
    Single(Task),
}

fn check_keys<'a>(
    hash: &'a linked_hash_map::LinkedHashMap<Yaml, Yaml>,
    allowed: &[&str],
) -> Result<(), Error> {
    let invalid_keys = hash
        .keys()
        .into_iter()
        .flat_map(|k| match k {
            Yaml::String(s) => {
                if allowed.contains(&s.as_str()) {
                    None
                } else {
                    Some(k)
                }
            }
            yaml => Some(yaml),
        })
        .map(|yaml| {
            let mut buf = String::new();
            yaml_rust::YamlEmitter::new(&mut buf).dump(yaml).unwrap();
            buf
        })
        .collect::<Vec<_>>();
    if !invalid_keys.is_empty() {
        Err(Error::ConfigError(format!(
            "invalid keys {}",
            invalid_keys.join(", ")
        )))
    } else {
        Ok(())
    }
}

impl Rule {
    fn from_yaml(ctx: &str, yaml: &Yaml) -> Result<Self, Error> {
        let hash = yaml
            .as_hash()
            .ok_or_else(|| Error::ConfigError("rules.* must be hash".to_owned()))?;
        if hash.contains_key(&Yaml::String("source".to_owned())) {
            check_keys(hash, &["source", "task"])?;
            let source = hash
                .get(&Yaml::String("source".to_owned()))
                .unwrap()
                .as_str()
                .ok_or_else(|| {
                    Error::ConfigError("rules.*.source must be parsed as a yaml string".to_owned())
                })?;
            let task = hash
                .get(&Yaml::String("task".to_owned()))
                .ok_or_else(|| Error::ConfigError(format!("rules.{}.task is required", ctx)))?;
            let source = parser::parse(source)
                .map_err(|_| Error::ConfigError("Cannot parse rules.*.source".to_owned()))?;
            let task = Task::from_yaml(&format!("{}.task", ctx), task)?;
            Ok(Self::Map { source, task })
        } else {
            let task = Task::from_yaml(ctx, yaml)?;
            Ok(Self::Single(task))
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Config {
    rules: HashMap<String, Rule>,
}

impl Config {
    pub fn from_str_src(src: &str) -> Result<Self, Error> {
        if let [yaml] = yaml_rust::YamlLoader::load_from_str(src)
            .map_err(Error::ConfigScanError)?
            .as_slice()
        {
            let rules = yaml
                .as_hash()
                .ok_or_else(|| Error::ConfigError("root object must be hash".to_owned()))?
                .get(&Yaml::String("rules".to_owned()))
                .ok_or_else(|| Error::ConfigError("rules is required".to_owned()))?
                .as_hash()
                .ok_or_else(|| Error::ConfigError("rules must be hash".to_owned()))?;
            Ok(Self {
                rules: rules
                    .into_iter()
                    .map(|(key, yaml)| {
                        let key = key
                            .as_str()
                            .ok_or_else(|| {
                                Error::ConfigError("key of rules.* must be string".to_owned())
                            })?
                            .to_owned();
                        let val = Rule::from_yaml(&key, yaml)?;
                        Ok((key, val))
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?,
            })
        } else {
            Err(Error::ConfigError(
                "config must include single yaml object".to_owned(),
            ))
        }
    }
}

#[derive(Debug)]
pub enum Error {
    ConfigLoadError(io::Error),
    ConfigScanError(yaml_rust::ScanError),
    ConfigError(String),
    SyntaxError(pest::error::LineColLocation),
    RuntimeError(String),
}
