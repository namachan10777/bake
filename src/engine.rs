use std::collections::{HashMap, HashSet};

use crate::Fqid;
use maplit::{hashmap, hashset};

#[derive(PartialEq, Debug, Clone)]
pub enum FunId {
    Glob,
    Ext,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    id: FunId,
    argc: usize,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    List(Vec<Val>),
    Var(Fqid),
    Function(Function, Vec<Val>),
    Map(HashMap<String, Val>),
}

impl<'a> Val {
    fn as_list(&'a self) -> Option<&'a Vec<Val>> {
        if let Val::List(l) = self {
            Some(l)
        } else {
            None
        }
    }

    fn as_str(&'a self) -> Option<&'a str> {
        if let Val::String(s) = self {
            Some(s)
        } else {
            None
        }
    }

    fn as_map_mut(&'a mut self) -> Option<&'a mut HashMap<String, Val>> {
        if let Val::Map(map) = self {
            Some(map)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    pub vars: HashMap<String, Val>,
}

fn generate_std() -> Val {
    Val::Map(hashmap! {
        "glob".to_owned() => Val::Function(
            Function {
                id: FunId::Glob,
                argc: 1,
            },
            vec![]
        ),
        "ext".to_owned() => Val::Function(
            Function {
                id: FunId::Ext,
                argc: 3,
            },
            vec![]
        ),
    })
}

impl Env {
    pub fn new_with_std() -> Self {
        Self {
            vars: hashmap! {
                "std".to_string() => generate_std(),
            },
        }
    }

    fn unregister_self(&mut self) {
        self.vars.remove("self");
    }
}

pub type Output = HashSet<String>;
pub type Input = HashSet<String>;
pub struct Task {
    pub input: Input,
    pub output: Output,
    pub command: Option<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    SourceMustBeStringList,
    CommandMustBeString,
    OutputMustBeStringOrStringList,
    InputMustBeStringHashOrString,
}

fn eval_exp(_env: &mut Env, _task: &crate::Exp) -> Result<Val, Error> {
    unimplemented!()
}

#[derive(Debug, Clone, PartialEq)]
enum EvaluatedInput {
    Single(String),
    Hash(HashMap<String, String>),
    List(Vec<String>),
}

impl EvaluatedInput {
    fn convert_to_hashset(self) -> HashSet<String> {
        match self {
            Self::Single(s) => hashset! {s},
            Self::Hash(hash) => hash
                .into_iter()
                .map(|(_, input)| input)
                .collect::<HashSet<_>>(),
            Self::List(l) => l.into_iter().collect::<HashSet<_>>(),
        }
    }

    fn convert_to_val(self) -> Val {
        match self {
            Self::Single(s) => Val::String(s),
            Self::Hash(hash) => Val::Map(
                hash.into_iter()
                    .map(|(k, input)| (k, Val::String(input)))
                    .collect::<HashMap<_, _>>(),
            ),
            Self::List(l) => Val::List(l.into_iter().map(Val::String).collect::<Vec<_>>()),
        }
    }
}

fn eval_input(env: &mut Env, input: &crate::Input) -> Result<EvaluatedInput, Error> {
    let input = match input {
        crate::Input::Obj(hash) => EvaluatedInput::Hash(
            hash.iter()
                .map(|(key, val)| {
                    let input = eval_exp(env, val)?
                        .as_str()
                        .ok_or(Error::InputMustBeStringHashOrString)?
                        .to_owned();
                    Ok((key.to_owned(), input))
                })
                .collect::<Result<HashMap<_, _>, _>>()?,
        ),
        crate::Input::Single(s) => match eval_exp(env, s)? {
            Val::String(s) => EvaluatedInput::Single(s),
            Val::List(l) => EvaluatedInput::List(
                l.into_iter()
                    .map(|v| {
                        v.as_str()
                            .ok_or(Error::InputMustBeStringHashOrString)
                            .map(|s| s.to_owned())
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Val::Map(hash) => EvaluatedInput::Hash(
                hash.into_iter()
                    .map(|(k, v)| {
                        v.as_str()
                            .ok_or(Error::InputMustBeStringHashOrString)
                            .map(|s| (k, s.to_owned()))
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?,
            ),
            _ => return Err(Error::InputMustBeStringHashOrString),
        },
    };
    Ok(input)
}

fn eval_task(env: &mut Env, task: &crate::Task) -> Result<(Task, Val), Error> {
    match task {
        crate::Task::Execute { input, command } => {
            let input = eval_input(env, input)?;
            let task_vars = Val::Map(hashmap! {
                "input".to_owned() => input.clone().convert_to_val(),
            });
            env.vars.insert("self".to_owned(), task_vars.clone());
            let command = eval_exp(env, command)?
                .as_str()
                .ok_or(Error::CommandMustBeString)?
                .to_owned();
            Ok((
                Task {
                    input: input.convert_to_hashset(),
                    command: Some(command),
                    output: HashSet::new(),
                },
                task_vars,
            ))
        }
        crate::Task::Phony { input } => {
            let input = eval_input(env, input)?;
            let task_vars = Val::Map(hashmap! {
                "input".to_owned() => input.clone().convert_to_val(),
            });
            env.vars.insert("self".to_owned(), task_vars.clone());
            Ok((
                Task {
                    input: input.convert_to_hashset(),
                    command: None,
                    output: HashSet::new(),
                },
                task_vars,
            ))
        }
        crate::Task::Produce {
            input,
            out,
            command,
        } => {
            let input = eval_input(env, input)?;
            env.vars.insert(
                "self".to_owned(),
                Val::Map(hashmap! {
                    "input".to_owned() => input.clone().convert_to_val(),
                }),
            );
            let output = match eval_exp(env, out)? {
                Val::List(l) => l
                    .into_iter()
                    .map(|s| {
                        s.as_str()
                            .map(|s| s.to_owned())
                            .ok_or(Error::OutputMustBeStringOrStringList)
                    })
                    .collect::<Result<HashSet<_>, _>>()?,
                Val::String(s) => hashset![s],
                _ => return Err(Error::OutputMustBeStringOrStringList),
            };
            let output_val = Val::List(
                output
                    .iter()
                    .map(|s| Val::String(s.to_owned()))
                    .collect::<Vec<_>>(),
            );
            let task_vars = Val::Map(hashmap! {
                "input".to_owned() => input.clone().convert_to_val(),
                "output".to_owned() => output_val,
            });
            env.vars.insert("self".to_owned(), task_vars.clone());
            let command = eval_exp(env, command)?
                .as_str()
                .ok_or(Error::CommandMustBeString)?
                .to_owned();
            Ok((
                Task {
                    input: input.convert_to_hashset(),
                    command: Some(command),
                    output,
                },
                task_vars,
            ))
        }
    }
}

pub fn eval(env: &mut Env, config: &crate::Config) -> Result<Vec<Task>, Error> {
    let tasks = config
        .rules
        .iter()
        .map(|(name, rule)| match rule {
            crate::Rule::Single(task) => {
                let (task, vars) = eval_task(env, task)?;
                env.unregister_self();
                env.vars
                    .get_mut("tasks")
                    .unwrap()
                    .as_map_mut()
                    .map(|map| map.insert(name.to_owned(), vars))
                    .unwrap();
                Ok(vec![task])
            }
            crate::Rule::Map { source, task } => {
                let sources = eval_exp(env, source)?;
                let sources = sources.as_list().ok_or(Error::SourceMustBeStringList)?;
                if !sources.iter().all(|val| matches!(val, Val::String(_))) {
                    return Err(Error::SourceMustBeStringList);
                }
                sources
                    .iter()
                    .map(|source| {
                        let mut env = env.clone();
                        env.vars.insert("source".to_owned(), source.clone());
                        let (task, vars) = eval_task(&mut env, task)?;
                        env.unregister_self();
                        env.vars
                            .get_mut("tasks")
                            .unwrap()
                            .as_map_mut()
                            .map(|map| map.insert(name.to_owned(), vars))
                            .unwrap();
                        Ok(task)
                    })
                    .collect::<Result<Vec<_>, _>>()
            }
        })
        .collect::<Result<Vec<_>, _>>();
    Ok(tasks?.into_iter().flatten().collect::<Vec<_>>())
}
