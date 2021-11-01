use std::collections::{HashMap, HashSet};

use maplit::{hashmap, hashset};
use unzip3::Unzip3;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum FunId {
    Glob,
    Ext,
    Join,
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
    Function(Function, Vec<Val>),
    Map(HashMap<String, Val>),
}

impl<'a> Val {
    fn as_list(&'a self) -> Result<&'a Vec<Val>, Error> {
        if let Val::List(l) = self {
            Ok(l)
        } else {
            Err(Error::MismatchedType {
                expected: Type::List(Box::new(Type::Any)),
                real: self.type_of(),
            })
        }
    }

    fn as_str(&'a self) -> Result<&'a str, Error> {
        if let Val::String(s) = self {
            Ok(s)
        } else {
            Err(Error::MismatchedType {
                expected: Type::String,
                real: self.type_of(),
            })
        }
    }

    fn as_map(&'a self) -> Result<&'a HashMap<String, Val>, Error> {
        if let Val::Map(map) = self {
            Ok(map)
        } else {
            Err(Error::MismatchedType {
                expected: Type::Map,
                real: self.type_of(),
            })
        }
    }

    fn as_map_mut(&'a mut self) -> Option<&'a mut HashMap<String, Val>> {
        if let Val::Map(map) = self {
            Some(map)
        } else {
            None
        }
    }

    fn as_fun(&'a self) -> Option<(&'a Function, &'a Vec<Val>)> {
        if let Val::Function(f, args) = self {
            Some((f, args))
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
        "join".to_owned() => Val::Function(
            Function {
                id: FunId::Join,
                argc: 2
            },
            vec![]
        )
    })
}

impl Env {
    pub fn new_with_std() -> Self {
        Self {
            vars: hashmap! {
                "std".to_string() => generate_std(),
                "tasks".to_string() => Val::Map(HashMap::new()),
            },
        }
    }

    fn unregister_self(&mut self) {
        self.vars.remove("self");
    }
}

pub type Output = HashSet<String>;
pub type Input = HashSet<String>;

#[derive(Debug, PartialEq, Clone)]
pub struct Task {
    pub name: String,
    pub input: Input,
    pub output: Output,
    pub command: Option<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Any,
    Bool,
    Float,
    Int,
    List(Box<Type>),
    String,
    Map,
    Function,
    Or(Vec<Type>),
}

impl Val {
    fn type_of(&self) -> Type {
        match self {
            Val::Bool(_) => Type::Bool,
            Val::Float(_) => Type::Float,
            Val::Int(_) => Type::Int,
            Val::Function(_, _) => Type::Function,
            // TODO: detect correct type
            Val::List(_) => Type::List(Box::new(Type::Any)),
            Val::Map(_) => Type::Map,
            Val::String(_) => Type::String,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    SourceMustBeStringList,
    CommandMustBeString,
    OutputMustBeStringOrStringList,
    InputMustBeStringHashOrString,
    UndefinedReferencing,
    NotFunction,
    EmptyIdentifier,
    TemplateElemMustBeString,
    UndefinedVariable(crate::Fqid),
    MismatchedType { expected: Type, real: Type },
    TooManyArgument(FunId),
    CannotTakeGlob,
}

fn take_val<'a>(env: &'a Env, fqid: &crate::Fqid) -> Result<&'a Val, Error> {
    let mut id = fqid.body.iter();
    let global_prefix = id.next().ok_or(Error::EmptyIdentifier)?;
    id.fold(
        env.vars
            .get(global_prefix)
            .ok_or_else(|| Error::UndefinedVariable(fqid.clone())),
        |acc, id| match acc {
            Ok(Val::Map(hash)) => hash
                .get(id)
                .ok_or_else(|| Error::UndefinedVariable(fqid.clone())),
            Ok(v) => Err(Error::MismatchedType {
                expected: Type::Map,
                real: v.type_of(),
            }),
            Err(e) => Err(e),
        },
    )
}

fn eval_template(env: &Env, template: crate::TemplateRef) -> Result<String, Error> {
    Ok(template
        .iter()
        .map(|e| match e {
            crate::TemplateElem::Text(s) => Ok(s.to_owned()),
            crate::TemplateElem::Exp(e) => eval_exp(env, e)?
                .as_str()
                .map(|s| s.to_owned())
                .map_err(|_| Error::TemplateElemMustBeString),
        })
        .collect::<Result<Vec<_>, _>>()?
        .join(""))
}

fn change_ext(from: &str, to: &str, file: &str) -> Val {
    if file.ends_with(from) {
        Val::String(format!("{}{}", file.trim_end_matches(from), to))
    } else {
        Val::String(file.to_owned())
    }
}

fn apply_ext(args: &[Val]) -> Result<Val, Error> {
    let ext_from = &args[0].as_str()?;
    let ext_to = &args[1].as_str()?;
    match &args[2] {
        Val::List(l) => l
            .iter()
            .map(|s| s.as_str().map(|file| change_ext(ext_from, ext_to, file)))
            .collect::<Result<Vec<_>, _>>()
            .map(Val::List),
        Val::String(file) => Ok(change_ext(ext_from, ext_to, file)),
        v => Err(Error::MismatchedType {
            expected: Type::Or(vec![Type::List(Box::new(Type::String)), Type::String]),
            real: v.type_of(),
        }),
    }
}

fn apply_glob(args: &[Val]) -> Result<Val, Error> {
    let files = glob::glob(args[0].as_str()?).map_err(|_| Error::CannotTakeGlob)?;
    // TODO: to_string_lossy -> to_str
    files
        .map(|path| {
            path.map(|p| Val::String(p.to_string_lossy().into_owned()))
                .map_err(|_| Error::CannotTakeGlob)
        })
        .collect::<Result<Vec<_>, _>>()
        .map(Val::List)
}

fn apply_join(args: &[Val]) -> Result<Val, Error> {
    let input = args[0]
        .as_list()?
        .iter()
        .map(|s| s.as_str())
        .collect::<Result<Vec<_>, _>>()?;
    let separator = args[1].as_str()?;
    Ok(Val::String(input.join(separator)))
}

fn apply_function(_: &Env, f: &Function, args: &[Val]) -> Result<Val, Error> {
    use std::cmp::Ordering;
    match f.argc.cmp(&args.len()) {
        Ordering::Equal => match f.id {
            FunId::Ext => apply_ext(args),
            FunId::Glob => apply_glob(args),
            FunId::Join => apply_join(args),
        },
        Ordering::Greater => Ok(Val::Function(f.clone(), args.to_vec())),
        Ordering::Less => Err(Error::TooManyArgument(f.id)),
    }
}

fn eval_exp(env: &Env, exp: &crate::Exp) -> Result<Val, Error> {
    use crate::Exp;
    match exp {
        Exp::Array(arr) => arr
            .iter()
            .map(|exp| eval_exp(env, exp))
            .collect::<Result<Vec<_>, _>>()
            .map(Val::List),
        Exp::Str(s) => Ok(Val::String(s.to_owned())),
        Exp::Var(fqid) => take_val(env, fqid).map(|v| v.clone()),
        Exp::Undefined => Err(Error::UndefinedReferencing),
        Exp::Template(template) => eval_template(env, template).map(Val::String),
        Exp::Call(fqid, passed_args) => {
            let fun = take_val(env, fqid)?;
            let (f, applied_args) = fun.as_fun().ok_or(Error::NotFunction)?;
            let mut passed_args = passed_args
                .iter()
                .map(|exp| eval_exp(env, exp))
                .collect::<Result<Vec<_>, _>>()?;
            let mut args = applied_args.clone();
            args.append(&mut passed_args);
            apply_function(env, f, args.as_slice())
        }
    }
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
                    let input = eval_exp(env, val)?.as_str()?.to_owned();
                    Ok((key.to_owned(), input))
                })
                .collect::<Result<HashMap<_, _>, _>>()?,
        ),
        crate::Input::Single(s) => match eval_exp(env, s)? {
            Val::String(s) => EvaluatedInput::Single(s),
            Val::List(l) => EvaluatedInput::List(
                l.into_iter()
                    .map(|v| v.as_str().map(|s| s.to_owned()))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Val::Map(hash) => EvaluatedInput::Hash(
                hash.into_iter()
                    .map(|(k, v)| v.as_str().map(|s| (k, s.to_owned())))
                    .collect::<Result<HashMap<_, _>, _>>()?,
            ),
            _ => return Err(Error::InputMustBeStringHashOrString),
        },
    };
    Ok(input)
}

type TaskSource = (Input, Output, Option<String>);

fn eval_execute_task(
    env: &mut Env,
    input: &crate::Input,
    command: &crate::Exp,
) -> Result<(TaskSource, Val), Error> {
    let input = eval_input(env, input)?;
    let mut task_vars = Val::Map(hashmap! {
        "in".to_owned() => input.clone().convert_to_val(),
    });
    env.vars.insert("self".to_owned(), task_vars.clone());
    let command = eval_exp(env, command)?.as_str()?.to_owned();
    task_vars
        .as_map_mut()
        .unwrap()
        .insert("command".to_owned(), Val::String(command.clone()));
    Ok((
        (input.convert_to_hashset(), HashSet::new(), Some(command)),
        task_vars,
    ))
}

fn eval_phony_task(
    env: &mut Env,
    input: &crate::Input,
) -> Result<(TaskSource, Val), Error> {
    let input = eval_input(env, input)?;
    let task_vars = Val::Map(hashmap! {
        "in".to_owned() => input.clone().convert_to_val(),
    });
    env.vars.insert("self".to_owned(), task_vars.clone());
    Ok((
        (input.convert_to_hashset(), HashSet::new(), None),
        task_vars,
    ))
}

fn eval_produce_task(
    env: &mut Env,
    input: &crate::Input,
    out: &crate::Exp,
    command: &crate::Exp,
) -> Result<(TaskSource, Val), Error> {
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
            .map(|s| s.as_str().map(|s| s.to_owned()))
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
    let mut task_vars = Val::Map(hashmap! {
        "in".to_owned() => input.clone().convert_to_val(),
        "out".to_owned() => output_val,
    });
    env.vars.insert("self".to_owned(), task_vars.clone());
    let command = eval_exp(env, command)?.as_str()?.to_owned();
    task_vars
        .as_map_mut()
        .unwrap()
        .insert("command".to_owned(), Val::String(command.clone()));
    Ok((
        (input.convert_to_hashset(), output, Some(command)),
        task_vars,
    ))
}

fn eval_task(
    env: &mut Env,
    task: &crate::Task,
) -> Result<(TaskSource, Val), Error> {
    match task {
        crate::Task::Execute { input, command } => eval_execute_task(env, input, command),
        crate::Task::Phony { input } => eval_phony_task(env, input),
        crate::Task::Produce {
            input,
            out,
            command,
        } => eval_produce_task(env, input, out, command),
    }
}

fn insert_emmited_task_variable(env: &mut Env, name: &str, emmited: Val) {
    env.vars
        .get_mut("tasks")
        .unwrap()
        .as_map_mut()
        .map(|map| map.insert(name.to_owned(), emmited))
        .unwrap();
}

fn flatten_as_string_list(val: Vec<Val>) -> Result<Vec<Val>, Error> {
    Ok(val
        .into_iter()
        .map(|v| match v {
            Val::List(l) => l
                .into_iter()
                .map(|s| s.as_str().map(|s| Val::String(s.to_owned())))
                .collect::<Result<Vec<_>, _>>(),
            Val::String(s) => Ok(vec![Val::String(s)]),
            _ => Err(Error::MismatchedType {
                expected: Type::Or(vec![Type::List(Box::new(Type::String)), Type::String]),
                real: v.type_of(),
            }),
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<_>>())
}

pub fn eval(env: &mut Env, config: &crate::Config) -> Result<Vec<Task>, Error> {
    let tasks = config
        .rules
        .iter()
        .map(|(name, rule)| match rule {
            crate::Rule::Single(task) => {
                let ((input, output, command), emmited) = eval_task(env, task)?;
                env.unregister_self();
                insert_emmited_task_variable(env, name, emmited);
                Ok(vec![Task {
                    name: name.to_owned(),
                    input,
                    output,
                    command,
                }])
            }
            crate::Rule::Map { source, task } => {
                let sources = eval_exp(env, source)?;
                let sources = sources.as_list()?;
                if !sources.iter().all(|val| matches!(val, Val::String(_))) {
                    return Err(Error::SourceMustBeStringList);
                }
                let (tasks, emmited) = sources
                    .iter()
                    .map(|source| {
                        env.vars.insert("source".to_owned(), source.clone());
                        let (task, vars) = eval_task(env, task)?;
                        env.unregister_self();
                        Ok((task, vars))
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .unzip::<_, _, Vec<_>, Vec<_>>();
                let emmited = match task {
                    crate::Task::Produce {
                        input: _,
                        out: _,
                        command: _,
                    } => {
                        let (input, out, command) = emmited
                            .into_iter()
                            .map(|emmited| {
                                let hash = emmited.as_map().unwrap();
                                (
                                    hash.get("in").unwrap().clone(),
                                    hash.get("out").unwrap().clone(),
                                    hash.get("command").unwrap().clone(),
                                )
                            })
                            .unzip3::<Vec<_>, Vec<_>, Vec<_>>();
                        hashmap! {
                            "in".to_owned() => Val::List(flatten_as_string_list(input)?),
                            "out".to_owned() => Val::List(flatten_as_string_list(out)?),
                            "command".to_owned() => Val::List(command)
                        }
                    }
                    crate::Task::Phony { input: _ } => {
                        let input = emmited
                            .into_iter()
                            .map(|emmited| emmited.as_map().unwrap().get("in").unwrap().clone())
                            .collect::<Vec<_>>();
                        hashmap! {
                            "in".to_owned() => Val::List(flatten_as_string_list(input)?),
                        }
                    }
                    crate::Task::Execute {
                        input: _,
                        command: _,
                    } => {
                        let (input, command) = emmited
                            .into_iter()
                            .map(|emmited| {
                                let hash = emmited.as_map().unwrap();
                                (
                                    hash.get("in").unwrap().clone(),
                                    hash.get("command").unwrap().clone(),
                                )
                            })
                            .unzip::<_, _, Vec<_>, Vec<_>>();
                        hashmap! {
                            "in".to_owned() => Val::List(flatten_as_string_list(input)?),
                            "command".to_owned() => Val::List(command)
                        }
                    }
                };
                insert_emmited_task_variable(env, name.as_str(), Val::Map(emmited));
                Ok(tasks
                    .into_iter()
                    .map(|(input, output, command)| Task {
                        name: name.to_owned(),
                        input,
                        output,
                        command,
                    })
                    .collect::<Vec<_>>())
            }
        })
        .collect::<Result<Vec<_>, _>>();
    Ok(tasks?.into_iter().flatten().collect::<Vec<_>>())
}
