use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use yaml_rust::Yaml;

use crate::Exp;

#[derive(Parser)]
#[grammar = "parser.pest"]
struct BakeParser;

#[derive(Debug)]
pub enum Error {}

fn parse_fqid(pair: Pair<Rule>) -> Result<crate::Fqid, Error> {
    match pair.as_rule() {
        Rule::fqid => Ok(crate::Fqid {
            body: pair
                .into_inner()
                .map(|p| p.as_str().to_owned())
                .collect::<Vec<_>>(),
        }),
        _ => unreachable!(),
    }
}

fn parse_exp(pair: Pair<Rule>) -> Result<crate::Exp, Error> {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::function => {
            let mut inner = pair.into_inner();
            let fid = parse_fqid(inner.next().unwrap())?;
            let args = inner.map(parse_exp).collect::<Result<Vec<_>, _>>()?;
            Ok(crate::Exp::Call(fid, args))
        }
        Rule::variable => Ok(Exp::Var(parse_fqid(pair.into_inner().next().unwrap())?)),
        Rule::string => {
            let s = pair
                .into_inner()
                .map(|p| match p.as_rule() {
                    Rule::string_escape => &p.as_str()[1..],
                    Rule::string_elem => p.as_str(),
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>()
                .join("");
            Ok(Exp::Str(s))
        }
        Rule::symbol => Ok(Exp::Symbol(pair.as_str()[1..].to_owned())),
        Rule::int => Ok(Exp::Int(pair.as_str().parse().unwrap())),
        _ => unreachable!(),
    }
}

pub fn parse_exp_from_str(src: &str) -> Result<crate::Exp, crate::Error> {
    let pair = BakeParser::parse(Rule::exp, src)
        .map_err(|e| crate::Error::SyntaxError(e.line_col))?
        .next()
        .unwrap();
    Ok(parse_exp(pair).unwrap())
}

fn parse_template(pair: Pair<Rule>) -> Result<crate::Template, Error> {
    match pair.as_rule() {
        Rule::template => Ok(pair
            .into_inner()
            .flat_map(|p| match p.as_rule() {
                Rule::EOI => None,
                Rule::text => Some(Ok(crate::TemplateElem::Text(
                    p.into_inner()
                        .map(|p| match p.as_rule() {
                            Rule::text_elem => p.as_str(),
                            Rule::text_escape => &p.as_str()[1..],
                            _ => unreachable!(),
                        })
                        .collect::<Vec<_>>()
                        .join(""),
                ))),
                Rule::subst => {
                    Some(parse_exp(p.into_inner().next().unwrap()).map(crate::TemplateElem::Exp))
                }
                _ => unreachable!(),
            })
            .collect::<Result<Vec<_>, _>>()?),
        _ => unreachable!(),
    }
}

pub fn parse(src: &str) -> Result<crate::Exp, crate::Error> {
    let pair = BakeParser::parse(Rule::syntax, src)
        .map_err(|e| crate::Error::SyntaxError(e.line_col))?
        .next()
        .unwrap();
    match pair.as_rule() {
        Rule::syntax => {
            let pair = pair.into_inner().next().unwrap();
            match pair.as_rule() {
                Rule::template => Ok(crate::Exp::Template(parse_template(pair).unwrap())),
                Rule::expand => Ok(parse_exp(pair.into_inner().next().unwrap()).unwrap()),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test_parse_rules {
    use super::*;
    use pest::Parser;

    #[test]
    fn test_fqid() {
        assert_eq!(
            parse_fqid(BakeParser::parse(Rule::fqid, "a").unwrap().next().unwrap()).unwrap(),
            crate::Fqid::new(&["a"])
        );
        assert_eq!(
            parse_fqid(
                BakeParser::parse(Rule::fqid, "a.b")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Fqid::new(&["a", "b"]),
        );
        assert_eq!(
            parse_fqid(
                BakeParser::parse(Rule::fqid, "a.b.c")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Fqid::new(&["a", "b", "c"]),
        );
    }

    #[test]
    fn test_exp() {
        assert_eq!(
            parse_exp(BakeParser::parse(Rule::exp, "xxx").unwrap().next().unwrap()).unwrap(),
            crate::Exp::Var(crate::Fqid::new(&["xxx"]))
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, "\"xxx\"")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Str("xxx".to_owned())
        );
        assert_eq!(
            parse_exp(BakeParser::parse(Rule::exp, "123").unwrap().next().unwrap()).unwrap(),
            crate::Exp::Int(123)
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, "+123")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Int(123)
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, "-123")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Int(-123)
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, "\"\\\"xxx\\\\\"")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Str("\"xxx\\".to_owned())
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, "xxx.yyy")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Var(crate::Fqid::new(&["xxx", "yyy"]))
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, ":xxx")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Symbol("xxx".to_owned()),
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, "fff()")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Call(crate::Fqid::new(&["fff"]), Vec::new())
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, "fff(xxx)")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Call(
                crate::Fqid::new(&["fff"]),
                vec![crate::Exp::Var(crate::Fqid::new(&["xxx"]))]
            )
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, "fff(xxx, yyy)")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Call(
                crate::Fqid::new(&["fff"]),
                vec![
                    crate::Exp::Var(crate::Fqid::new(&["xxx"])),
                    crate::Exp::Var(crate::Fqid::new(&["yyy"]))
                ]
            )
        );
    }

    #[test]
    fn test_template() {
        assert_eq!(
            parse_template(
                BakeParser::parse(Rule::template, "xxx")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            vec![crate::TemplateElem::Text("xxx".to_owned())]
        );
        assert_eq!(
            parse_template(
                BakeParser::parse(Rule::template, "xxx{{ yyy }}zzz")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            vec![
                crate::TemplateElem::Text("xxx".to_owned()),
                crate::TemplateElem::Exp(crate::Exp::Var(crate::Fqid::new(&["yyy"]))),
                crate::TemplateElem::Text("zzz".to_owned())
            ]
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("{{ xxx }}").unwrap(),
            crate::Exp::Template(vec![crate::TemplateElem::Exp(crate::Exp::Var(
                crate::Fqid::new(&["xxx"])
            ))])
        );
        assert_eq!(
            parse("${ xxx }").unwrap(),
            crate::Exp::Var(crate::Fqid::new(&["xxx"]))
        );
        assert_eq!(
            parse("xxx").unwrap(),
            crate::Exp::Template(vec![crate::TemplateElem::Text("xxx".to_owned())])
        );
    }
}

fn inside_out<T, E>(x: Option<Result<T, E>>) -> Result<Option<T>, E> {
    match x {
        Some(Ok(x)) => Ok(Some(x)),
        Some(Err(e)) => Err(e),
        None => Ok(None),
    }
}

fn check_keys<'a>(
    hash: &'a linked_hash_map::LinkedHashMap<Yaml, Yaml>,
    allowed: &[&str],
) -> Result<(), crate::Error> {
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
        Err(crate::Error::ConfigError(format!(
            "invalid keys {}",
            invalid_keys.join(", ")
        )))
    } else {
        Ok(())
    }
}

fn task_from_yaml(ctx: &str, yaml: &Yaml) -> Result<crate::Task, crate::Error> {
    let hash = yaml
        .as_hash()
        .ok_or_else(|| crate::Error::ConfigError(format!("rules.{} must be hash", ctx)))?;
    check_keys(hash, &["in", "out", "command"])?;
    let input =
        match hash
            .get(&Yaml::String("in".to_owned()))
            .ok_or_else(|| crate::Error::ConfigError(format!("rules.{}.in is required", ctx)))?
        {
            Yaml::String(s) => crate::Input::Single(parse(s.as_str()).map_err(|_| {
                crate::Error::ConfigError(format!("Syntax error on rules.{}.in", ctx))
            })?),
            _ => {
                return Err(crate::Error::ConfigError(
                    "rules.*.in must be string".to_owned(),
                ))
            }
        };
    let command = hash.get(&Yaml::String("command".to_owned())).map(|y| {
        y.as_str().ok_or_else(|| {
            crate::Error::ConfigError(format!("rules.{}.command must be string", ctx))
        })
    });
    let command = inside_out(command)?;
    let command = command.map(|command| parse(command));
    let command = inside_out(command)
        .map_err(|_| crate::Error::ConfigError(format!("Syntax error on rules.{}.command", ctx)))?;
    let out = match hash.get(&Yaml::String("out".to_owned())) {
        Some(Yaml::String(s)) => Some(parse(s.as_str()).map_err(|_| {
            crate::Error::ConfigError(format!("Syntax error on rules.{}.out", ctx))
        })?),
        None => None,
        _ => {
            return Err(crate::Error::ConfigError(
                "rules.*.out must be string".to_owned(),
            ))
        }
    };
    match (out, command) {
        (Some(out), Some(command)) => Ok(crate::Task::Produce {
            input,
            out,
            command,
        }),
        (None, Some(command)) => Ok(crate::Task::Execute { input, command }),
        (None, None) => Ok(crate::Task::Phony { input }),
        (Some(_), None) => Err(crate::Error::ConfigError(format!(
            "rule.{}.task: out requires command",
            ctx
        ))),
    }
}

fn rule_from_yaml(ctx: &str, yaml: &Yaml) -> Result<crate::Rule, crate::Error> {
    let hash = yaml
        .as_hash()
        .ok_or_else(|| crate::Error::ConfigError("rules.* must be hash".to_owned()))?;
    if hash.contains_key(&Yaml::String("source".to_owned())) {
        check_keys(hash, &["source", "task"])?;
        let source = hash
            .get(&Yaml::String("source".to_owned()))
            .unwrap()
            .as_str()
            .ok_or_else(|| {
                crate::Error::ConfigError(
                    "rules.*.source must be parsed as a yaml string".to_owned(),
                )
            })?;
        let task = hash
            .get(&Yaml::String("task".to_owned()))
            .ok_or_else(|| crate::Error::ConfigError(format!("rules.{}.task is required", ctx)))?;
        let source = parse(source)
            .map_err(|_| crate::Error::ConfigError("Cannot parse rules.*.source".to_owned()))?;
        let task = task_from_yaml(&format!("{}.task", ctx), task)?;
        Ok(crate::Rule::Map { source, task })
    } else {
        let task = task_from_yaml(ctx, yaml)?;
        Ok(crate::Rule::Single(task))
    }
}

pub fn config_from_str_src(src: &str) -> Result<crate::Config, crate::Error> {
    if let [yaml] = yaml_rust::YamlLoader::load_from_str(src)
        .map_err(crate::Error::ConfigScanError)?
        .as_slice()
    {
        let rules = yaml
            .as_hash()
            .ok_or_else(|| crate::Error::ConfigError("root object must be hash".to_owned()))?
            .get(&Yaml::String("rules".to_owned()))
            .ok_or_else(|| crate::Error::ConfigError("rules is required".to_owned()))?
            .as_hash()
            .ok_or_else(|| crate::Error::ConfigError("rules must be hash".to_owned()))?;
        Ok(crate::Config {
            rules: rules
                .into_iter()
                .map(|(key, yaml)| {
                    let key = key
                        .as_str()
                        .ok_or_else(|| {
                            crate::Error::ConfigError("key of rules.* must be string".to_owned())
                        })?
                        .to_owned();
                    let val = rule_from_yaml(&key, yaml)?;
                    Ok((key, val))
                })
                .collect::<Result<Vec<_>, _>>()?,
        })
    } else {
        Err(crate::Error::ConfigError(
            "config must include single yaml object".to_owned(),
        ))
    }
}
