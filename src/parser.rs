use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::Exp;

#[derive(Parser)]
#[grammar = "parser.pest"]
struct BakeParser;

#[derive(Debug)]
enum Error {}

fn parse_fqid(pair: Pair<Rule>) -> Result<crate::Fqid, Error> {
    match pair.as_rule() {
        Rule::fqid => Ok(pair
            .into_inner()
            .map(|p| p.as_str().to_owned())
            .collect::<Vec<_>>()),
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
        _ => unreachable!(),
    }
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

#[allow(unused)]
fn parse_expand(pair: Pair<Rule>) -> Result<crate::Expand, Error> {
    match pair.as_rule() {
        Rule::expand => {
            let pair = pair.into_inner().next().unwrap();
            match pair.as_rule() {
                Rule::array_expand => Ok(crate::Expand::Array(parse_exp(
                    pair.into_inner().next().unwrap(),
                )?)),
                Rule::string_expand => Ok(crate::Expand::String(parse_exp(
                    pair.into_inner().next().unwrap(),
                )?)),
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
            vec!["a".to_owned()]
        );
        assert_eq!(
            parse_fqid(
                BakeParser::parse(Rule::fqid, "a.b")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            vec!["a".to_owned(), "b".to_owned()]
        );
        assert_eq!(
            parse_fqid(
                BakeParser::parse(Rule::fqid, "a.b.c")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            vec!["a".to_owned(), "b".to_owned(), "c".to_owned()]
        );
    }

    #[test]
    fn test_exp() {
        assert_eq!(
            parse_exp(BakeParser::parse(Rule::exp, "xxx").unwrap().next().unwrap()).unwrap(),
            crate::Exp::Var(vec!["xxx".to_owned()])
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
            crate::Exp::Var(vec!["xxx".to_owned(), "yyy".to_owned()])
        );
        assert_eq!(
            parse_exp(
                BakeParser::parse(Rule::exp, "fff()")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Exp::Call(vec!["fff".to_owned()], Vec::new())
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
                vec!["fff".to_owned()],
                vec![crate::Exp::Var(vec!["xxx".to_owned()])]
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
                vec!["fff".to_owned()],
                vec![
                    crate::Exp::Var(vec!["xxx".to_owned()]),
                    crate::Exp::Var(vec!["yyy".to_owned()])
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
                crate::TemplateElem::Exp(crate::Exp::Var(vec!["yyy".to_owned()])),
                crate::TemplateElem::Text("zzz".to_owned())
            ]
        );
    }

    #[test]
    fn test_parse_expand() {
        assert_eq!(
            parse_expand(
                BakeParser::parse(Rule::expand, "$array{ xxx }")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Expand::Array(crate::Exp::Var(vec!["xxx".to_owned()]))
        );
        assert_eq!(
            parse_expand(
                BakeParser::parse(Rule::expand, "$string{ xxx }")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap(),
            crate::Expand::String(crate::Exp::Var(vec!["xxx".to_owned()]))
        );
    }
}

#[derive(Serialize, Deserialize)]
struct Task {
    deps: HashMap<String, String>,
    command: String,
}

#[derive(Serialize, Deserialize)]
pub struct Config {
    tasks: Vec<Task>,
}

fn parse_dep(_: &str) -> Result<crate::Expand, crate::Error> {
    unimplemented!()
}
fn parse_command(src: &str) -> Result<crate::Template, crate::Error> {
    let pair = BakeParser::parse(Rule::template, src)
        .map_err(|e| crate::Error::SyntaxError(e.line_col))?
        .next()
        .unwrap();
    Ok(parse_template(pair).unwrap())
}

fn parse_task(src: Task) -> Result<crate::Task, crate::Error> {
    Ok(crate::Task {
        deps: src
            .deps
            .into_iter()
            .map(|(k, v)| parse_dep(&v).map(|d| (k, d)))
            .collect::<Result<HashMap<_, _>, _>>()?,
        command: parse_command(&src.command)?,
    })
}

pub fn parse(src: &str) -> Result<crate::Config, crate::Error> {
    let raw: Config = serde_yaml::from_str(src).map_err(crate::Error::ConfigScanError)?;
    Ok(crate::Config {
        tasks: raw
            .tasks
            .into_iter()
            .map(parse_task)
            .collect::<Result<Vec<_>, _>>()?,
    })
}
