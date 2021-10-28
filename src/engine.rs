use std::collections::HashMap;

use crate::Fqid;
use maplit::hashmap;

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
}
