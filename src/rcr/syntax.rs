use std::{collections::HashMap, ops::BitAndAssign};

use pest::{error::Error, iterators::Pair, pratt_parser::Op, Parser};
use pest_derive::Parser;

/// Instead of traditional expressions, everything in this language is a target.
/// All targets listed here, then, are just references to specific cells once
/// compiled away.
#[derive(Clone, Debug, Hash)]
pub enum TargetInner {
    /// references a local
    Local(u32),
    /// references a block of targets
    Array(Vec<Target>),
    /// creates a new local with the given value
    Int(u32),
    /// creates a new local with the given value
    Char(char),
    /// creates a new local with the given value
    Str(String),
    /// returns the value of the last statement
    Expr { expr: Box<Script> },
}

#[derive(Clone, Debug, Hash)]
pub struct Target {
    inner: TargetInner,
    /// an index into the target (if it's an array)
    index: Option<u32>,
}

pub type Script = Vec<Statement>;

#[derive(Clone, Debug, Hash)]
pub enum BuiltinName {
    Inc,
    Dec,
    Read,
    Write,
    Goto,
}

#[derive(Clone, Debug, Hash)]
pub enum LetBinding {
    Standard {
        name: u32,
        /// if None, it is not an array
        /// if Some(None), an auto-sized array
        /// if Some(Some(size)), an array of size `size`
        size: Option<Option<usize>>,
    },
    Destructured {
        /// a None in this Vec means the element is ignored
        els: Vec<Option<u32>>,
    },
}

#[derive(Clone, Debug, Hash)]
pub enum Statement {
    /// declares a variable
    Let {
        binding: LetBinding,
        value: Option<Target>,
    },
    /// runs the given code for each target of an array
    Each {
        bound: u32,
        array: Target,
        body: Script,
    },
    /// runs the given code while `target` is nonzero
    While { target: Target, body: Script },
    /// creates a block which can be exited from
    Breakable {
        target: Option<Target>,
        body: Script,
    },
    /// only valid in an `Exitable` block
    Break,
    /// calls a function
    Call { name: u32, args: Vec<Target> },
    /// calls a builtin function
    CallBuiltin {
        name: BuiltinName,
        args: Vec<Target>,
    },
}

#[derive(Clone, Debug, Hash)]
pub struct FnParam {
    mutable: bool,
    name: u32,
    default: Option<Target>,
}

#[derive(Clone, Debug, Hash)]
pub struct FnDeclaration {
    name: u32,
    args: Vec<FnParam>,
    /// `returns` specifies what a (...) expression containing this function
    /// call should target
    returns: Option<Target>,
    body: Script,
}

#[derive(Parser)]
#[grammar = "rcr/grammar.pest"] // relative to src
struct MyParser;

struct NameManager {
    data: HashMap<String, u32>,
    next: u32,
}

impl NameManager {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
            next: 0,
        }
    }

    fn get(&mut self, name: &str) -> u32 {
        if let Some(x) = self.data.get(name) {
            return *x;
        }
        let value = self.next;
        self.next += 1;
        *self.data.entry(name.to_string()).or_insert(value)
    }
}

fn parse(input: &str) -> Result<(), Error<Rule>> {
    let mut pairs = MyParser::parse(Rule::stmt_let, input)?;
    let mut names = NameManager::new();

    let first = pairs.next().unwrap();
    dbg!(parse_stmt(&mut names, first));

    return Ok(());

    /// Expects a `Rule::target` to be passed.
    fn parse_target(names: &mut NameManager, pair: Pair<Rule>) -> Target {
        let mut inner = pair.into_inner();
        let target_inner = inner.next().unwrap();
        let int = inner.next();

        return Target {
            inner: parse_target_inner(names, target_inner.into_inner().next().unwrap()),
            index: int.map(|x| x.as_str().parse().unwrap()),
        };

        fn parse_target_inner(names: &mut NameManager, pair: Pair<Rule>) -> TargetInner {
            match pair.as_rule() {
                Rule::target_array => {
                    TargetInner::Array(pair.into_inner().map(|x| parse_target(names, x)).collect())
                }
                Rule::target_name => TargetInner::Local(names.get(pair.as_str())),
                Rule::target_lit_int => TargetInner::Int(pair.as_str().parse().unwrap()),
                Rule::target_lit_str => TargetInner::Str(
                    pair.into_inner()
                        .map(|pair| match pair.as_str() {
                            "\\\\" => "\\",
                            "\\\"" => "\"",
                            "\\\n" => "\n",
                            "\\\r" => "\r",
                            x => x,
                        })
                        .collect(),
                ),
                Rule::target_expr => TargetInner::Expr {
                    expr: Box::new(parse_script(names, pair.into_inner().next().unwrap())),
                },
                rule => unreachable!("{rule:?} is not a target"),
            }
        }
    }

    /// Expects a `Rule::stmt_...` to be passed.
    fn parse_stmt(names: &mut NameManager, pair: Pair<Rule>) -> Statement {
        match pair.as_rule() {
            Rule::stmt_break => Statement::Break,
            Rule::stmt_breakable => {
                let mut inner = pair.into_inner();
                let (target, body) = match (inner.next().unwrap(), inner.next()) {
                    (target, Some(body)) => (Some(target), body),
                    (body, None) => (None, body),
                };
                Statement::Breakable {
                    target: target.map(|pair| parse_target(names, pair)),
                    body: parse_script(names, body),
                }
            }
            Rule::stmt_call => {
                let mut inner = pair.into_inner();
                let fn_name = inner.next().unwrap().as_str();
                let args = inner.map(|x| parse_target(names, x)).collect();
                Statement::CallBuiltin {
                    name: match fn_name {
                        "inc" => BuiltinName::Inc,
                        "dec" => BuiltinName::Dec,
                        "read" => BuiltinName::Read,
                        "write" => BuiltinName::Write,
                        "goto" => BuiltinName::Goto,
                        name => {
                            return Statement::Call {
                                name: names.get(name),
                                args,
                            }
                        }
                    },
                    args,
                }
            }
            Rule::stmt_each => {
                let mut inner = pair.into_inner();
                inner.next().unwrap();
                let name = inner.next().unwrap();
                inner.next().unwrap();
                let target = inner.next().unwrap();
                let block = inner.next().unwrap();
                Statement::Each {
                    bound: names.get(name.as_str()),
                    array: parse_target(names, target),
                    body: parse_script(names, block),
                }
            }
            Rule::stmt_while => {
                let mut inner = pair.into_inner();
                inner.next().unwrap();
                let target = inner.next().unwrap();
                let block = inner.next().unwrap();
                Statement::While {
                    target: parse_target(names, target),
                    body: parse_script(names, block),
                }
            }
            Rule::stmt_let => {
                let mut inner = pair.into_inner();
                inner.next().unwrap();
                let let_bindable = inner.next().unwrap();
                let let_init = inner.next();

                Statement::Let {
                    binding: match let_bindable.as_rule() {
                        Rule::let_bind_dest => todo!(),
                        Rule::let_bind_standard => {
                            let mut inner = let_bindable.into_inner();
                            let name = inner.next().unwrap();
                            let array_size = inner.next();
                            LetBinding::Standard {
                                name: names.get(name.as_str()),
                                size: array_size.map(|x| {
                                    x.into_inner().next().map(|y| y.as_str().parse().unwrap())
                                }),
                            }
                        }
                        _ => unreachable!(),
                    },
                    value: let_init.map(|x| parse_target(names, x)),
                }
            }
            _ => todo!(),
        }
    }

    /// Expects a `Rule::stmt_list_...` to be passed.
    fn parse_script(names: &mut NameManager, pair: Pair<Rule>) -> Script {
        match pair.as_rule() {
            Rule::stmt_list_semi => pair.into_inner().map(|x| parse_stmt(names, x)).collect(),
            Rule::stmt_list_no_semi => pair.into_inner().map(|x| parse_stmt(names, x)).collect(),
            _ => unreachable!(),
        }    
    }
}

#[cfg(test)]
#[test]
fn test() {
    parse("let c;").unwrap();
}
