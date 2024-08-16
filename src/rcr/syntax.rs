use pest::{error::Error, iterators::Pair, Parser};
use pest_derive::Parser;
use std::collections::HashMap;

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
    Int(i32),
    /// creates a new local with the given value
    Char(char),
    /// creates a new local with the given value
    Str(String),
    /// references a cell relative to the one currently pointed at
    Relative(isize),
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

/// if None, it is not an array
/// if Some(None), an auto-sized array
/// if Some(Some(size)), an array of size `size`
pub type ArraySize = Option<Option<usize>>;

#[derive(Clone, Debug, Hash)]
pub enum LetBinding {
    Standard {
        name: u32,
        size: ArraySize,
    },
    Destructured {
        /// a None in this Vec means the element is ignored
        els: Vec<Option<u32>>,
    },
}

#[derive(Clone, Debug, Hash)]
pub enum FnName {
    UserDefined(u32),
    Builtin(BuiltinName),
}

#[derive(Clone, Debug, Hash)]
pub enum Statement {
    /// declares a variable
    Let {
        binding: LetBinding,
        mutable: bool,
        value: Option<Target>,
    },
    /// runs the given code for each target of an array
    For {
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
    Call {
        name: FnName,
        is_unsafe: bool,
        args: Vec<Option<Target>>,
        rest: Option<Target>,
    },
}

#[derive(Clone, Debug, Hash)]
pub struct FnParam {
    mutable: bool,
    name: u32,
    size: ArraySize,
    default: Option<Target>,
}

#[derive(Clone, Debug, Hash)]
pub struct FnRestParam {
    mutable: bool,
    name: u32,
}

#[derive(Clone, Debug, Hash)]
pub struct FnDeclaration {
    name: u32,
    args: Vec<FnParam>,
    rest: Option<FnRestParam>,
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

pub fn parse(input: &str) -> Result<Vec<FnDeclaration>, Error<Rule>> {
    let pair = MyParser::parse(Rule::main, input)?.next().unwrap().into_inner();
    let mut names = NameManager::new();

    return Ok(pair.filter(|x| x.as_rule() == Rule::r#fn).map(|x| parse_fn(&mut names, x)).collect());

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
                Rule::target_relative => TargetInner::Relative(pair.as_str().parse().unwrap()),
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
                let is_unsafe = inner.next().unwrap().into_inner().next().is_some();
                let fn_name = inner.next().unwrap().as_str();
                let args = inner.next().unwrap().into_inner().map(|x| match x.as_rule() {
                    Rule::target => Some(parse_target(names, x)),
                    Rule::keyword_underscore => None,
                    _ => unreachable!(),
                }).collect();
                let rest = inner.next().unwrap().into_inner().next().map(|x| parse_target(names, x));
                Statement::Call {
                    name: match fn_name {
                        "inc" => FnName::Builtin(BuiltinName::Inc),
                        "dec" => FnName::Builtin(BuiltinName::Dec),
                        "read" => FnName::Builtin(BuiltinName::Read),
                        "write" => FnName::Builtin(BuiltinName::Write),
                        "goto" => FnName::Builtin(BuiltinName::Goto),
                        _ => FnName::UserDefined(names.get(fn_name)),
                    },
                    is_unsafe,
                    args,
                    rest,
                }
            }
            Rule::stmt_for => {
                let mut inner = pair.into_inner();
                inner.next().unwrap();
                let name = inner.next().unwrap();
                inner.next().unwrap();
                let target = inner.next().unwrap();
                let block = inner.next().unwrap();
                Statement::For {
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
                let mutable = inner.next().unwrap().into_inner().next().is_some();
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
                    mutable,
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

    /// Expects a `Rule::fn` to be passed.
    fn parse_fn(names: &mut NameManager, pair: Pair<Rule>) -> FnDeclaration {
        assert_eq!(pair.as_rule(), Rule::r#fn);

        let mut inner = pair.into_inner();
        inner.next().unwrap(); // fn keyword
        let name = names.get(inner.next().unwrap().as_str());
        let fn_args = inner.next().unwrap().into_inner();
        let returns = inner.next().unwrap().into_inner().next().map(|x| parse_target(names, x));
        let body = inner.next().map(|x| parse_script(names, x)).unwrap_or(Vec::new());
        
        let mut args = Vec::new();
        let rest = 'a: {
            for arg in fn_args {
                match arg.as_rule() {
                    Rule::fn_arg => {
                        let mut inner = arg.into_inner();
                        let mutable = inner.next().unwrap().into_inner().next().is_some();
                        let name = names.get(inner.next().unwrap().as_str());
                        let size = inner.next().unwrap().into_inner().next().map(|x| x.into_inner().next().map(|x| x.as_str().parse().unwrap()));
                        let default = inner.next().map(|x| parse_target(names, x));

                        args.push(FnParam {
                            mutable,
                            name,
                            size,
                            default,
                        });
                    }
                    Rule::fn_rest => {
                        let mut inner = arg.into_inner();
                        let mutable = inner.next().unwrap().into_inner().next().is_some();
                        let name = names.get(inner.next().unwrap().as_str());
                        
                        break 'a Some(FnRestParam {
                            name,
                            mutable,
                        });
                    }
                    _ => unreachable!(),
                }
            }

            None
        };

        FnDeclaration {
            name,
            args,
            rest,
            returns,
            body,
        }
    }
}
