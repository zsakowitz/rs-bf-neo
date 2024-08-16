use pest::{error::Error, iterators::Pair, Parser};
use pest_derive::Parser;
use std::{collections::HashMap, fmt};

#[derive(Copy, Clone, Hash)]
pub struct Name(u32);

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

#[derive(Copy, Clone, Hash)]
pub struct Offset(pub(super) isize);

impl fmt::Debug for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            0 => write!(f, "@"),
            0.. => write!(f, "@>{}", self.0),
            ..0 => write!(f, "@<{}", -self.0),
        }
    }
}

#[derive(Clone, Debug, Hash)]
pub enum LiteralSingle {
    Int(i32),
    Str(String),
}

#[derive(Clone, Debug, Hash)]
pub enum Literal {
    Single(LiteralSingle),
    Array(Vec<LiteralSingle>),
}

/// Instead of traditional expressions, everything in this language is a target.
/// All targets listed here, then, are just references to specific cells once
/// compiled away.
#[derive(Clone, Hash)]
#[non_exhaustive]
pub enum TargetInner {
    /// references a local
    Local(Name),
    /// creates a new local with the given value
    Int(i32),
    /// creates a new local with the given value
    Str(String),
    /// references a cell relative to the one currently pointed at
    Relative(Offset),
    /// references a block of targets
    Array(Vec<Target>),
    /// returns the value of the last statement
    Expr(Box<Script>),
}

impl TargetInner {
    fn is_multiline(&self) -> bool {
        match self {
            Self::Local(_) | Self::Int(_) | Self::Str(_) | Self::Relative(_) => false,
            Self::Expr(_) => true,
            Self::Array(x) => x.iter().any(|x| x.inner.is_multiline()),
        }
    }
}

impl fmt::Debug for TargetInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Local(x) => x.fmt(f),
            Self::Int(x) => x.fmt(f),
            Self::Str(x) => x.fmt(f),
            Self::Relative(x) => x.fmt(f),
            Self::Expr(x) => f.debug_tuple("Expr").field(x).finish(),
            Self::Array(x) => {
                if x.iter().any(|x| x.inner.is_multiline()) {
                    f.debug_list().entries(x).finish()
                } else {
                    write!(f, "{:?}", x)
                }
            }
        }
    }
}

#[derive(Clone, Debug, Hash)]
pub struct Target {
    pub(super) inner: TargetInner,
    /// an index into the target (if it's an array)
    pub(super) index: Option<u32>,
}

#[derive(Clone, Debug, Default, Hash)]
#[non_exhaustive]
pub struct Script {
    pub(super) stmts: Vec<Statement>,
    pub(super) fns: Vec<FnDeclaration>,
}

#[derive(Copy, Clone, Hash)]
#[non_exhaustive]
pub enum BuiltinName {
    Inc,
    Dec,
    Read,
    Write,
    Goto,
    AssertIsZero,
    AssertIsUnknown,
}

impl fmt::Debug for BuiltinName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\"{}\"",
            match *self {
                Self::Inc => "inc",
                Self::Dec => "dec",
                Self::Read => "read",
                Self::Write => "write",
                Self::Goto => "goto",
                Self::AssertIsZero => "assert::is_zero",
                Self::AssertIsUnknown => "assert::is_unknown",
            }
        )
    }
}

/// if None, it is not an array
/// if Some(None), an auto-sized array
/// if Some(Some(size)), an array of size `size`
pub type ArraySize = Option<Option<usize>>;

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum LetBindingInDestructure {
    Ignored,
    Named {
        name: Name,
        default: Option<Literal>,
    },
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum LetBinding {
    Standard {
        name: Name,
        size: ArraySize,
    },
    Destructured {
        els: Vec<LetBindingInDestructure>,
        accept_inexact: bool,
    },
}

#[derive(Copy, Clone, Hash)]
#[non_exhaustive]
pub enum FnName {
    Builtin(BuiltinName),
    UserDefined(Name),
}

impl fmt::Debug for FnName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            FnName::Builtin(x) => write!(f, "{x:?}"),
            FnName::UserDefined(x) => write!(f, "{x:?}"),
        }
    }
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum Statement {
    /// declares a variable
    Let {
        binding: LetBinding,
        mutable: bool,
        value: Option<Literal>,
    },
    /// runs the given code for each target of an array
    For {
        mutable: bool,
        bound: Name,
        array: Target,
        body: Script,
    },
    /// runs the given code while `target` is nonzero
    While { target: Target, body: Script },
    /// calls a function
    Call {
        name: FnName,
        is_unsafe: bool,
        args: Vec<Option<Target>>,
        rest: Option<Target>,
    },
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub struct FnParam {
    pub(super) mutable: bool,
    pub(super) binding: LetBinding,
    pub(super) default: Option<Literal>,
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub struct FnRestParam {
    pub(super) mutable: bool,
    pub(super) name: Name,
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub struct FnDeclaration {
    pub(super) name: Name,
    pub(super) args: Vec<FnParam>,
    pub(super) rest: Option<FnRestParam>,
    /// `returns` specifies what a (...) expression containing this function
    /// call should target
    pub(super) returns: Option<Target>,
    pub(super) body: Script,
}

#[derive(Parser)]
#[grammar = "rcr/grammar.pest"] // relative to src
struct MyParser;

struct NameManager {
    data: HashMap<String, Name>,
    next: u32,
}

impl NameManager {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
            next: 0,
        }
    }

    fn get(&mut self, name: &str) -> Name {
        if let Some(x) = self.data.get(name) {
            return *x;
        }
        let value = self.next;
        self.next += 1;
        *self.data.entry(name.to_string()).or_insert(Name(value))
    }
}

pub fn parse(input: &str) -> Result<Vec<FnDeclaration>, Error<Rule>> {
    let pair = MyParser::parse(Rule::main, input)?
        .next()
        .unwrap()
        .into_inner();
    let mut names = NameManager::new();

    return Ok(pair
        .filter(|x| x.as_rule() == Rule::r#fn)
        .map(|x| parse_fn(&mut names, x))
        .collect());

    fn parse_offset(s: &str) -> Offset {
        let direction = match s {
            "@" => return Offset(0),
            _ if s.starts_with(">") => 1,
            _ => -1,
        };
        let size = s[1..].parse::<isize>().unwrap();
        Offset(direction * size)
    }

    fn parse_str(pair: Pair<Rule>) -> String {
        pair.into_inner()
            .map(|pair| match pair.as_str() {
                "\\\\" => "\\",
                "\\\"" => "\"",
                "\\\n" => "\n",
                "\\\r" => "\r",
                x => x,
            })
            .collect()
    }

    fn parse_literal(pair: Pair<Rule>) -> Literal {
        return match pair.as_rule() {
            Rule::literal_one => Literal::Single(parse_literal_one(pair)),
            Rule::literal_array => {
                Literal::Array(pair.into_inner().map(|x| parse_literal_one(x)).collect())
            }
            _ => unreachable!(),
        };

        fn parse_literal_one(pair: Pair<Rule>) -> LiteralSingle {
            match pair.as_rule() {
                Rule::int => LiteralSingle::Int(pair.as_str().parse().unwrap()),
                Rule::str => LiteralSingle::Str(parse_str(pair)),
                _ => unreachable!(),
            }
        }
    }

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
                Rule::target_relative => TargetInner::Relative(parse_offset(pair.as_str())),
                Rule::int => TargetInner::Int(pair.as_str().parse().unwrap()),
                Rule::str => TargetInner::Str(parse_str(pair)),
                Rule::target_expr => TargetInner::Expr(Box::new({
                    let inner = pair.into_inner().next().unwrap();
                    match inner.as_rule() {
                        Rule::target_expr_one => Script {
                            stmts: vec![parse_stmt(names, inner.into_inner().next().unwrap())],
                            fns: Vec::new(),
                        },
                        Rule::target_expr_block => {
                            parse_script(names, inner.into_inner().next().unwrap())
                        }
                        _ => unreachable!(),
                    }
                })),
                rule => unreachable!("{rule:?} is not a target"),
            }
        }
    }

    /// Expects a `Rule::stmt_...` to be passed.
    fn parse_stmt(names: &mut NameManager, pair: Pair<Rule>) -> Statement {
        match pair.as_rule() {
            Rule::stmt_call => {
                let mut inner = pair.into_inner();
                let is_unsafe = inner.next().unwrap().into_inner().next().is_some();
                let fn_name = inner.next().unwrap().as_str();
                let args = inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|x| match x.as_rule() {
                        Rule::target => Some(parse_target(names, x)),
                        Rule::keyword_underscore => None,
                        _ => unreachable!(),
                    })
                    .collect();
                let rest = inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .map(|x| parse_target(names, x));
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
                let mutable = inner.next().unwrap().into_inner().next().is_some();
                let name = inner.next().unwrap();
                inner.next().unwrap();
                let target = inner.next().unwrap();
                let block = inner.next().unwrap();
                Statement::For {
                    mutable,
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
                    mutable,
                    binding: parse_let_bindable(names, let_bindable),
                    value: let_init.map(parse_literal),
                }
            }
            _ => todo!(),
        }
    }

    /// Expects a `Rule::stmt_list_...` to be passed.
    fn parse_script(names: &mut NameManager, pair: Pair<Rule>) -> Script {
        let mut fns = Vec::new();
        let mut stmts = Vec::new();

        for x in pair.into_inner() {
            if x.as_rule() == Rule::r#fn {
                fns.push(parse_fn(names, x));
            } else {
                stmts.push(parse_stmt(names, x));
            }
        }

        Script { stmts, fns }
    }

    /// Expects a `Rule::let_dest` or `Rule::let_bind_standard` to be passed.
    fn parse_let_bindable(names: &mut NameManager, pair: Pair<Rule>) -> LetBinding {
        match pair.as_rule() {
            Rule::let_bind_standard => {
                let mut inner = pair.into_inner();
                LetBinding::Standard {
                    name: names.get(inner.next().unwrap().as_str()),
                    size: inner
                        .next()
                        .map(|x| x.into_inner().next().map(|x| x.as_str().parse().unwrap())),
                }
            }
            Rule::let_dest => {
                let mut inner = pair.into_inner();
                let els = inner.next().unwrap().into_inner();
                let accept_inexact = inner.next().is_some();
                LetBinding::Destructured {
                    els: els
                        .map(|x| match x.into_inner().next() {
                            Some(x) => {
                                let mut inner = x.into_inner();
                                LetBindingInDestructure::Named {
                                    name: names.get(inner.next().unwrap().as_str()),
                                    default: inner.next().map(parse_literal),
                                }
                            }
                            None => LetBindingInDestructure::Ignored,
                        })
                        .collect(),
                    accept_inexact,
                }
            }
            _ => unreachable!(),
        }
    }

    /// Expects a `Rule::fn` to be passed.
    fn parse_fn(names: &mut NameManager, pair: Pair<Rule>) -> FnDeclaration {
        assert_eq!(pair.as_rule(), Rule::r#fn);

        let mut inner = pair.into_inner();
        inner.next().unwrap(); // fn keyword
        let name = names.get(inner.next().unwrap().as_str());
        let mut fn_args = inner.next().unwrap().into_inner();
        let (args, rest) = match (fn_args.next(), fn_args.next()) {
            (None, _) => (None, None),
            (Some(a), None) if a.as_rule() == Rule::fn_rest => (None, Some(a)),
            (Some(a), None) => (Some(a.into_inner()), None),
            (Some(a), Some(b)) => (Some(a.into_inner()), Some(b)),
        };
        let returns = inner
            .next()
            .unwrap()
            .into_inner()
            .next()
            .map(|x| parse_target(names, x));
        let body = inner
            .next()
            .map(|x| parse_script(names, x))
            .unwrap_or_default();

        FnDeclaration {
            name,
            args: args
                .map(|x| {
                    x.map(|x| {
                        let mut inner = x.into_inner();
                        FnParam {
                            mutable: inner.next().unwrap().into_inner().next().is_some(),
                            binding: parse_let_bindable(names, inner.next().unwrap()),
                            default: inner.next().map(parse_literal),
                        }
                    })
                    .collect()
                })
                .unwrap_or_default(),
            rest: rest.map(|x| {
                let mut inner = x.into_inner();
                FnRestParam {
                    mutable: inner.next().unwrap().into_inner().next().is_some(),
                    name: names.get(inner.next().unwrap().as_str()),
                }
            }),
            returns,
            body,
        }
    }
}
