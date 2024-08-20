use pest::{error::Error, iterators::Pair, Parser};
use pest_derive::Parser;
use std::{collections::HashMap, fmt};

use crate::builder::CellState;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Name(pub(super) u32);

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
pub enum Literal {
    Int(i32),
    Str(String),
    IntArray(Vec<i32>),
}

impl Literal {
    pub fn memory_needed(&self) -> usize {
        match self {
            Self::Int(_) => 1,
            Self::Str(x) => x.len(),
            Self::IntArray(x) => x.len(),
        }
    }
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
pub enum Builtin {
    Inc,
    Dec,
    Read,
    Write,
    Goto,
    Assert(CellState),
}

impl Builtin {
    pub fn mutates(self) -> bool {
        match self {
            Self::Assert(_) | Self::Goto => false,
            Self::Dec | Self::Inc | Self::Read | Self::Write => true,
        }
    }
}

impl fmt::Debug for Builtin {
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
                Self::Assert(CellState::Zeroed) => "assert::is_zero",
                Self::Assert(CellState::Unknown) => "assert::is_unknown",
            }
        )
    }
}

#[derive(Copy, Clone, Debug, Hash)]
pub enum Kind {
    Scalar,
    Array(ArraySize),
}

#[derive(Copy, Clone, Debug, Hash)]
pub enum ArraySize {
    Inferred,
    Exact(usize),
    AtLeast(usize),
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum BindingInDestructure {
    Ignored,
    Named { name: Name, default: Option<i32> },
}

impl BindingInDestructure {
    /// Returns `true` if the binding in destructure is [`Ignored`].
    ///
    /// [`Ignored`]: BindingInDestructure::Ignored
    #[must_use]
    pub fn is_ignored(&self) -> bool {
        matches!(self, Self::Ignored)
    }
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum Binding {
    Standard {
        name: Name,
        kind: Kind,
    },
    Destructured {
        els: Vec<BindingInDestructure>,
        accept_inexact: bool,
    },
}

#[derive(Copy, Clone, Hash)]
#[non_exhaustive]
pub enum FnName {
    Builtin(Builtin),
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
/// declares a variable
pub struct Let {
    pub mutable: bool,
    pub binding: Binding,
    pub value: Option<Literal>,
}

#[derive(Clone, Debug, Hash)]
/// runs the given code for each target of an array
pub struct For {
    pub mutable: bool,
    pub bound: Name,
    pub array: Target,
    pub body: Script,
}

#[derive(Clone, Debug, Hash)]
/// runs the given code while `target` is nonzero
pub struct While {
    pub target: Target,
    pub body: Script,
}

#[derive(Clone, Debug, Hash)]
/// calls a function
pub struct Call {
    pub name: FnName,
    pub is_unsafe: bool,
    pub args: Vec<Option<Target>>,
    pub rest: Option<Target>,
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum Statement {
    Let(Let),
    For(For),
    While(While),
    Call(Call),
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub struct FnParam {
    pub(super) mutable: bool,
    pub(super) binding: Binding,
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

pub struct NameManager {
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

    pub fn get(&self, name: &str) -> Option<Name> {
        self.data.get(name).copied()
    }

    pub fn get_or_create(&mut self, name: &str) -> Name {
        if let Some(x) = self.data.get(name) {
            return *x;
        }
        let value = self.next;
        self.next += 1;
        *self.data.entry(name.to_string()).or_insert(Name(value))
    }
}

pub struct Parse {
    pub fns: Vec<FnDeclaration>,
    pub names: NameManager,
}

pub fn parse(input: &str) -> Result<Parse, Error<Rule>> {
    let pair = MyParser::parse(Rule::main, input)?
        .next()
        .unwrap()
        .into_inner();
    let mut names = NameManager::new();

    let fns = pair
        .filter(|x| x.as_rule() == Rule::r#fn)
        .map(|x| parse_fn(&mut names, x))
        .collect();

    return Ok(Parse { fns, names });

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
            Rule::int => Literal::Int(pair.as_str().parse().unwrap()),
            Rule::str => Literal::Str(parse_str(pair)),
            Rule::int_array => Literal::IntArray(
                pair.into_inner()
                    .map(|x| x.as_str().parse().unwrap())
                    .collect(),
            ),
            _ => unreachable!(),
        };
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
                Rule::target_name => TargetInner::Local(names.get_or_create(pair.as_str())),
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
                Statement::Call(Call {
                    name: match fn_name {
                        "inc" => FnName::Builtin(Builtin::Inc),
                        "dec" => FnName::Builtin(Builtin::Dec),
                        "read" => FnName::Builtin(Builtin::Read),
                        "write" => FnName::Builtin(Builtin::Write),
                        "goto" => FnName::Builtin(Builtin::Goto),
                        "assert::is_zero" => FnName::Builtin(Builtin::Assert(CellState::Zeroed)),
                        "assert::is_unknown" => {
                            FnName::Builtin(Builtin::Assert(CellState::Unknown))
                        }
                        _ => FnName::UserDefined(names.get_or_create(fn_name)),
                    },
                    is_unsafe,
                    args,
                    rest,
                })
            }
            Rule::stmt_for => {
                let mut inner = pair.into_inner();
                inner.next().unwrap();
                let mutable = inner.next().unwrap().into_inner().next().is_some();
                let name = inner.next().unwrap();
                inner.next().unwrap();
                let target = inner.next().unwrap();
                let block = inner.next().unwrap();
                Statement::For(For {
                    mutable,
                    bound: names.get_or_create(name.as_str()),
                    array: parse_target(names, target),
                    body: parse_script(names, block),
                })
            }
            Rule::stmt_while => {
                let mut inner = pair.into_inner();
                inner.next().unwrap();
                let target = inner.next().unwrap();
                let block = inner.next().unwrap();
                Statement::While(While {
                    target: parse_target(names, target),
                    body: parse_script(names, block),
                })
            }
            Rule::stmt_let => {
                let mut inner = pair.into_inner();
                inner.next().unwrap();
                let mutable = inner.next().unwrap().into_inner().next().is_some();
                let let_bindable = inner.next().unwrap();
                let let_init = inner.next();

                Statement::Let(Let {
                    mutable,
                    binding: parse_let_bindable(names, let_bindable),
                    value: let_init.map(parse_literal),
                })
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
    fn parse_let_bindable(names: &mut NameManager, pair: Pair<Rule>) -> Binding {
        match pair.as_rule() {
            Rule::let_bind_standard => {
                let mut inner = pair.into_inner();
                Binding::Standard {
                    name: names.get_or_create(inner.next().unwrap().as_str()),
                    kind: match inner.next() {
                        None => Kind::Scalar,
                        Some(x) => {
                            match x.into_inner().next().map(|x| x.as_str().parse().unwrap()) {
                                Some(x) => Kind::Array(ArraySize::Exact(x)),
                                None => Kind::Array(ArraySize::Inferred),
                            }
                        }
                    },
                }
            }
            Rule::let_dest => {
                let mut inner = pair.into_inner();
                let els = inner.next().unwrap().into_inner();
                let accept_inexact = inner.next().is_some();
                Binding::Destructured {
                    els: els
                        .map(|x| match x.into_inner().next() {
                            Some(x) => {
                                let mut inner = x.into_inner();
                                BindingInDestructure::Named {
                                    name: names.get_or_create(inner.next().unwrap().as_str()),
                                    default: inner.next().map(|x| x.as_str().parse().unwrap()),
                                }
                            }
                            None => BindingInDestructure::Ignored,
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
        let name = names.get_or_create(inner.next().unwrap().as_str());
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
                    name: names.get_or_create(inner.next().unwrap().as_str()),
                }
            }),
            returns,
            body,
        }
    }
}
