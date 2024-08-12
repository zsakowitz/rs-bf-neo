use pest::Parser;
use pest_derive::Parser;

/// Instead of traditional expressions, everything in this language is a target.
/// All targets listed here, then, are just references to specific cells once
/// compiled away.
pub enum TargetInner {
    /// references a local
    Local(u32),
    /// references a block of targets
    Array(Vec<Target>),
    /// references the automatically-generated percent symbol
    PercentSymbol,
    /// creates a new local with the given value
    Int(u32),
    /// creates a new local with the given value
    Char(char),
    /// creates a new local with the given value
    Str(String),
    /// runs the statements inside
    ExprPercent {
        expr: Box<Script>,
        // what target is bound to `%`
        // defaults to a new cell with value zero
        target: Option<Box<Target>>,
    },
    Expr {
        expr: Box<Script>,
    },
}

pub struct Target {
    inner: TargetInner,
    /// an index into the target (if it's an array)
    index: Option<u32>,
}

pub type Script = Vec<Statement>;

pub enum BuiltinName {
    Inc,
    Dec,
    Read,
    Write,
    Goto,
}

pub enum LetBinding {
    Single,
    Array(Option<usize>),
}

pub enum Statement {
    /// runs a builtin command
    Builtin { name: BuiltinName, target: Target },
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
    Exit,
    /// runs a function call
    Call { name: u32, args: Vec<Target> },
}

pub struct FnParam {
    mutable: bool,
    name: u32,
    default: Option<Target>,
}

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

fn parse(input: &str) {
    let a = MyParser::parse(Rule::main, input);
}
