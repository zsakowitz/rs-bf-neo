mod syntax;

enum RcrTarget {
    Retval(usize),
    Local(usize),
}

enum RcrFnArg {
    Cell(RcrTarget),
    Int(i32),
    Bool(bool),
    Str(String),
}

enum RcrStmt {
    Declare(usize),
    DeclareBlock {
        name: usize,
        count: usize,
    },
    Inc(RcrTarget),
    Dec(RcrTarget),
    Read(RcrTarget),
    Write(RcrTarget),
    Call {
        name: usize,
        args: Vec<RcrFnArg>,
        retval: Vec<RcrTarget>,
    },
}

struct RcrFn {
    locals: usize,
    returns: usize,
    stmts: RcrStmt,
}

/*
    Argument Types

    ref name    // &name in rust
    mut name    // &mut name in rust
        name    // name in rust
*/
