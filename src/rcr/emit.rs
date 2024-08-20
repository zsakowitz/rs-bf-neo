use crate::rcr::{
    emit::{
        alloc::{stmt_let, Local, Locals, Memory},
        error::{e, Result},
        output::Output,
        scope::Scope,
    },
    syntax::{Builtin, Call, FnName, ParseTree, Script, Statement, Target, TargetInner},
};

mod output {
    #[derive(Clone, Debug, Default)]
    pub struct Output {
        data: String,
        pos: usize,
    }

    pub trait Settable: Copy {
        fn is_nonzero(self) -> bool;
        fn inc(self, output: &mut Output);
    }

    impl Settable for i32 {
        fn is_nonzero(self) -> bool {
            self != 0
        }

        fn inc(self, output: &mut Output) {
            if self < 0 {
                output.data += &"-".repeat(-self as usize);
            } else {
                output.data += &"+".repeat(self as usize);
            }
        }
    }

    impl Settable for u8 {
        fn is_nonzero(self) -> bool {
            self != 0
        }

        fn inc(self, output: &mut Output) {
            output.data += &"+".repeat(self as usize);
        }
    }

    impl Output {
        pub fn goto(&mut self, pos: usize) {
            if pos < self.pos {
                self.data += &"<".repeat(self.pos - pos);
            } else {
                self.data += &">".repeat(pos - self.pos);
            }
            self.pos = pos;
        }

        pub fn zero_current(&mut self) {
            self.data += "[-]";
        }

        pub fn inc_current_by(&mut self, value: impl Settable) {
            value.inc(self);
        }

        pub fn inc_current(&mut self) {
            self.data += "+";
        }

        pub fn dec_current(&mut self) {
            self.data += "+";
        }

        pub fn read_current(&mut self) {
            self.data += ",";
        }

        pub fn write_current(&mut self) {
            self.data += ".";
        }

        pub fn into_data(self) -> String {
            self.data
        }
    }
}

mod scope {
    use crate::rcr::{
        emit::error::{e, Result},
        syntax::{FnDeclaration, Name},
    };
    use std::collections::{hash_map::Entry, HashMap};

    #[derive(Clone, Debug)]
    pub struct Scope<'a> {
        parent: Option<&'a Scope<'a>>,
        fns: HashMap<Name, &'a FnDeclaration>,
    }

    impl<'a> Scope<'a> {
        fn create_hash_map(slice: &'a [FnDeclaration]) -> Result<HashMap<Name, &'a FnDeclaration>> {
            let mut fns = HashMap::new();
            for el in slice {
                match fns.entry(el.name) {
                    Entry::Occupied(_) => e!(DuplicateFunctionDefinition),
                    Entry::Vacant(entry) => entry.insert(el),
                };
            }
            Ok(fns)
        }

        fn create(parent: Option<&'a Self>, fns: &'a [FnDeclaration]) -> Result<Self> {
            Ok(Self {
                parent,
                fns: Self::create_hash_map(fns)?,
            })
        }

        pub fn new(fns: &'a [FnDeclaration]) -> Result<Self> {
            Scope::create(None, fns)
        }

        pub fn child(&'a self, next: &'a [FnDeclaration]) -> Result<Self> {
            Scope::create(Some(self), next)
        }

        pub fn get(&self, name: &Name) -> Option<&'a FnDeclaration> {
            let mut this = self;
            loop {
                match this.fns.get(name) {
                    Some(x) => return Some(*x),
                    None => match this.parent {
                        Some(x) => this = x,
                        None => return None,
                    },
                }
            }
        }
    }
}

mod alloc {
    use super::error::{e, Result};
    use super::output::{Output, Settable};
    use crate::{
        builder::CellState,
        rcr::syntax::{ArraySize, Binding, BindingInDestructure, Kind, Let, Literal, Name},
    };
    use std::ops::{Index, RangeFrom};
    use std::{
        cmp::max,
        collections::{hash_map::Entry, HashMap},
    };

    #[derive(Clone, Debug)]
    pub enum Content {
        Single(usize),
        Array(Vec<usize>),
    }

    impl Content {
        pub fn each(&self, mut f: impl FnMut(usize)) {
            match self {
                Self::Single(x) => f(*x),
                Self::Array(x) => {
                    for x in x {
                        f(*x)
                    }
                }
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct Local {
        content: Content,
        mutable: bool,
    }

    impl Local {
        pub fn index(&self, index: usize) -> Result<Local> {
            match &self.content {
                Content::Array(x) => match x.get(index) {
                    Some(x) => Ok(Local {
                        content: Content::Single(*x),
                        mutable: self.mutable,
                    }),
                    None => e!(IndexOutOfBounds),
                },
                Content::Single(_) => e!(IndexedIntoSingle),
            }
        }

        pub fn assert_mutability(&self, needs_mutability: bool) -> Result<&Content> {
            if needs_mutability && !self.mutable {
                e!(CannotMutateImmutableVariable)
            } else {
                Ok(&self.content)
            }
        }
    }

    #[derive(Clone, Debug, Default)]
    pub struct Memory {
        cells: Vec<CellState>,
    }

    impl Memory {
        pub fn len(&self) -> usize {
            self.cells.len()
        }

        pub fn assert(&mut self, pos: usize, state: CellState) {
            self.cells[pos] = state;
        }

        pub fn clear(&mut self, output: &mut Output, range: RangeFrom<usize>) {
            while self.cells.len() > range.start {
                let state = self.cells.pop().unwrap();
                let index = self.cells.len();
                match state {
                    CellState::Zeroed => {}
                    CellState::Unknown => {
                        output.goto(index);
                        output.zero_current();
                    }
                }
            }
        }
    }

    #[derive(Clone, Debug, Default)]
    pub struct Locals {
        named: HashMap<Name, Local>,
        unnamed: Vec<Local>,
    }

    impl Locals {
        pub fn get(&self, name: Name) -> Option<&Local> {
            self.named.get(&name)
        }

        fn set(&mut self, name: Name, local: Local) -> &Local {
            match self.named.entry(name) {
                Entry::Occupied(entry) => {
                    let value = entry.into_mut();
                    *value = local;
                    value
                }
                Entry::Vacant(entry) => entry.insert(local),
            }
        }

        fn hold(&mut self, local: Local) -> &Local {
            self.unnamed.push(local);
            self.unnamed.last().unwrap()
        }
    }

    impl ArraySize {
        pub fn memory_needed_and_check_literal_size(
            self,
            value: &Option<Literal>,
        ) -> Result<usize> {
            if matches!(value, Some(Literal::Int(_))) {
                e!(ArrayInitializedWithScalar)
            }
            Ok(match self {
                Self::Inferred => match value {
                    None => e!(ArrayOfInexactSizeHasNoInitializer),
                    Some(v) => v.memory_needed(),
                },
                Self::Exact(exact_size) => match value {
                    None => exact_size,
                    Some(literal) => {
                        if literal.memory_needed() == exact_size {
                            exact_size
                        } else {
                            e!(ExactInitializerInvalidLength)
                        }
                    }
                },
                Self::AtLeast(min_size) => match value {
                    None => e!(ArrayOfInexactSizeHasNoInitializer),
                    Some(literal) => {
                        let preferred_size = literal.memory_needed();
                        if preferred_size < min_size {
                            e!(InexactInitializerTooShort)
                        }
                        preferred_size
                    }
                },
            })
        }
    }

    fn init(output: &mut Output, memory: &mut Memory, pos: usize, value: impl Settable) {
        output.goto(pos);
        value.inc(output);
        if value.is_nonzero() {
            memory.cells[pos] = CellState::Unknown;
        }
    }

    impl Content {
        fn create_standard_scalar(
            output: &mut Output,
            memory: &mut Memory,
            value: &Option<Literal>,
        ) -> Result<Self> {
            let pos = memory.cells.len();
            let content = Content::Single(pos);
            match value {
                None => {}
                Some(Literal::Int(value)) => init(output, memory, pos, *value),
                Some(Literal::Str(str)) if str.len() == 1 => {
                    init(output, memory, pos, str.bytes().next().unwrap())
                }
                Some(_) => e!(ScalarInitializedWithArray),
            }

            // only allocate memory after we've thrown all possible errors
            memory.cells.push(CellState::Zeroed);

            Ok(content)
        }

        fn create_standard_array(
            output: &mut Output,
            memory: &mut Memory,
            size: ArraySize,
            value: &Option<Literal>,
        ) -> Result<Self> {
            let memory_needed = size.memory_needed_and_check_literal_size(&value)?;
            let pos_base = memory.cells.len();
            let pos: Vec<_> = (0..memory_needed).map(|x| pos_base + x).collect();

            match value {
                None => {}
                Some(Literal::Int(_)) => e!(ArrayInitializedWithScalar),
                Some(Literal::IntArray(array)) => {
                    for (i, value) in array.iter().enumerate() {
                        init(output, memory, pos[i], *value)
                    }
                }
                Some(Literal::Str(str)) => {
                    for (i, value) in str.bytes().enumerate() {
                        init(output, memory, pos[i], value)
                    }
                }
            }

            // only allocate memory after we've thrown all possible errors
            for _ in 0..memory_needed {
                memory.cells.push(CellState::Zeroed);
            }

            Ok(Content::Array(pos))
        }

        fn create_destructured(
            output: &mut Output,
            memory: &mut Memory,
            els: &[BindingInDestructure],
            accept_inexact: bool,
            value: &Option<Literal>,
        ) -> Result<Vec<usize>> {
            let memory_needed = match accept_inexact {
                true => max(
                    els.len(),
                    match &value {
                        None => 0,
                        Some(Literal::Int(_)) => e!(ArrayInitializedWithScalar),
                        Some(Literal::IntArray(x)) => x.len(),
                        Some(Literal::Str(x)) => x.len(),
                    },
                ),
                false => {
                    match &value {
                        Some(Literal::Int(_)) => e!(ArrayInitializedWithScalar),
                        Some(Literal::IntArray(x)) if x.len() != els.len() => {
                            e!(ExactInitializerInvalidLength)
                        }
                        Some(Literal::Str(x)) if x.len() != els.len() => {
                            e!(ExactInitializerInvalidLength)
                        }
                        _ => {}
                    };
                    els.len()
                }
            };
            let pos_base = memory.cells.len();
            let pos: Vec<_> = (0..memory_needed).map(|x| pos_base + x).collect();
            let mut is_init: Vec<_> = (0..memory_needed).map(|_| false).collect();

            match &value {
                None => {}
                Some(Literal::Int(_)) => e!(ArrayInitializedWithScalar),
                Some(Literal::IntArray(array)) => {
                    for (i, value) in array.iter().enumerate() {
                        init(output, memory, pos[i], *value);
                        is_init[i] = true;
                    }
                }
                Some(Literal::Str(str)) => {
                    for (i, value) in str.bytes().enumerate() {
                        init(output, memory, pos[i], value);
                        is_init[i] = true;
                    }
                }
            }

            for i in 0..memory_needed {
                if !is_init[i] {
                    let el = &els[i];
                    if let BindingInDestructure::Named {
                        default: Some(value),
                        ..
                    } = el
                    {
                        init(output, memory, pos[i], *value);
                        is_init[i] = true;
                    }
                }
            }

            if value.is_some()
                && (0..memory_needed).any(|i| match els[i] {
                    BindingInDestructure::Ignored => false,
                    BindingInDestructure::Named { .. } => !is_init[i],
                })
            {
                e!(DestructuredElementLeftUninitialized)
            }

            // only allocate memory after we've thrown all possible errors
            for _ in 0..memory_needed {
                memory.cells.push(CellState::Zeroed);
            }

            Ok(pos)
        }
    }

    pub fn stmt_let<'a>(
        output: &mut Output,
        memory: &mut Memory,
        locals: &'a mut Locals,
        Let {
            mutable,
            binding,
            value,
        }: &Let,
    ) -> Result<&'a Local> {
        let mutable = *mutable;
        Ok(match binding {
            Binding::Standard { name, kind } => {
                let content = match kind {
                    Kind::Scalar => Content::create_standard_scalar(output, memory, value),
                    Kind::Array(size) => {
                        Content::create_standard_array(output, memory, *size, value)
                    }
                }?;
                let local = Local { content, mutable };
                locals.set(*name, local)
            }
            Binding::Destructured {
                els,
                accept_inexact,
            } => {
                let pos =
                    Content::create_destructured(output, memory, &els, *accept_inexact, value)?;
                for (i, el) in els.into_iter().enumerate() {
                    if let BindingInDestructure::Named { name, .. } = el {
                        locals.set(
                            *name,
                            Local {
                                content: Content::Single(pos[i]),
                                mutable,
                            },
                        );
                    }
                }
                locals.hold(Local {
                    content: Content::Array(pos),
                    mutable,
                })
            }
        })
    }
}

mod error {
    macro_rules! error {
        (
            $(
                #[doc = $msga:literal]
                $(#[doc = $msg:literal])*
                $name:ident,
            )*
        ) => {
            #[derive(Clone, Debug)]
            pub enum Error {
                $(#[doc = $msga] $(#[doc = $msg])* $name,)*
            }

            impl Display for Error {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_str(match self {
                        $(Self::$name => $msga,)*
                    })
                }
            }
        };
    }

    error! {
        /// An array with an unspecified size has no initializer
        ///
        /// ```
        /// fn main() {
        ///   let a[];
        ///   let [a b ...];
        ///   // how much memory should be allocated? it's unknown, and is thus an error
        /// }
        /// ```
        ArrayOfInexactSizeHasNoInitializer,
        /// An inexact-length array's initializer does not specify enough memory
        ///
        /// ```
        /// fn main() {
        ///   let [a b c d ...] = "hi";
        ///   // should `c` and `d` be zero? it's unknown, and is thus an error
        /// }
        /// ```
        InexactInitializerTooShort,
        /// An exact-length array's initializer does not specify the correct amount of memory
        ///
        /// ```
        /// fn main() {
        ///   let a[3] = "hi";
        ///   let a[5] = [2 7];
        ///   let a[1] = [8 19 23];
        /// }
        /// ```
        ExactInitializerInvalidLength,
        /// An array was initialized with a scalar value
        ///
        /// ```
        /// fn main() {
        ///   let a[3] = 5;
        ///   // how should `a` be initialized? it's unknown, and is thus an error
        /// }
        /// ```
        ArrayInitializedWithScalar,
        /// A scalar was initialized with an array value other than a single-byte string
        ///
        /// ```
        /// fn main() {
        ///   let a = [5 3];
        ///   let a = "world";
        /// }
        /// ```
        ScalarInitializedWithArray,
        /// A destructuring pattern was executed but a cell was left without a value
        ///
        /// ```
        /// fn main() {
        ///   let [a b] = "h";
        ///   let [a b=3 c] = "hi";
        /// }
        /// ```
        DestructuredElementLeftUninitialized,
        /// An attempt was made to mutate an immutable variable
        ///
        /// ```
        /// fn main() {
        ///   let a = 2;
        ///   inc a;
        /// }
        /// ```
        CannotMutateImmutableVariable,
        /// The same function was defined multiple times
        ///
        /// ```
        /// fn hi() {}
        /// fn hi() {}
        /// ```
        DuplicateFunctionDefinition,
        /// There was no main function in the top-level script
        ///
        /// ```
        /// // so empty...
        /// ```
        MainDoesNotExist,
        /// The main function is not allowed to take parameters
        ///
        /// ```
        /// fn main(a, b, [c d]) {}
        /// ```
        MainTakesParameters,
        /// The main function is not allowed to return a value
        ///
        /// ```
        /// fn main() -> 23 {}
        /// ```
        MainReturns,
        /// A function was called with `_` as an argument but no default value was present
        ///
        /// ```
        /// fn main() {
        ///   inc _;
        ///   add _ 3;
        /// }
        ///
        /// fn add(mut a, mut b) -> a {
        ///   while b {
        ///     dec b;
        ///     inc a;
        ///   }
        /// }
        /// ```
        NoDefaultValue,
        /// A local does not exist with the given name
        ///
        /// ```
        /// fn main() {
        ///   let a = 23;
        ///   inc c;
        /// }
        /// ```
        LocalDoesNotExist,
        /// Cannot index into a single
        ///
        /// ```
        /// fn main() {
        ///   let a = 23;
        ///   inc a.3;
        /// }
        /// ```
        IndexedIntoSingle,
        /// Attempted to index outside of an array's bounds
        ///
        /// ```
        /// fn main() {
        ///   let a[] = [2 4];
        ///   inc a.3;
        /// }
        /// ```
        IndexOutOfBounds,
    }

    pub type Result<T> = std::result::Result<T, Error>;

    macro_rules! e {
        ($x:ident) => {
            return ::std::result::Result::Err($crate::rcr::emit::error::Error::$x)
        };
    }

    use std::fmt::Display;

    pub(super) use e;
}

#[derive(Debug)]
struct State<'a> {
    pub output: Output,
    pub memory: Memory,
    pub locals: Locals,
    pub scope: Scope<'a>,
}

fn stmt_call(state: &mut State, stmt: &Call) -> Result<Option<Target>> {
    match stmt.name {
        FnName::Builtin(Builtin::Goto) => {
            todo!()
        }
        FnName::Builtin(name) => {
            let mut args = Vec::new();

            for el in stmt.args.iter() {
                let Some(el) = el else { e!(NoDefaultValue) };
                let local = exec_target(state, el)?;
                local
                    .assert_mutability(name.mutates())?
                    .each(|x| args.push(x));
            }

            match name {
                Builtin::Goto => unreachable!(),
                Builtin::Assert(cell_state) => {
                    for pos in args {
                        state.memory.assert(pos, cell_state);
                    }
                }
                Builtin::Inc => {
                    for pos in args {
                        state.output.goto(pos);
                        state.output.inc_current();
                    }
                }
                Builtin::Dec => {
                    for pos in args {
                        state.output.goto(pos);
                        state.output.dec_current();
                    }
                }
                Builtin::Read => {
                    for pos in args {
                        state.output.goto(pos);
                        state.output.read_current();
                    }
                }
                Builtin::Write => {
                    for pos in args {
                        state.output.goto(pos);
                        state.output.write_current();
                    }
                }
            }

            Ok(None)
        }
        FnName::UserDefined(_) => todo!(),
    }
}

fn exec_target(state: &mut State, target: &Target) -> Result<Local> {
    let local = match target.inner {
        TargetInner::Local(name) => match state.locals.get(name) {
            Some(x) => x,
            None => e!(LocalDoesNotExist),
        },
        _ => todo!(),
    };

    match target.index {
        None => Ok(local.clone()),
        Some(index) => local.index(index as usize),
    }
}

fn exec(state: &mut State, stmts: &[Statement]) -> Result<()> {
    let original_memory_len = state.memory.len();

    for stmt in stmts {
        match stmt {
            Statement::Let(stmt) => {
                stmt_let(
                    &mut state.output,
                    &mut state.memory,
                    &mut state.locals,
                    stmt,
                )?;
            }
            Statement::Call(stmt) => {
                stmt_call(state, stmt)?;
            }
            _ => todo!(),
        }
    }

    state.memory.clear(&mut state.output, original_memory_len..);
    Ok(())
}

pub fn emit(parse: &ParseTree) -> Result<String> {
    let scope = Scope::new(&parse.fns)?;

    let Some(main) = &parse.names.get("main") else {
        e!(MainDoesNotExist)
    };
    let Some(main) = scope.get(main) else {
        e!(MainDoesNotExist)
    };
    if !main.args.is_empty() || main.rest.is_some() {
        e!(MainTakesParameters)
    }
    if main.returns.is_some() {
        e!(MainReturns)
    }

    let mut state = State {
        output: Output::default(),
        locals: Locals::default(),
        memory: Memory::default(),
        scope: scope.child(&main.body.fns)?,
    };

    exec(&mut state, &main.body.stmts).map(|_| state.output.into_data())
}
