use crate::rcr::{
    emit::{
        alloc::{stmt_let, Content, Local, Locals, Memory},
        error::{e, Error, Result},
        output::Output,
        scope::Scope,
    },
    syntax::{
        Builtin, Call, FnName, For, ParseTree, Script, Statement, Target, TargetInner, While,
    },
};

mod output {
    use crate::rcr::{
        emit::error::{e, Result},
        syntax::Offset,
    };

    #[derive(Clone, Debug, Default)]
    pub struct Output {
        data: String,
        pos: usize,
    }

    impl Output {
        pub fn start_loop(&mut self) {
            self.data += "[";
        }

        pub fn end_loop(&mut self) {
            self.data += "]";
        }

        pub fn pos_by_offset(&self, offset: Offset) -> Result<usize> {
            match self.pos.checked_add_signed(offset.0) {
                Some(x) => Ok(x),
                None => e!(OffsetExitsTapeBounds),
            }
        }
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
    use crate::rcr::emit::State;
    use crate::{
        builder::CellState,
        rcr::syntax::{ArraySize, Binding, BindingInDestructure, Kind, Let, Literal, Name},
    };
    use std::mem;
    use std::ops::RangeFrom;
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
        pub fn create_single(
            output: &mut Output,
            memory: &mut Memory,
            value: impl Settable,
        ) -> Self {
            let pos = memory.create_single();
            init(output, memory, pos, value);
            Content::Single(pos)
        }

        pub fn create_array(
            output: &mut Output,
            memory: &mut Memory,
            value: &[impl Settable],
        ) -> Self {
            let pos = memory.len();
            let size = value.len();
            for _ in 0..size {
                memory.create_single();
            }
            let pos: Vec<_> = (0..size).map(|x| pos + x).collect();
            for (index, pos) in pos.iter().enumerate() {
                init(output, memory, *pos, value[index]);
            }
            Content::Array(pos)
        }

        pub fn into_local(self, mutable: bool) -> Local {
            Local {
                content: self,
                mutable,
            }
        }

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
        pub content: Content,
        pub mutable: bool,
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
        /// in a block of possibly executed code, this stores the state of any cell before it was
        /// modified. if a cell is zero in both branches, it is zero after the possible execution.
        /// if a cell is nonzero in either branch, it is nonzero after the possible execution.
        previous: Option<HashMap<usize, CellState>>,
    }

    impl Memory {
        fn create_single(&mut self) -> usize {
            let pos = self.cells.len();
            // if let Some(ref mut x) = self.previous {
            //     x.entry(pos).or_insert(CellState::Zeroed);
            // }
            self.cells.push(CellState::Zeroed);
            pos
        }

        fn set_state(&mut self, pos: usize, state: CellState) {
            assert!(pos <= self.cells.len(), "pos exists in the map");
            if let Some(ref mut x) = self.previous {
                x.entry(pos).or_insert(self.cells[pos]);
            }
            self.cells[pos] = state;
        }

        pub fn len(&self) -> usize {
            self.cells.len()
        }

        pub fn assert(&mut self, pos: usize, state: CellState) {
            self.set_state(pos, state);
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

        /// Assumes any changes to local in the function `f` may not happen.
        /// This is used to emit `while {}` constructs.
        pub fn possibly<T>(state: &mut State, f: impl FnOnce(&mut State) -> T) -> T {
            let prev_map = state.memory.previous.replace(HashMap::new());
            let result = f(state);
            // we put it there so it should still exist
            let this_map = mem::replace(&mut state.memory.previous, prev_map).unwrap();
            for (index, old_state) in this_map {
                let new_state = state.memory.cells.get(index).copied();

                state.memory.cells[index] = match (old_state, new_state) {
                    (CellState::Zeroed, Some(CellState::Zeroed)) => CellState::Zeroed,
                    (CellState::Zeroed, None) => CellState::Zeroed,
                    (_, _) => CellState::Unknown,
                };
            }
            result
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

        pub fn remove(&mut self, name: Name) -> Option<Local> {
            self.named.remove(&name)
        }

        pub fn set(&mut self, name: Name, local: Local) -> &Local {
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
            memory.set_state(pos, CellState::Unknown);
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
            memory.create_single();

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
                memory.create_single();
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
                memory.create_single();
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
        /// Offset targets cannot go beyond the edges of memory
        ///
        /// ```
        /// fn main() {
        ///   inc <<<;
        ///   // what cell does this point to? it doesn't exist, and thus errors
        /// }
        /// ```
        OffsetExitsTapeBounds,
        /// A while loop must be headed by a single cell
        ///
        /// ```
        /// fn main() {
        ///   let a[2] = [2 3];
        ///
        ///   while a {
        ///     // which cell should be taken as the condition? unknown, and thus an error
        ///   }
        /// }
        /// ```
        WhileLoopHeadedByArray,
        /// Cannot iterate over a scalar value
        ///
        /// ```
        /// fn main() {
        ///   for a in 34 {
        ///     // what value should `a` have? unknown, and thus an error
        ///   }
        /// }
        /// ```
        IterationOverScalar,
        /// Array elements must be scalars
        ///
        /// ```
        /// fn main() {
        ///   let a[] = [2 3 4];
        ///   let b[] = [a 7];
        /// }
        /// ```
        ArrayElementsMustBeScalars,
        /// A while loop was used as a target
        ///
        /// ```
        /// fn main() {
        ///   mut a = 2;
        ///   let b = (while a { dec a; });
        ///   // while loops do not return values and cannot be used as targets
        /// }
        /// ```
        TargetedWhileLoop,
        /// A for loop was used as a target
        ///
        /// ```
        /// fn main() {
        ///   mut a[] = [2 3];
        ///   let b = (for mut x in a { inc x; });
        ///   // for loops do not return values and cannot be used as targets
        /// }
        /// ```
        TargetedForLoop,
        /// An empty block was used as a target
        ///
        /// This should never occur in a regular program.
        TargetedEmptyBlock,
        /// A function without a return target was used as a target
        ///
        /// ```
        /// fn zero(mut x) {
        ///   while x {
        ///     dec x;
        ///   }
        /// }
        ///
        /// fn main() {
        ///   mut a = 2;
        ///   let b = (inc a);
        ///   let c = (zero a);
        ///   // `inc` and `zero` do not return values and cannot be used as targets
        /// }
        /// ```
        TargetedFunctionWithoutReturn,
    }

    pub type Result<T> = std::result::Result<T, Error>;

    macro_rules! e {
        ($x:ident) => {
            return ::std::result::Result::Err($crate::rcr::emit::error::Error::$x)
        };
    }

    macro_rules! ex {
        ($x:ident) => {
            ::std::result::Result::Err($crate::rcr::emit::error::Error::$x)
        };
    }

    use std::fmt::Display;

    pub(super) use {e, ex};
}

#[derive(Debug)]
struct State<'output, 'memory, 'locals, 'scope> {
    pub output: &'output mut Output,
    pub memory: &'memory mut Memory,
    pub locals: &'locals mut Locals,
    pub scope: &'scope Scope<'scope>,
}

/// Return values:
///
/// - `Ok(Ok(local))` means the script executed successfully and returned `local`
/// - `Ok(Err(e))` means the script executed successfully but returned no local because `e`
/// - `Err(e)` means the script failed to execute because `e`
fn stmt_call(state: &mut State, stmt: &Call) -> Result<Result<Local>> {
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

            Ok(ex!())
        }
        FnName::UserDefined(_) => todo!(),
    }
}

fn stmt_while(state: &mut State, stmt: &While) -> Result<()> {
    let local = exec_target(state, &stmt.target)?;
    let pos = match local.content {
        Content::Single(x) => x,
        Content::Array(_) => e!(WhileLoopHeadedByArray),
    };
    state.output.goto(pos);
    state.output.start_loop();
    Memory::possibly(state, |state| exec_block(state, &stmt.body))?;
    state.output.goto(pos);
    state.output.end_loop();
    Ok(())
}

fn stmt_for(state: &mut State, stmt: &For) -> Result<()> {
    let local = exec_target(state, &stmt.array)?;
    let content = local.assert_mutability(stmt.mutable)?;
    let pos = match content {
        Content::Single(_) => e!(IterationOverScalar),
        Content::Array(x) => x,
    };
    let prev = state.locals.remove(stmt.bound);
    for pos in pos {
        let local = Content::Single(*pos).into_local(stmt.mutable);
        state.locals.set(stmt.bound, local);
        exec_block(state, &stmt.body)?;
        // TODO: ensure zero-tracking is valid in for loops
    }
    if let Some(x) = prev {
        state.locals.set(stmt.bound, x);
    }
    Ok(())
}

fn exec_target(state: &mut State, target: &Target) -> Result<Local> {
    let local = match target.inner {
        TargetInner::Local(name) => match state.locals.get(name) {
            Some(x) => x.clone(),
            None => e!(LocalDoesNotExist),
        },
        TargetInner::Int(value) => {
            Content::create_single(&mut state.output, &mut state.memory, value).into_local(true)
        }
        TargetInner::Str(ref value) => {
            Content::create_array(&mut state.output, &mut state.memory, value.as_bytes())
                .into_local(true)
        }
        TargetInner::Relative(offset) => {
            Content::Single(state.output.pos_by_offset(offset)?).into_local(false)
        }
        TargetInner::Array(ref block) => {
            let mut pos = Vec::new();
            let mut mutable = true;
            for target in block {
                let local = exec_target(state, target)?;
                if !local.mutable {
                    mutable = false;
                }
                match local.content {
                    Content::Single(x) => pos.push(x),
                    Content::Array(_) => e!(ArrayElementsMustBeScalars),
                }
            }
            Content::Array(pos).into_local(mutable)
        }
        TargetInner::Expr(ref block) => exec_block(state, script),
        _ => todo!(),
    };

    match target.index {
        None => Ok(local),
        Some(index) => local.index(index as usize),
    }
}

// fn exec(state: &mut State, stmts: &[Statement]) -> Result<Option<Local>> {
//     let original_memory_len = state.memory.len();
//     // exec stmts
//     state.memory.clear(&mut state.output, original_memory_len..);
//     Ok(())
// }

/// Return values:
///
/// - `Ok(Ok(local))` means the script executed successfully and returned `local`
/// - `Ok(Err(e))` means the script executed successfully but returned no local because `e`
/// - `Err(e)` means the script failed to execute because `e`
fn exec_block(state: &mut State, script: &Script) -> Result<Result<Local>> {
    let state = &mut State {
        locals: state.locals,
        memory: state.memory,
        output: state.output,
        scope: &state.scope.child(&script.fns)?,
    };

    let mut output = Err(Error::TargetedEmptyBlock);

    for stmt in &script.stmts {
        output = match stmt {
            Statement::Let(stmt) => Ok(stmt_let(
                &mut state.output,
                &mut state.memory,
                &mut state.locals,
                stmt,
            )?
            .clone()),
            Statement::Call(stmt) => stmt_call(state, stmt)?,
            Statement::While(stmt) => {
                stmt_while(state, stmt)?;
                None
            }
            Statement::For(stmt) => {
                stmt_for(state, stmt)?;
                None
            }
        }
    }

    Ok(output)
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

    let mut output = Output::default();

    let mut state = State {
        output: &mut output,
        locals: &mut Locals::default(),
        memory: &mut Memory::default(),
        scope: &scope,
    };

    exec_block(&mut state, &main.body).map(|_| output.into_data())
}
