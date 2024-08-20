use alloc::*;
use error::*;
use output::*;
use scope::*;

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

        pub fn inc_by(&mut self, value: impl Settable) {
            value.inc(self);
        }
    }
}

mod scope {
    use std::collections::HashMap;

    use crate::rcr::syntax::{FnDeclaration, Name};

    #[derive(Clone, Debug)]
    pub struct Scope<'a> {
        parent: Option<&'a Scope<'a>>,
        fns: HashMap<Name, &'a FnDeclaration>,
    }

    impl<'a> Scope<'a> {
        fn create_hash_map(slice: &'a [FnDeclaration]) -> HashMap<Name, &'a FnDeclaration> {
            let mut fns = HashMap::new();
            for el in slice {
                fns.insert(el.name, el);
            }
            fns
        }

        pub fn new(fns: &'a [FnDeclaration]) -> Self {
            Self {
                parent: None,
                fns: Self::create_hash_map(fns),
            }
        }

        pub fn child(&'a self, next: &'a [FnDeclaration]) -> Self {
            Self {
                parent: Some(self),
                fns: Self::create_hash_map(next),
            }
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
    use crate::{
        builder::CellState,
        rcr::{
            emit::{e, Output, Result, Settable},
            syntax::{ArraySize, Binding, BindingInDestructure, Kind, Let, Literal, Name},
        },
    };
    use std::{cmp::max, collections::HashMap};

    #[derive(Clone, Debug)]
    pub enum Content {
        Single(usize),
        Array(Vec<usize>),
    }

    #[derive(Clone, Debug)]
    pub struct Local {
        content: Content,
        mutable: bool,
    }

    #[derive(Clone, Debug, Default)]
    pub struct Memory {
        cells: Vec<CellState>,
    }

    #[derive(Clone, Debug, Default)]
    pub struct Locals {
        named: HashMap<Name, Local>,
        unnamed: Vec<Local>,
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
            value: Option<Literal>,
        ) -> Result<Self> {
            let pos = memory.cells.len();
            let content = Content::Single(pos);
            match value {
                None => {}
                Some(Literal::Int(value)) => init(output, memory, pos, value),
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
            value: Option<Literal>,
        ) -> Result<Self> {
            let memory_needed = size.memory_needed_and_check_literal_size(&value)?;
            let pos_base = memory.cells.len();
            let pos: Vec<_> = (0..memory_needed).map(|x| pos_base + x).collect();

            match value {
                None => {}
                Some(Literal::Int(_)) => e!(ArrayInitializedWithScalar),
                Some(Literal::IntArray(array)) => {
                    for (i, value) in array.into_iter().enumerate() {
                        init(output, memory, pos[i], value)
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
            value: Option<Literal>,
        ) -> Result<Self> {
            let memory_needed = max(
                els.len(),
                match &value {
                    None => 0,
                    Some(Literal::Int(_)) => e!(ArrayInitializedWithScalar),
                    Some(Literal::IntArray(x)) => x.len(),
                    Some(Literal::Str(x)) => x.len(),
                },
            );
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

            Ok(Content::Array(pos))
        }
    }

    pub fn stmt_let(
        output: &mut Output,
        memory: &mut Memory,
        locals: &mut Locals,
        Let {
            mutable,
            binding,
            value,
        }: Let,
    ) -> Result<Local> {
        match binding {
            Binding::Standard {
                name,
                kind: Kind::Scalar,
            } => {
                let content = Content::create_standard_scalar(output, memory, value);
                todo!()
            }
            Binding::Standard {
                name,
                kind: Kind::Array(array_size),
            } => todo!(),
            Binding::Destructured {
                els,
                accept_inexact,
            } => todo!(),
        }
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
