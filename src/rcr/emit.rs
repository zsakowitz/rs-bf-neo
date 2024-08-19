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

    impl Output {
        pub fn goto(&mut self, pos: usize) {
            if pos < self.pos {
                self.data += &"<".repeat(self.pos - pos);
            } else {
                self.data += &">".repeat(pos - self.pos);
            }
            self.pos = pos;
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
    use std::collections::HashMap;

    use crate::{
        builder::CellState,
        rcr::{
            emit::{Output, Result},
            syntax::{Kind, Let, Literal, Name},
        },
    };

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
    pub struct Locals {
        states: HashMap<usize, CellState>,
        named: HashMap<Name, Local>,
        unnamed: Vec<Local>,
        next: usize,
    }

    impl Content {
        fn create(output: &mut Output, kind: Kind, value: Option<Literal>) -> Result<Self> {
            let memory_needed = match kind {};
        }
    }
}

mod error {
    #[derive(Clone, Debug)]
    pub enum Error {}

    pub type Result<T> = std::result::Result<T, Error>;

    macro_rules! e {
        ($x:ident) => {
            return Error::$x
        };
    }

    pub(super) use e;
}
