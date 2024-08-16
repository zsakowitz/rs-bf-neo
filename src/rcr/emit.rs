use std::collections::HashMap;

use crate::rcr::Name;

use super::syntax::{FnDeclaration, Statement};

#[derive(Clone, Debug, Default)]
struct Output {
    data: String,
}

#[derive(Copy, Clone, Debug)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    fns: &'a [FnDeclaration],
}

impl<'a> Scope<'a> {
    fn new(fns: &'a [FnDeclaration]) -> Self {
        Self { parent: None, fns }
    }

    fn child(&'a self, fns: &'a [FnDeclaration]) -> Scope {
        Scope {
            parent: Some(self),
            fns,
        }
    }
}

#[derive(Clone, Debug)]
struct Single {
    /// position relative to function call point
    pos: isize,
    /// whether this local is definitely zero
    ///
    /// if false, the local may be zero, or it may be something else
    is_zero: bool,
}

#[derive(Clone, Debug)]
enum LocalInner {
    Single(Single),
    Array(Vec<Single>),
}

#[derive(Clone, Debug)]
struct Local {
    inner: LocalInner,
    mutable: bool,
}

#[derive(Clone, Debug)]
struct Locals {
    locals: HashMap<Name, Local>,
    /// if the same name is used twice and the local was allocated in this function, we store it for
    /// later so that we can zero it properly
    inaccessible: Vec<Local>,
    next: isize,
}

impl Locals {
    fn create_single(&mut self, name: Name, mutable: bool) -> &Local {
        let next = self.next;
        self.next += 1;
        let local = Local {
            inner: LocalInner::Single(Single {
                pos: next,
                is_zero: true,
            }),
            mutable,
        };
        todo!()
        // self.locals.entry(name)
    }
}
