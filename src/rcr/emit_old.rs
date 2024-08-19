use super::syntax::FnDeclaration;
use crate::rcr::syntax::Name;
use crate::{
    builder::CellState,
    rcr::syntax::{Binding, BindingInDestructure, Let, Literal, Size, Target},
};
use std::collections::{hash_map::Entry, HashMap};
use std::fmt::Debug;

#[derive(Clone, Debug, Default)]
struct Output {
    data: String,
}

#[derive(Debug)]
struct PosTracker<'a> {
    output: &'a mut Output,
    pos: isize,
}

#[derive(Copy, Clone, Debug)]
struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    fns: &'a [FnDeclaration],
}

#[derive(Clone, Debug)]
struct Single {
    /// position relative to function call point
    pos: isize,
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

#[derive(Copy, Clone, Debug)]
pub enum EmitError {
    AutoSizedArrayIsMissingInitializer,
    AutoSizedArrayIsInitializedWithScalar,
    InitializedScalarFromIntArray,
    InitializedScalarFromEmptyString,
    InitializedScalarFromMultiByteString,
    InitializedArrayFromInt,
    InitializedArrayFromIncorrectlySizedIntArray,
    InitializedArrayFromIncorrectlySizedString,
}

pub type Result<T> = std::result::Result<T, EmitError>;

impl<'a> PosTracker<'a> {
    fn goto(&mut self, pos: isize) {
        if pos < self.pos {
            for _ in pos..self.pos {
                self.output.data += "<";
            }
        } else if pos > self.pos {
            for _ in self.pos..pos {
                self.output.data += ">";
            }
        }
        self.pos = pos;
    }

    fn init_i32(&mut self, locals: &mut Locals, pos: isize, value: i32) {
        self.goto(pos);
        if value > 0 {
            self.output.data += &"+".repeat(value.try_into().unwrap());
        } else if value < 0 {
            self.output.data += &"-".repeat((-value).try_into().unwrap());
        }
        if value != 0 {
            locals.state(pos, CellState::Unknown);
        }
    }

    fn init_u8(&mut self, locals: &mut Locals, pos: isize, value: u8) {
        self.goto(pos);
        self.output.data += &"+".repeat(value.try_into().unwrap());
        if value != 0 {
            locals.state(pos, CellState::Unknown);
        }
    }
}

fn stmt_let(
    tracker: &mut PosTracker,
    locals: &mut Locals,
    Let {
        mutable,
        binding,
        value,
    }: Let,
) -> Result<()> {
    match binding {
        Binding::Standard { name, size } => {
            let size = match size {
                Size::Scalar => None,            // let a;
                Size::Array(Some(x)) => Some(x), // let a[3];
                Size::Array(None) => Some(match value {
                    None => return Err(EmitError::AutoSizedArrayIsMissingInitializer), // let a[]
                    Some(Literal::Int(_)) => {
                        return Err(EmitError::AutoSizedArrayIsInitializedWithScalar);
                    } // let a[] = 2;
                    Some(Literal::IntArray(ref x)) => x.len(), // let a[] = [2 3];
                    Some(Literal::Str(ref x)) => x.len(),      // let a[] = "hello world";
                }),
            };

            let local = locals.internal(mutable, Some(name), size);

            match size {
                None => {
                    let mut single = local.inner.as_single().unwrap();
                    match value {
                        None => {
                            // no initializer and value is already zeroed
                        }
                        Some(Literal::Int(value)) => {
                            tracker.init_i32(&mut single, value);
                        }
                        Some(Literal::IntArray(_)) => {
                            return Err(EmitError::InitializedScalarFromIntArray)
                        }
                        Some(Literal::Str(ref str)) => {
                            let mut b = str.bytes();
                            match (b.next(), b.next()) {
                                (Some(value), None) => {
                                    tracker.init_u8(&mut single, value);
                                }
                                (None, _) => {
                                    return Err(EmitError::InitializedScalarFromEmptyString)
                                }
                                (Some(_), Some(_)) => {
                                    return Err(EmitError::InitializedScalarFromMultiByteString)
                                }
                            }
                        }
                    }
                }
                Some(_) => {
                    let array = local.inner.as_array().unwrap();
                    match value {
                        None => {
                            // no initializer and value is already zeroed
                        }
                        Some(Literal::Int(_)) => {
                            return Err(EmitError::InitializedArrayFromInt);
                        }
                        Some(Literal::IntArray(ref ints)) => {
                            if ints.len() != array.len() {
                                return Err(
                                    EmitError::InitializedArrayFromIncorrectlySizedIntArray,
                                );
                            }
                            for (index, el) in ints.iter().enumerate() {
                                tracker.init_i32(&mut array[index], *el);
                            }
                        }
                        Some(Literal::Str(ref str)) => {
                            if str.len() != array.len() {
                                return Err(EmitError::InitializedArrayFromIncorrectlySizedString);
                            }
                            for (index, byte) in str.bytes().enumerate() {
                                tracker.init_u8(&mut array[index], byte);
                            }
                        }
                    }
                }
            }
        }
        Binding::Destructured {
            els,
            accept_inexact,
        } => {
            let size = els.len();

            let mut array: Vec<_> = locals
                .internal_for_destructuring(size)
                .into_iter()
                // the value and whether it has been initialized
                .map(|x| (x, false))
                .collect();

            match value {
                None => {
                    // everything is already zeroed, so do nothing
                }
                Some(Literal::Int(_)) => return Err(EmitError::InitializedArrayFromInt),
                Some(Literal::IntArray(arr)) => {
                    if !accept_inexact && arr.len() != els.len() {
                        return Err(EmitError::InitializedArrayFromIncorrectlySizedIntArray);
                    }

                    for (index, value) in arr.into_iter().enumerate() {
                        if els.get(index).map(|x| !x.is_ignored()).unwrap_or_default() {
                            tracker.init_i32(&mut array[index].0, value);
                            array[index].1 = true;
                        }
                    }
                }
                Some(Literal::Str(str)) => {
                    if !accept_inexact && str.len() != els.len() {
                        return Err(EmitError::InitializedArrayFromIncorrectlySizedString);
                    }

                    for (index, value) in str.bytes().enumerate() {
                        if els.get(index).map(|x| !x.is_ignored()).unwrap_or_default() {
                            tracker.init_u8(&mut array[index].0, value);
                            array[index].1 = true;
                        }
                    }
                }
            }

            for (index, mut el) in array.into_iter().enumerate() {
                let BindingInDestructure::Named { name, default } = &els[index] else {
                    continue;
                };

                if !el.1 {
                    if let Some(value) = default {
                        tracker.init_i32(&mut el.0, *value);
                    }
                }

                locals.save(
                    Local {
                        inner: LocalInner::Single(el.0),
                        mutable,
                    },
                    *name,
                );
            }
        }
    }

    Ok(())
}

// fn exec_target(tracker: &mut PosTracker, locals: &mut Locals, target: Target) -> Result<()> {}

// fn stmt_while(
//     tracker: &mut PosTracker,
//     locals: &mut Locals,
//     value: Single,
//     inner: impl FnOnce(&mut PosTracker, &mut Locals) -> Result<()>,
// ) -> Result<()> {
// }

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

impl LocalInner {
    fn as_single(&mut self) -> Option<&mut Single> {
        if let Self::Single(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_array(&mut self) -> Option<&mut Vec<Single>> {
        if let Self::Array(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Locals {
    /// a map of all positions with a local and their current state
    all: HashMap<isize, CellState>,
    /// a map of all currently assigned locals
    locals: HashMap<Name, Local>,
    /// local which are now inaccessible due to having their names overriden
    inaccessible: Vec<Local>,
    /// list of locals which changed. used to track changes in a `while` loop
    changed: Option<Vec<isize>>,
    next: isize,
}

impl Locals {
    pub fn state() {}
    pub fn save(&mut self, local: Local, name: Name) -> &mut Local {
        match self.locals.entry(name) {
            Entry::Occupied(mut entry) => {
                let old = entry.insert(local);
                self.inaccessible.push(old);
                entry.into_mut()
            }
            Entry::Vacant(entry) => entry.insert(local),
        }
    }

    /// Creates a local with the given mutability, name, and size.
    ///
    /// If `name` is [`None`], the local is created but is unnamed and inaccessible.
    ///
    /// If `size` is [`None`], the local is a cell. Otherwise, it is an array of cells.
    pub fn internal(
        &mut self,
        mutable: bool,
        name: Option<Name>,
        size: Option<usize>,
    ) -> &mut Local {
        let next = self.next;
        let isize = size
            .map(|x| isize::try_from(x).expect("the size fits into an `isize`"))
            .unwrap_or(1);
        self.next += isize;
        let local = Local {
            inner: match size {
                Some(_) => LocalInner::Array(
                    (0..isize)
                        .map(|x| Single {
                            pos: next + x,
                            is_zero: true,
                        })
                        .collect(),
                ),
                None => LocalInner::Single(Single {
                    pos: next,
                    is_zero: true,
                }),
            },
            mutable,
        };
        match name {
            Some(name) => self.save(local, name),
            None => {
                self.inaccessible.push(local);
                self.inaccessible.last_mut().expect("we just pushed it")
            }
        }
    }

    /// Creates an array of locals with the given size.
    pub fn internal_for_destructuring(&mut self, size: usize) -> Vec<Single> {
        let next = self.next;
        let isize = isize::try_from(size).expect("the size fits into an `isize`");
        self.next += isize;
        (0..isize)
            .map(|x| Single {
                pos: next + x,
                is_zero: true,
            })
            .collect()
    }
}

#[cfg(test)]
#[test]
fn test() {
    use crate::rcr::syntax::Name;

    let mut output = Output::default();
    let mut tracker = PosTracker {
        output: &mut output,
        pos: 0,
    };
    let mut locals = Locals::default();

    let l = Let {
        mutable: true,
        binding: Binding::Destructured {
            els: vec![
                BindingInDestructure::Ignored,
                BindingInDestructure::Named {
                    name: Name(2),
                    default: None,
                },
                BindingInDestructure::Named {
                    name: Name(3),
                    default: Some(34),
                },
            ],
            accept_inexact: true,
        },
        value: Some(Literal::Str("Hello world".to_string())),
    };

    stmt_let(&mut tracker, &mut locals, l).unwrap();

    println!("{locals:#?}");
    println!("{output:?}");
}
