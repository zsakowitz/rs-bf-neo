use std::fmt::Debug;

use super::syntax::FnDeclaration;

#[derive(Clone, Debug, Default)]
struct Output {
    data: String,
}

impl Output {
    fn set_current_signed(
        &mut self,
        value: impl TryInto<usize, Error: Debug>
            + std::ops::Neg<Output: TryInto<usize, Error: Debug> + std::cmp::PartialOrd<i32>>
            + std::cmp::PartialOrd<i32>,
    ) {
        if value > 0 {
            self.data += &"+".repeat(value.try_into().unwrap());
        } else if value < 0 {
            self.data += &"-".repeat((-value).try_into().unwrap());
        }
    }

    fn set_current_unsigned(&mut self, value: impl Into<usize>) {
        self.data += &"+".repeat(value.try_into().unwrap());
    }
}

#[derive(Debug)]
struct Context<'a> {
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

pub enum EmitError {
    AutoSizedArrayIsMissingInitializer,
    AutoSizedArrayIsInitializedWithScalar,
    InitializedScalarFromIntArray,
    InitializedScalarFromEmptyString,
    InitializedScalarFromMultiByteString,
    InitializedArrayFromScalar,
    InitializedArrayFromIncorrectlySizedString,
    InitializedArrayFromIncorrectlySizedIntArray,
}

pub type Result<T> = std::result::Result<T, EmitError>;

impl<'a> Context<'a> {
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

impl LocalInner {
    fn as_single(&self) -> Option<&Single> {
        if let Self::Single(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_array(&self) -> Option<&Vec<Single>> {
        if let Self::Array(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

mod locals {
    use std::collections::{hash_map::Entry, HashMap};

    use crate::rcr::{
        emit::{Context, EmitError, Local, LocalInner, Result, Single},
        syntax::{Binding, Let, Literal, Name, Size},
    };

    #[derive(Clone, Debug)]
    pub struct Locals {
        locals: HashMap<Name, Local>,
        /// local which are now inaccessible due to having their names overriden
        inaccessible: Vec<Local>,
        next: isize,
    }

    impl Locals {
        /// Creates a local with the given mutability, name, and size.
        ///
        /// If name is None, the local is created but is unnamed and inaccessible.
        ///
        /// If size is None, the local is a cell. Otherwise, it is an array of cells.
        fn internal(
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
            let new = Local {
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
                Some(name) => match self.locals.entry(name) {
                    Entry::Occupied(mut entry) => {
                        let old = entry.insert(new);
                        self.inaccessible.push(old);
                        entry.into_mut()
                    }
                    Entry::Vacant(entry) => entry.insert(new),
                },
                None => {
                    self.inaccessible.push(new);
                    self.inaccessible.last_mut().expect("we just pushed it")
                }
            }
        }

        pub fn from_let_binding(
            &mut self,
            context: &mut Context,
            Let {
                mutable,
                binding,
                value,
            }: Let,
        ) -> Result<&mut Local> {
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

                    let local = self.internal(mutable, Some(name), size);

                    match size {
                        None => {
                            let single = local.inner.as_single().unwrap();
                            match value {
                                None => {
                                    // no initializer and value is already zeroed
                                }
                                Some(Literal::Int(value)) => {
                                    context.goto(single.pos);
                                    context.output.set_current_signed(value);
                                }
                                Some(Literal::IntArray(_)) => {
                                    return Err(EmitError::InitializedScalarFromIntArray)
                                }
                                Some(Literal::Str(ref str)) => {
                                    let mut b = str.bytes();
                                    match (b.next(), b.next()) {
                                        (Some(value), None) => {
                                            context.goto(single.pos);
                                            context.output.set_current_unsigned(value);
                                        }
                                        (None, _) => {
                                            return Err(EmitError::InitializedScalarFromEmptyString)
                                        }
                                        (Some(_), Some(_)) => {
                                            return Err(
                                                EmitError::InitializedScalarFromMultiByteString,
                                            )
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
                                    return Err(EmitError::InitializedArrayFromScalar);
                                }
                                Some(Literal::IntArray(ref ints)) => {
                                    if ints.len() != array.len() {
                                        return Err(
                                            EmitError::InitializedArrayFromIncorrectlySizedIntArray,
                                        );
                                    }
                                    for (index, el) in ints.iter().enumerate() {
                                        context.goto(array[index].pos);
                                        context.output.set_current_signed(*el);
                                    }
                                }
                                Some(Literal::Str(ref str)) => {
                                    if str.len() != array.len() {
                                        return Err(
                                            EmitError::InitializedArrayFromIncorrectlySizedString,
                                        );
                                    }
                                    for (index, byte) in str.bytes().enumerate() {
                                        context.goto(array[index].pos);
                                        context.output.set_current_unsigned(byte);
                                    }
                                }
                            }
                        }
                    }

                    match &mut local.inner {
                        LocalInner::Single(cell) => {}
                        LocalInner::Array(array) => {}
                    }

                    Ok(local)
                }
                Binding::Destructured {
                    els,
                    accept_inexact,
                } => todo!(),
            }
        }
    }
}
