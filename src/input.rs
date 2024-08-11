use std::{
    io::{self, Read},
    marker::PhantomData,
};

#[derive(Clone, Copy, Debug)]
pub struct NoInput<T>(PhantomData<T>);

impl<T> Default for NoInput<T> {
    fn default() -> Self {
        NoInput::NEW
    }
}

impl<T> NoInput<T> {
    pub const NEW: NoInput<T> = NoInput(PhantomData);

    pub fn new() -> Self {
        Self::NEW
    }
}

impl<T> Iterator for NoInput<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

#[derive(Debug)]
pub struct Stdin(io::Bytes<io::Stdin>);

impl Default for Stdin {
    fn default() -> Self {
        Self::new()
    }
}

impl Stdin {
    pub fn new() -> Self {
        Self(io::stdin().bytes())
    }
}

impl Iterator for Stdin {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.0.next()?.unwrap_or_default())
    }
}
