use super::Tape;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug)]
pub struct FixedLengthTape<const N: usize, T> {
    data: [T; N],
    index: usize,
}

impl<const N: usize, T: Default> Default for FixedLengthTape<N, T>
where
    [T; N]: Default,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
            index: 0,
        }
    }
}

impl<const N: usize, T: Default> FixedLengthTape<N, T>
where
    T: Default + Copy,
{
    pub fn new() -> Self {
        Self {
            data: [Default::default(); N],
            index: 0,
        }
    }
}

impl<const N: usize, T> Deref for FixedLengthTape<N, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data[self.index]
    }
}

impl<const N: usize, T> DerefMut for FixedLengthTape<N, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data[self.index]
    }
}

impl<const N: usize, T> Tape for FixedLengthTape<N, T> {
    fn shift(&mut self, amount: isize) {
        let new_index = self
            .index
            .checked_add_signed(amount)
            .expect("attempted to go beyond the edges of a fixed length tape");

        if new_index >= N {
            panic!("attempted to go beyond the edges of a fixed length tape");
        }

        self.index = new_index;
    }
}
