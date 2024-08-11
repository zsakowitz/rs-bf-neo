mod fixed;

pub use fixed::*;

use std::ops::DerefMut;

pub trait Tape<T>: DerefMut<Target = T> {
    fn shift(&mut self, amount: isize);
}
