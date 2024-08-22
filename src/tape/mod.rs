mod fixed;

pub use fixed::*;

use std::ops::DerefMut;

pub trait Tape: DerefMut {
    fn shift(&mut self, amount: isize);
}
