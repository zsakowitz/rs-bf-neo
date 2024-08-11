use std::ops::{AddAssign, SubAssign};

use crate::{
    builder::{Cell, EnsureZeroed},
    step::Step,
};

impl<'a> Cell<'a> {
    pub fn inc(&mut self) {
        self.inc_internal();
    }

    pub fn dec(&mut self) {
        self.dec_internal();
    }

    pub fn read(&mut self) {
        self.read_internal();
    }

    pub fn zero(&mut self) {
        self.zero_internal();
    }

    pub fn inc_by(&mut self, amount: isize) {
        if amount < 0 {
            for _ in amount..0 {
                self.dec();
            }
        } else if amount > 0 {
            for _ in 0..amount {
                self.inc();
            }
        }
    }

    pub fn dec_by(&mut self, amount: isize) {
        if amount < 0 {
            for _ in amount..0 {
                self.inc();
            }
        } else if amount > 0 {
            for _ in 0..amount {
                self.dec();
            }
        }
    }

    pub fn zero_and_add_into(&mut self, other: &mut Cell<'a>) {
        self.while_nonzero_mut(|this| {
            this.dec();
            other.inc();
        });
    }

    pub fn zero_and_add_into_many<const M: usize>(&mut self, other: [&mut Cell<'a>; M]) {
        self.while_nonzero_mut(|this| {
            this.dec();
            for other in other {
                other.inc();
            }
        });
    }

    pub fn zero_and_sub_from(&mut self, other: &mut Cell<'a>) {
        self.while_nonzero_mut(|this| {
            this.dec();
            other.dec();
        });
    }

    pub fn zero_and_duplicate<const M: usize>(&mut self) -> [Cell<'a>; M] {
        let mut new: [Cell<'a>; M] = self.builder.zeroed();
        self.while_nonzero_mut(|this| {
            this.dec();
            for other in &mut new {
                other.inc();
            }
        });
        new
    }
}

impl<'a> Clone for Cell<'a> {
    fn clone(&self) -> Self {
        let [mut returned, mut temp] = self.builder.zeroed();
        self.while_nonzero(|| {
            self.dec_internal();
            returned.inc();
            temp.inc();
        });
        temp.while_nonzero_mut(|temp| {
            temp.dec();
            self.inc_internal();
        });
        returned
    }
}

impl<'a> EnsureZeroed for Cell<'a> {
    fn ensure_zeroed(&mut self) {
        self.zero();
    }
}

// arithmetic assigning with primitives

impl<'a, T> AddAssign<T> for Cell<'a>
where
    T: Step,
{
    fn add_assign(&mut self, rhs: T) {
        self.inc_by(rhs.into_isize());
    }
}

impl<'a, T> SubAssign<T> for Cell<'a>
where
    T: Step,
{
    fn sub_assign(&mut self, rhs: T) {
        self.dec_by(rhs.into_isize());
    }
}

// arithmetic assigning with cells

impl<'a> AddAssign<Cell<'a>> for Cell<'a> {
    fn add_assign(&mut self, mut rhs: Cell<'a>) {
        rhs.while_nonzero_mut(|other| {
            other.dec();
            self.inc();
        });
    }
}

impl<'a> SubAssign<Cell<'a>> for Cell<'a> {
    fn sub_assign(&mut self, mut rhs: Cell<'a>) {
        rhs.while_nonzero_mut(|other| {
            other.dec();
            self.dec();
        });
    }
}
