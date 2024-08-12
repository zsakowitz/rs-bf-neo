use std::ops::{AddAssign, MulAssign, SubAssign};

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

    pub fn is_nonzero(&self) -> Cell<'a> {
        let mut is_nonzero: Cell = self.builder.zeroed();
        self.if_nonzero(|| {
            is_nonzero.inc();
        });
        is_nonzero
    }

    /// Returns `1` if the current cell is zero, and `0` otherwise.
    pub fn is_zero(&self) -> Cell<'a> {
        let mut is_zero: Cell = self.builder.zeroed();
        is_zero.inc();
        self.if_nonzero(|| {
            is_zero.dec();
        });
        is_zero
    }

    pub fn if_nonzero(&self, f: impl FnOnce()) {
        let mut is_nonzero = self.clone();
        is_nonzero.while_nonzero_mut(|is_nonzero| {
            is_nonzero.zero();
            f();
        });
    }

    /// This cell will be zero after this function returns.
    pub fn if_nonzero_consuming(&mut self, f: impl FnOnce()) {
        self.while_nonzero_mut(|this| {
            this.zero();
            f();
        });
    }

    pub fn if_zero(&self, f: impl FnOnce()) {
        self.is_zero().if_nonzero_consuming(f)
    }

    /// Calls the first function when the current cell is zero, and the second if it is nonzero.
    pub fn if_zero_else_if_nonzero(&self, f_is_zero: impl FnOnce(), f_is_nonzero: impl FnOnce()) {}
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

macro_rules! arithmetic {
    {
        $trait:ident::$method:ident();

        fn $name:ident(& $lifetime:lifetime mut $self:ident, &mut $rhs:ident) {
            $($body:stmt;)+
        }
    } => {
        impl<$lifetime> Cell<$lifetime> {
            #[allow(redundant_semicolons)]
            pub fn $name(&mut $self, $rhs: &mut Cell<$lifetime>) {
                $($body;)+
            }
        }

        impl<'a> $trait<Cell<'a>> for Cell<'a> {
            fn $method(&mut self, mut rhs: Cell<'a>) {
                self.$name(&mut rhs);
            }
        }
    };
}

arithmetic! {
    AddAssign::add_assign();

    fn add_assign_and_zero_rhs(&'a mut self, &mut rhs) {
        rhs.while_nonzero_mut(|rhs| {
            rhs.dec();
            self.inc();
        });
    }
}

arithmetic! {
    SubAssign::sub_assign();

    fn sub_assign_and_zero_rhs(&'a mut self, &mut rhs) {
        rhs.while_nonzero_mut(|rhs| {
            rhs.dec();
            self.dec();
        });
    }
}

arithmetic! {
    MulAssign::mul_assign();

    fn mul_assign(&'a mut self, &mut rhs) {
        let [mut iter] = self.zero_and_duplicate();
        let mut a: Cell = self.builder.zeroed();
        iter.while_nonzero_mut(|iter| {
            iter.dec();
            rhs.while_nonzero_mut(|rhs| {
                rhs.dec();
                a.inc();
            });
            a.while_nonzero_mut(|a| {
                a.dec();
                self.inc();
                rhs.inc();
            });
        });
    }
}
