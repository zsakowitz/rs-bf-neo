use std::num::{Saturating, Wrapping};

pub trait Step {
    fn zero() -> Self;
    fn is_zero(self) -> bool;
    fn inc(self) -> Self;
    fn dec(self) -> Self;
}

macro_rules! impl_step {
    ($x:ty, $zero:expr, $one:expr) => {
        impl Step for $x {
            fn zero() -> Self {
                $zero
            }

            fn is_zero(self) -> bool {
                self == $zero
            }

            fn inc(self: Self) -> Self {
                self + $one
            }

            fn dec(self: Self) -> Self {
                self - $one
            }
        }
    };
}

macro_rules! impl_step_all {
    ($x:ident) => {
        impl_step!($x, 0, 1);
        impl_step!(Wrapping<$x>, Wrapping(0), Wrapping(1));
        impl_step!(Saturating<$x>, Saturating(0), Saturating(1));
    };
}

macro_rules! impl_step_all_each {
    ($($x:ident),+) => {
        $(impl_step_all!($x);)+
    };
}

impl_step_all_each!(u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize);

impl Step for bool {
    fn zero() -> Self {
        false
    }

    fn is_zero(self) -> bool {
        !self
    }

    fn inc(self) -> Self {
        if self {
            panic!("cannot increment `true`")
        } else {
            true
        }
    }

    fn dec(self) -> Self {
        if self {
            false
        } else {
            panic!("cannot decrement `false`")
        }
    }
}

impl Step for Wrapping<bool> {
    fn zero() -> Self {
        Wrapping(false)
    }

    fn is_zero(self) -> bool {
        !self.0
    }

    fn inc(self) -> Self {
        Wrapping(!self.0)
    }

    fn dec(self) -> Self {
        Wrapping(!self.0)
    }
}

impl Step for Saturating<bool> {
    fn zero() -> Self {
        Saturating(false)
    }

    fn is_zero(self) -> bool {
        !self.0
    }

    fn inc(self) -> Self {
        Saturating(true)
    }

    fn dec(self) -> Self {
        Saturating(false)
    }
}
