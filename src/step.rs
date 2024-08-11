use std::num::{Saturating, Wrapping};

pub trait Step: Sized {
    fn zero() -> Self;
    fn is_zero(self) -> bool;
    fn inc_by(self, amount: i8) -> Self;

    fn from_isize(self, amount: i8) -> Self {
        Self::zero().inc_by(amount)
    }
}

macro_rules! impl_step {
    ($x:ty, $zero:expr, $one:expr, $self:ident, $amount:ident, $add:expr) => {
        impl Step for $x {
            fn zero() -> Self {
                $zero
            }

            fn is_zero($self) -> bool {
                $self == $zero
            }

            fn inc_by($self, $amount: i8) -> Self {
                $add
            }
        }
    };
}

macro_rules! impl_step_all_u {
    ($x:ident) => {
        impl_step!(
            $x,
            0,
            1,
            self,
            amount,
            self.checked_add_signed(amount.into())
                .expect("overflows are not allowed")
        );
        impl_step!(
            Wrapping<$x>,
            Wrapping(0),
            Wrapping(1),
            self,
            amount,
            Wrapping(self.0.wrapping_add_signed(amount.into()))
        );
        impl_step!(
            Saturating<$x>,
            Saturating(0),
            Saturating(1),
            self,
            amount,
            Saturating(self.0.saturating_add_signed(amount.into()))
        );
    };
}

macro_rules! impl_step_all_each_u {
    ($($x:ident),+) => {
        $(impl_step_all_u!($x);)+
    };
}

impl_step_all_each_u!(u8, u16, u32, u64, u128, usize);

macro_rules! impl_step_all_i {
    ($x:ident) => {
        impl_step!(
            $x,
            0,
            1,
            self,
            amount,
            self + <i8 as Into<Self>>::into(amount)
        );
        impl_step!(
            Wrapping<$x>,
            Wrapping(0),
            Wrapping(1),
            self,
            amount,
            Wrapping(self.0.wrapping_add(amount.into()))
        );
        impl_step!(
            Saturating<$x>,
            Saturating(0),
            Saturating(1),
            self,
            amount,
            Saturating(self.0.saturating_add(amount.into()))
        );
    };
}

macro_rules! impl_step_all_each_i {
    ($($x:ident),+) => {
        $(impl_step_all_i!($x);)+
    };
}

impl_step_all_each_i!(i8, i16, i32, i64, i128, isize);

impl Step for bool {
    fn zero() -> Self {
        false
    }

    fn is_zero(self) -> bool {
        !self
    }

    fn inc_by(self, amount: i8) -> Self {
        match self {
            false => match amount {
                0 => false,
                1 => true,
                _ => panic!("changed `false` by too much"),
            },
            true => match amount {
                -1 => false,
                0 => true,
                _ => panic!("changed `true` by too much"),
            },
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

    fn inc_by(self, amount: i8) -> Self {
        Wrapping(self.0 == (amount % 2 == 0))
    }
}

impl Step for Saturating<bool> {
    fn zero() -> Self {
        Saturating(false)
    }

    fn is_zero(self) -> bool {
        !self.0
    }

    fn inc_by(self, amount: i8) -> Self {
        Saturating(match self.0 {
            false => match amount {
                1.. => true,
                _ => false,
            },
            true => match amount {
                ..=-1 => false,
                _ => true,
            },
        })
    }
}
