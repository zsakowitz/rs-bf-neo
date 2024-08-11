use std::num::{Saturating, Wrapping};

pub trait Step: Sized {
    fn zero() -> Self;
    fn is_zero(self) -> bool;

    fn inc_by(self, amount: i8) -> Self;

    fn inc_by_isize(mut self, mut amount: isize) -> Self {
        if let Ok(v) = i8::try_from(amount) {
            return self.inc_by(v);
        }

        if amount < 0 {
            while amount < -128 {
                amount -= -128;
                self = self.inc_by(-128);
            }
            self.inc_by(
                amount
                    .try_into()
                    .expect("amount is greater than or equal to -128"),
            )
        } else {
            while amount > 127 {
                amount -= 127;
                self = self.inc_by(127);
            }
            self.inc_by(
                amount
                    .try_into()
                    .expect("amount is less than or equal to 127"),
            )
        }
    }

    fn from_i8(self, amount: i8) -> Self {
        Self::zero().inc_by(amount)
    }

    fn into_isize(self) -> isize;
}

macro_rules! impl_step {
    ($x:ty, $zero:expr, $one:expr, $self:ident, $amount:ident, $add:expr, $into_isize:expr) => {
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

            fn into_isize($self) -> isize {
                $into_isize
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
                .expect("overflows are not allowed"),
            self.try_into().expect("should fit in an isize")
        );
        impl_step!(
            Wrapping<$x>,
            Wrapping(0),
            Wrapping(1),
            self,
            amount,
            Wrapping(self.0.wrapping_add_signed(amount.into())),
            self.0.try_into().expect("should fit in an isize")
        );
        impl_step!(
            Saturating<$x>,
            Saturating(0),
            Saturating(1),
            self,
            amount,
            Saturating(self.0.saturating_add_signed(amount.into())),
            self.0.try_into().expect("should fit in an isize")
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
            self + <i8 as Into<Self>>::into(amount),
            self.try_into().expect("should fit in an isize")
        );
        impl_step!(
            Wrapping<$x>,
            Wrapping(0),
            Wrapping(1),
            self,
            amount,
            Wrapping(self.0.wrapping_add(amount.into())),
            self.0.try_into().expect("should fit in an isize")
        );
        impl_step!(
            Saturating<$x>,
            Saturating(0),
            Saturating(1),
            self,
            amount,
            Saturating(self.0.saturating_add(amount.into())),
            self.0.try_into().expect("should fit in an isize")
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

    fn from_i8(self, amount: i8) -> Self {
        match amount {
            0 => false,
            1 => true,
            _ => panic!("value is outside `bool` bounds"),
        }
    }

    fn into_isize(self) -> isize {
        self.into()
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

    fn from_i8(self, amount: i8) -> Self {
        Wrapping(amount % 2 != 0)
    }

    fn into_isize(self) -> isize {
        self.0.into()
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

    fn from_i8(self, amount: i8) -> Self {
        match amount {
            ..=0 => Saturating(false),
            1.. => Saturating(true),
        }
    }

    fn into_isize(self) -> isize {
        self.0.into()
    }
}
