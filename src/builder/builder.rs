use std::{
    cell::{Ref, RefCell, RefMut},
    fmt::Debug,
};

use crate::program::{CompileError, Program};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum CellState {
    #[default]
    Zeroed,
    Unknown,
}

pub struct Builder {
    /// If an `Option<CellState>` is `None`, it means it's currently loaned out. If it's
    /// `Some<...>`, then the cell is not in use anywhere.
    data: RefCell<Vec<Option<CellState>>>,
    /// Sorted list of available indices, with the lowest index at the end. More indices can be
    /// appended to the beginning as the program runs.
    available: RefCell<Vec<usize>>,
    source: RefCell<String>,
    pointer: RefCell<usize>,
    is_in_loop: RefCell<bool>,
}

struct Q<T>(T);

impl Debug for Q<&[Option<CellState>]> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for x in self.0 {
            f.write_str(match *x {
                None => "_",
                Some(CellState::Zeroed) => "0",
                Some(CellState::Unknown) => "?",
            })?;
        }
        Ok(())
    }
}

impl Debug for Builder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Builder")
            .field("data", &Q(&self.data.borrow()[..]))
            .field("available", &self.available.borrow())
            .field("source", &self.source())
            .finish()
    }
}

// internal methods go here
impl Builder {
    pub fn source(&self) -> Ref<String> {
        self.source.borrow()
    }

    fn source_mut(&self) -> RefMut<String> {
        self.source.borrow_mut()
    }

    fn pointer(&self) -> RefMut<usize> {
        self.pointer.borrow_mut()
    }

    fn internal_reserve(&self) -> (usize, CellState) {
        let mut data = self.data.borrow_mut();
        let mut available = self.available.borrow_mut();

        let Some(index) = available.pop() else {
            let index = data.len();
            data.push(None);
            return (index, CellState::Zeroed);
        };

        let Some(state) = data[index] else {
            panic!("the `self.available` vector had malformed data")
        };
        data[index] = None;
        return (index, state);
    }

    fn internal_return_reserved(&self, index: usize, state: CellState) {
        let mut data = self.data.borrow_mut();
        let mut available = self.available.borrow_mut();

        if !matches!(data[index], None) {
            panic!("cannot return a cell which wasn't in use")
        }

        data[index] = Some(state);
        let point = available.partition_point(|&x| x > index);
        available.insert(point, index);
    }

    fn internal_try_reserve_block(&self, count: usize) -> (usize, Vec<CellState>) {
        if count == 0 {
            return (0, Vec::new());
        }

        if count == 1 {
            let (index, state) = self.internal_reserve();
            return (index, vec![state]);
        }

        let mut data = self.data.borrow_mut();
        let mut available = self.available.borrow_mut();

        let (index_in_available, cell_index) = 'a: {
            for (index_in_available, cell_index) in available.iter().copied().enumerate().rev() {
                if index_in_available >= count - 1
                    && cell_index.checked_add(count).is_some()
                    && data[cell_index..(cell_index + count)]
                        .iter()
                        .all(|x| x.is_some())
                {
                    break 'a (index_in_available, cell_index);
                }
            }

            // Unable to allocate using existing memory, so invent some. If the programmer wants
            // memory closer together, they should just use an increased initial capacity.

            let index = data.len();
            for _ in 0..count {
                data.push(None);
            }

            return (index, vec![CellState::Zeroed; count]);
        };

        available.drain(index_in_available + 1 - count..=index_in_available);

        let states: Vec<CellState> = data[cell_index..cell_index + count]
            .iter()
            .copied()
            .map(|x| x.expect("all allocated cell states should be Some(...)"))
            .collect();

        for i in cell_index..cell_index + count {
            data[i] = None;
        }

        return (cell_index, states);
    }
}

// public methods go here
impl Builder {
    pub fn new() -> Self {
        Self {
            data: RefCell::new(Vec::new()),
            available: RefCell::new(Vec::new()),
            source: Default::default(),
            pointer: 0.into(),
            is_in_loop: false.into(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: RefCell::new(vec![Some(CellState::Zeroed); capacity]),
            available: RefCell::new((0..capacity).rev().collect()),
            source: Default::default(),
            pointer: 0.into(),
            is_in_loop: false.into(),
        }
    }

    pub fn compile(&self) -> Result<Program, CompileError> {
        Program::new(&self.source.borrow())
    }
}

// reservation methods
impl Builder {
    /// SAFETY: the caller must ensure it does not rely on the previous value of the cell
    pub unsafe fn volatile<'a, T: Reservable<'a>>(&'a self) -> T
    where
        T::Options: Default,
    {
        T::reserve_volatile(&self, Default::default())
    }

    /// Ensures the returned cells are zeroed.
    pub fn zeroed<'a, T: Reservable<'a> + EnsureZeroed>(&'a self) -> T
    where
        T::Options: Default,
    {
        let mut data: T = unsafe { self.volatile() };
        data.ensure_zeroed();
        data
    }

    /// SAFETY: the caller must ensure it does not rely on the previous value of the cell
    pub unsafe fn volatile_of<'a, T: Reservable<'a>>(&'a self, options: T::Options) -> T {
        T::reserve_volatile(&self, options)
    }

    /// Ensures the returned cells are zeroed.
    pub fn zeroed_of<'a, T: Reservable<'a> + EnsureZeroed>(&'a self, options: T::Options) -> T {
        let mut data: T = unsafe { self.volatile_of(options) };
        data.ensure_zeroed();
        data
    }
}

pub trait EnsureZeroed {
    fn ensure_zeroed(&mut self);
}

pub trait Reservable<'a>: Sized {
    type Options;
    unsafe fn reserve_volatile(builder: &'a Builder, options: Self::Options) -> Self;
}

impl<'a> Reservable<'a> for Cell<'a> {
    type Options = ();
    unsafe fn reserve_volatile(builder: &'a Builder, _: Self::Options) -> Self {
        let (index, state) = builder.internal_reserve();

        Cell {
            builder,
            index,
            state: state.into(),
        }
    }
}

impl<'a, const M: usize> EnsureZeroed for [Cell<'a>; M] {
    fn ensure_zeroed(&mut self) {
        for cell in self {
            cell.zero();
        }
    }
}

impl<'a, const M: usize> Reservable<'a> for [Cell<'a>; M] {
    type Options = ();
    unsafe fn reserve_volatile(builder: &'a Builder, _: Self::Options) -> Self {
        let (index, states) = builder.internal_try_reserve_block(M);

        states
            .into_iter()
            .enumerate()
            .map(|(offset, state)| Cell {
                builder,
                index: index + offset,
                state: state.into(),
            })
            .collect::<Vec<_>>()
            .try_into()
            .expect("we should have reserved the correct number of cells")
    }
}

impl<'a> EnsureZeroed for Vec<Cell<'a>> {
    fn ensure_zeroed(&mut self) {
        for cell in self {
            cell.zero();
        }
    }
}

// impl<'a> Reservable<'a> for Vec<Cell<'a>> {
//     type Options = usize;

//     unsafe fn reserve_volatile(builder: &'a Builder, count: Self::Options) -> Self {
//         let (index, states) = builder.internal_try_reserve_block(count);

//         states
//             .into_iter()
//             .enumerate()
//             .map(|(offset, state)| Cell {
//                 builder,
//                 index: index + offset,
//                 state: RefCell::new(state),
//             })
//             .collect()
//     }
// }

macro_rules! tuple {
    ($($idx:tt $name:ident $comma:tt)*) => {
        impl<'a $(, $name)*> EnsureZeroed for ($($name,)*)
        where
            $($name: EnsureZeroed,)*
        {
            fn ensure_zeroed(&mut self) {
                $(self.$idx.ensure_zeroed();)*
            }
        }

        impl<'a> Reservable<'a> for ($(Cell<'a> $comma)*)
        {
            type Options = ();
            unsafe fn reserve_volatile(_builder: &'a Builder, _options: Self::Options) -> Self {
                ($(Cell::reserve_volatile(_builder, ()) $comma)*)
            }
        }
    };
}

tuple!();
tuple!(0 A,);
tuple!(0 A, 1 B,);
tuple!(0 A, 1 B, 2 C,);
tuple!(0 A, 1 B, 2 C, 3 D,);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E,);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F,);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G,);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H,);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I,);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J,);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K,);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L,);

pub struct Cell<'a> {
    pub(crate) builder: &'a Builder,
    pub(crate) index: usize,
    state: RefCell<CellState>,
}

impl<'a> Debug for Cell<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cell")
            .field("index", &self.index)
            .field("state", &self.state)
            .finish_non_exhaustive()
    }
}

impl<'a> Drop for Cell<'a> {
    fn drop(&mut self) {
        self.builder
            .internal_return_reserved(self.index, *self.state.borrow())
    }
}

// cell methods which require internal access go here
impl<'a> Cell<'a> {
    pub fn goto(&self) {
        let mut source = self.builder.source_mut();
        let mut pointer = self.builder.pointer();

        if *pointer < self.index {
            *source += &">".repeat(self.index - *pointer);
        } else {
            *source += &"<".repeat(*pointer - self.index);
        }

        *pointer = self.index;
    }

    pub(super) fn inc_internal(&self) {
        self.goto();
        *self.builder.source_mut() += "+";
        *self.state.borrow_mut() = CellState::Unknown;
    }

    pub(super) fn dec_internal(&self) {
        self.goto();
        *self.builder.source_mut() += "-";
        *self.state.borrow_mut() = CellState::Unknown;
    }

    pub(super) fn read_internal(&self) {
        self.goto();
        *self.builder.source_mut() += ",";
        *self.state.borrow_mut() = CellState::Unknown;
    }

    pub fn write(&self) {
        self.goto();
        *self.builder.source_mut() += ".";
    }

    pub fn while_nonzero<V>(&self, f: impl FnOnce() -> V) -> V {
        let was_in_loop = *self.builder.is_in_loop.borrow();
        *self.builder.is_in_loop.borrow_mut() = true;
        self.goto();
        *self.builder.source_mut() += "[";
        let v = f();
        self.goto();
        *self.builder.source_mut() += "]";
        self.assume_zero_internal();
        *self.builder.is_in_loop.borrow_mut() = was_in_loop;
        v
    }

    pub fn while_nonzero_mut<V>(&mut self, f: impl FnOnce(&mut Self) -> V) -> V {
        self.goto();
        *self.builder.source_mut() += "[";
        let v = f(self);
        self.goto();
        *self.builder.source_mut() += "]";
        self.assume_zero_internal();
        v
    }

    pub fn is_dirty(&self) -> bool {
        match *self.state.borrow() {
            CellState::Zeroed => false,
            CellState::Unknown => true,
        }
    }

    pub fn assume_dirty(&self) {
        *self.state.borrow_mut() = CellState::Unknown
    }

    /// Assumes this cell is zeroed. In a loop, this has no effect.
    pub(super) fn assume_zero_internal(&self) {
        if !*self.builder.is_in_loop.borrow() {
            *self.state.borrow_mut() = CellState::Zeroed
        }
    }

    /// SAFETY: the caller must ensure this cell is actually zero
    ///
    /// In a loop, this has no effect.
    pub unsafe fn assume_zero(&self) {
        self.assume_zero_internal();
    }

    pub(super) fn zero_internal(&self) {
        if *self.state.borrow() == CellState::Zeroed {
            return;
        }
        self.goto();
        *self.builder.source_mut() += "[-]";
        self.assume_zero_internal();
    }
}
