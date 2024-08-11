use std::{cell::RefCell, fmt::Debug, marker::PhantomData, mem::forget, ops::Index};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum CellState {
    #[default]
    Zeroed,
    Unknown,
}

pub struct Builder<const N: usize> {
    /// If an `Option<CellState>` is `None`, it means it's currently loaned out. If it's
    /// `Some<...>`, then the cell is not in use anywhere.
    data: RefCell<[Option<CellState>; N]>,
    /// Sorted list of available indices, with the lowest index at the end.
    available: RefCell<Vec<usize>>,
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

impl<const N: usize> Debug for Builder<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let data = self.data.borrow();
        let available = self.available.borrow();

        f.debug_struct("Builder")
            .field("data", &Q(&data[..]))
            .field("available", &available)
            .finish()
    }
}

// internal methods go here
impl<const N: usize> Builder<N> {
    fn internal_try_reserve(&self) -> Option<(usize, CellState)> {
        let mut data = self.data.borrow_mut();
        let mut available = self.available.borrow_mut();

        let index = available.pop()?;
        let Some(state) = data[index] else {
            panic!("the `self.available` vector had malformed data")
        };
        data[index] = None;
        return Some((index, state));
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

    fn internal_try_reserve_block(&self, count: usize) -> Option<(usize, Vec<CellState>)> {
        if count == 0 {
            return Some((0, Vec::new()));
        }

        if count == 1 {
            return self
                .internal_try_reserve()
                .map(|(index, state)| (index, vec![state]));
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

            return None;
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

        return Some((cell_index, states));
    }

    fn internal_return_reserved_block(&self, index: usize, states: &[CellState]) {
        // TODO: optimize
        for (offset, state) in states.iter().enumerate() {
            self.internal_return_reserved(index + offset, *state);
        }
    }
}

// public methods go here
impl<const N: usize> Builder<N> {
    pub fn new() -> Self {
        Self {
            data: RefCell::new([Some(CellState::Zeroed); N]),
            available: RefCell::new((0..N).rev().collect()),
        }
    }

    pub fn try_reserve<T>(&self) -> Option<Cell<N, T>> {
        self.internal_try_reserve().map(|(index, state)| Cell {
            builder: &self,
            index,
            phantom: PhantomData,
            state,
        })
    }

    pub fn reserve<T>(&self) -> Cell<N, T> {
        self.try_reserve().expect("failed to allocate a cell")
    }

    pub fn try_reserve_block<T>(&self, count: usize) -> Option<CellBlock<N, T>> {
        self.internal_try_reserve_block(count)
            .map(|(index, states)| CellBlock {
                builder: &self,
                index,
                phantom: PhantomData,
                states,
            })
    }

    pub fn reserve_block<T>(&self, count: usize) -> CellBlock<N, T> {
        self.try_reserve_block(count)
            .expect("failed to allocate a cell")
    }
}

pub struct Cell<'a, const N: usize, T> {
    builder: &'a Builder<N>,
    index: usize,
    phantom: PhantomData<T>,
    state: CellState,
}

impl<'a, const N: usize, T> Drop for Cell<'a, N, T> {
    fn drop(&mut self) {
        self.builder
            .internal_return_reserved(self.index, self.state)
    }
}

pub struct CellBlock<'a, const N: usize, T> {
    builder: &'a Builder<N>,
    index: usize,
    phantom: PhantomData<T>,
    states: Vec<CellState>,
}

// impl<'a, const N: usize, T> Drop for CellBlock<'a, N, T> {
//     fn drop(&mut self) {
//         self.builder
//             .internal_return_reserved_block(self.index, &self.states)
//     }
// }

// impl<'a, const N: usize, T> CellBlock<'a, N, T> {
//     pub fn for_each(&self, mut f: impl FnMut(&Cell<'a, N, T>)) {
//         for (offset, state) in self.states.iter().enumerate() {
//             let cell = Cell::<'a, N, T> {
//                 index: self.index + offset,
//                 builder: self.builder,
//                 phantom: PhantomData,
//                 state: *state,
//             };

//             f(&cell);

//             if self.states[offset] != cell.state {
//                 panic!("cannot change a cell's state in an immutable closure")
//             }

//             forget(cell);
//         }
//     }

//     pub fn for_each_mut(&mut self, mut f: impl FnMut(&mut Cell<'a, N, T>)) {
//         for offset in 0..self.states.len() {
//             let mut cell = Cell::<'a, N, T> {
//                 index: self.index + offset,
//                 builder: self.builder,
//                 phantom: PhantomData,
//                 state: self.states[offset],
//             };

//             f(&mut cell);

//             self.states[offset] = cell.state;

//             forget(cell);
//         }
//     }
// }

// impl<'a, const N: usize, T> Index<usize> for CellBlock<'a, N, T> {
//     type Output = Cell<'a, N, T>;

//     fn index(&self, index: usize) -> &Self::Output {}
// }
