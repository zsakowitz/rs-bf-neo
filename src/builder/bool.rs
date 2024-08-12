use crate::builder::Cell;

pub struct Bool<T> {
    cell: T,
}

impl<T> Bool<T> {
    pub(super) unsafe fn new_unsafe(cell: T) -> Self {
        Bool { cell }
    }
}

impl<'a> Bool<Cell<'a>> {
    pub fn if_consuming<V>(&mut self, f: impl FnOnce() -> V) -> V {
        self.cell.while_nonzero_mut(|cell| {
            cell.dec();
            f()
        })
    }
}
