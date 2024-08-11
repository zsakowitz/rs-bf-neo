#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    IncBy(i8),
    Shift(isize),
    Read,
    Write,
    /// The `usize` here is the index of the corresponding LoopEnd.
    LoopStart(usize),
    /// The `usize` here is the index of the corresponding LoopStart.
    LoopEnd(usize),
}
