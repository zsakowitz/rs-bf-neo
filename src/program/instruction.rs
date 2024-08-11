#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    Inc,
    Dec,
    Shl,
    Shr,
    Read,
    Write,
    /// The `usize` here is the index of the corresponding LoopEnd.
    LoopStart(usize),
    /// The `usize` here is the index of the corresponding LoopStart.
    LoopEnd(usize),
}
