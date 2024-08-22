use std::{
    fmt::Debug,
    iter::Map,
    marker::PhantomData,
    num::{Saturating, Wrapping},
};

use crate::{
    input::{NoInput, Stdin},
    output::{IgnoreOutput, Output, Stdout},
    program::{Instruction, Program},
    step::Step,
    tape::Tape,
};

#[derive(Clone)]
pub struct Executor<'a, U, I, O> {
    instructions: &'a [Instruction],
    instruction_index: usize,
    tape: U,
    input: I,
    output: O,
}

impl<'a, T, I, O> Debug for Executor<'a, T, I, O>
where
    T: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Executor")
            .field("instructions", &self.instructions)
            .field("instruction_index", &self.instruction_index)
            .field("tape", &self.tape)
            .finish_non_exhaustive()
    }
}

impl<'a, T, I, O> Executor<'a, T, I, O> {
    pub fn new(program: &'a Program, tape: T, input: I, output: O) -> Self {
        Self {
            instructions: program,
            instruction_index: 0,
            tape,
            input,
            output,
        }
    }

    pub fn is_done(&self) -> bool {
        self.instruction_index >= self.instructions.len()
    }

    /// Returns `true` if the program was completed immediately before `.step()` was called.
    pub fn step(&mut self) -> bool
    where
        T: Tape,
        T::Target: Copy + Step,
        I: Iterator<Item = T::Target>,
        O: Output<T::Target>,
    {
        let Some(current) = self.instructions.get(self.instruction_index) else {
            return true;
        };

        match *current {
            Instruction::IncBy(value) => {
                *self.tape = self.tape.inc_by(value);
            }
            Instruction::Shift(value) => {
                self.tape.shift(value);
            }
            Instruction::Read => {
                if let Some(value) = self.input.next() {
                    *self.tape = value;
                }
            }
            Instruction::Write => {
                self.output.push(*self.tape);
            }
            Instruction::LoopStart(loop_end) => {
                if self.tape.is_zero() {
                    self.instruction_index = loop_end + 1;
                    return false;
                }
            }
            Instruction::LoopEnd(loop_start) => {
                if !self.tape.is_zero() {
                    self.instruction_index = loop_start + 1;
                    return false;
                }
            }
        };

        self.instruction_index += 1;
        false
    }

    /// Runs this executor to completion.
    pub fn run(&mut self)
    where
        T::Target: Copy + Step,
        T: Tape,
        I: Iterator<Item = T::Target>,
        O: Output<T::Target>,
    {
        while !self.step() {}
    }

    /// Allows this executor to accept a different type of input.
    pub fn map_input<I2, Fn: FnMut(I::Item) -> I2>(self, map: Fn) -> Executor<'a, T, Map<I, Fn>, O>
    where
        I: Iterator,
    {
        Executor {
            input: self.input.into_iter().map(map),
            instruction_index: self.instruction_index,
            instructions: self.instructions,
            output: self.output,
            tape: self.tape,
        }
    }

    /// Allows this executor to push a different type of output.
    pub fn map_output<O1, O2, Fn: FnMut(O2) -> O1>(
        self,
        map: Fn,
    ) -> Executor<'a, T, I, MapOutput<O, O1, Fn>>
    where
        O: Output<O1>,
    {
        Executor {
            input: self.input,
            instruction_index: self.instruction_index,
            instructions: self.instructions,
            output: MapOutput {
                base: self.output,
                f: map,
                phantom: PhantomData,
            },
            tape: self.tape,
        }
    }
}

pub struct MapOutput<O: Output<O1>, O1, Fn> {
    base: O,
    f: Fn,
    phantom: PhantomData<fn(O1)>,
}

impl<O, O1, O2, Fn> Output<O2> for MapOutput<O, O1, Fn>
where
    O: Output<O1>,
    Fn: FnMut(O2) -> O1,
{
    fn push(&mut self, value: O2) {
        self.base.push((self.f)(value))
    }
}

impl<'a, T> Executor<'a, T, Stdin, Stdout> {
    pub fn new_stdio(program: &'a Program, tape: T) -> Self {
        Self {
            instructions: program,
            instruction_index: 0,
            tape,
            input: Stdin::new(),
            output: Stdout::new(),
        }
    }

    pub fn new_stdio_wrapping(
        program: &'a Program,
        tape: T,
    ) -> Executor<
        T,
        Map<Stdin, impl Fn(u8) -> Wrapping<u8>>,
        MapOutput<Stdout, u8, impl FnMut(Wrapping<u8>) -> u8>,
    > {
        Self {
            instructions: program,
            instruction_index: 0,
            tape,
            input: Stdin::new(),
            output: Stdout::new(),
        }
        .map_input(Wrapping)
        .map_output(|x: Wrapping<_>| x.0)
    }

    pub fn new_stdio_saturating(
        program: &'a Program,
        tape: T,
    ) -> Executor<
        T,
        Map<Stdin, impl Fn(u8) -> Saturating<u8>>,
        MapOutput<Stdout, u8, impl FnMut(Saturating<u8>) -> u8>,
    > {
        Self {
            instructions: program,
            instruction_index: 0,
            tape,
            input: Stdin::new(),
            output: Stdout::new(),
        }
        .map_input(Saturating)
        .map_output(|x: Saturating<_>| x.0)
    }
}

impl<'a, Target, T> Executor<'a, T, NoInput<Target>, IgnoreOutput> {
    pub fn new_without_io(program: &'a Program, tape: T) -> Self {
        Self {
            instructions: program,
            instruction_index: 0,
            tape,
            input: NoInput::new(),
            output: IgnoreOutput,
        }
    }
}
