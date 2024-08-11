use std::marker::PhantomData;

use crate::{
    input::{NoInput, Stdin},
    output::{IgnoreOutput, Output, Stdout},
    program::{Instruction, Program},
    step::Step,
    tape::Tape,
};

#[derive(Clone, Debug)]
pub struct Executor<'a, T, U, I, O> {
    instructions: &'a [Instruction],
    instruction_index: usize,
    tape: U,
    input: I,
    output: O,
    phantom: PhantomData<T>,
}

impl<'a, T, U, I, O> Executor<'a, T, U, I, O> {
    pub fn new(program: &'a Program, tape: U, input: I, output: O) -> Self {
        Self {
            instructions: program,
            instruction_index: 0,
            tape,
            input,
            output,
            phantom: PhantomData,
        }
    }

    pub fn is_done(&self) -> bool {
        self.instruction_index >= self.instructions.len()
    }

    /// Returns `true` if the program was completed immediately before `.step()` was called.
    pub fn step(&mut self) -> bool
    where
        T: Copy + Step,
        U: Tape<T>,
        I: Iterator<Item = T>,
        O: Output<T>,
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
        T: Copy + Step,
        U: Tape<T>,
        I: Iterator<Item = T>,
        O: Output<T>,
    {
        while !self.step() {}
    }
}

impl<'a, U> Executor<'a, u8, U, Stdin, Stdout> {
    pub fn new_stdio(program: &'a Program, tape: U) -> Self {
        Self {
            instructions: program,
            instruction_index: 0,
            tape,
            input: Stdin::new(),
            output: Stdout::new(),
            phantom: PhantomData,
        }
    }
}

impl<'a, T, U> Executor<'a, T, U, NoInput<T>, IgnoreOutput> {
    pub fn new_without_io(program: &'a Program, tape: U) -> Self {
        Self {
            instructions: program,
            instruction_index: 0,
            tape,
            input: NoInput::new(),
            output: IgnoreOutput,
            phantom: PhantomData,
        }
    }
}
