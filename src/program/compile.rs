use std::{error::Error, fmt::Display, ops::Deref};

use crate::program::Instruction;

#[derive(Clone, Debug, Default)]
pub struct Program {
    data: Vec<Instruction>,
}

#[derive(Clone, Copy, Debug)]
pub enum CompileError {
    UnmatchedLoopStart,
    UnmatchedLoopEnd,
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::UnmatchedLoopEnd => f.write_str("`]` found without corresponding `[`"),
            Self::UnmatchedLoopStart => f.write_str("`[` found without corresponding `]`"),
        }
    }
}

impl Error for CompileError {}

impl Program {
    pub fn new(source: &str) -> Result<Self, CompileError> {
        let mut output = Vec::new();
        let mut loop_start_indices = Vec::new();

        for char in source.chars() {
            match char {
                '+' | '-' => {
                    let value = if char == '-' { -1 } else { 1 };
                    let last = output.last_mut();
                    if let Some(Instruction::IncBy(ref mut prev_value)) = last {
                        if let Some(next) = prev_value.checked_add(value) {
                            if next == 0 {
                                output.pop();
                                continue;
                            }
                            *prev_value = next;
                        } else {
                            output.push(Instruction::IncBy(value));
                        }
                    } else {
                        output.push(Instruction::IncBy(value));
                    }
                }
                '<' | '>' => {
                    let value = if char == '<' { -1 } else { 1 };
                    let last = output.last_mut();
                    if let Some(Instruction::Shift(ref mut prev_value)) = last {
                        *prev_value += value;
                        if *prev_value == 0 {
                            output.pop();
                        }
                    } else {
                        output.push(Instruction::Shift(value));
                    }
                }
                ',' => output.push(Instruction::Read),
                '.' => output.push(Instruction::Write),
                '[' => {
                    loop_start_indices.push(output.len());
                    output.push(Instruction::LoopStart(0));
                }
                ']' => {
                    let Some(loop_start_index) = loop_start_indices.pop() else {
                        return Err(CompileError::UnmatchedLoopEnd);
                    };
                    let loop_end_index = output.len();
                    output[loop_start_index] = Instruction::LoopStart(loop_end_index);
                    output.push(Instruction::LoopEnd(loop_start_index));
                }
                _ => {}
            }
        }

        if !loop_start_indices.is_empty() {
            return Err(CompileError::UnmatchedLoopStart);
        }

        Ok(Self { data: output })
    }
}

impl Deref for Program {
    type Target = [Instruction];

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
