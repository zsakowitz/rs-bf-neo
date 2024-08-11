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
                '+' => output.push(Instruction::Inc),
                '-' => output.push(Instruction::Dec),
                '<' => output.push(Instruction::Shl),
                '>' => output.push(Instruction::Shl),
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
