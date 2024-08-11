use std::io::{self, Write};

pub trait Output<T> {
    fn push(&mut self, value: T);
}

#[derive(Clone, Copy, Debug, Default)]
pub struct IgnoreOutput;

impl<T> Output<T> for IgnoreOutput {
    fn push(&mut self, _: T) {}
}

#[derive(Debug)]
pub struct Stdout(io::Stdout);

impl Default for Stdout {
    fn default() -> Self {
        Self::new()
    }
}

impl Stdout {
    pub fn new() -> Self {
        Self(io::stdout())
    }
}

impl<T> Output<T> for Stdout
where
    T: Into<u8>,
{
    fn push(&mut self, value: T) {
        self.0
            .write(&[value.into()])
            .expect("calls to stdout should succeed");
    }
}
