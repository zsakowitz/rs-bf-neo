use std::num::Wrapping;

use rs_bf_neo::{builder::Builder, executor::Executor, tape::FixedLengthTape};

fn main() {
    let builder = Builder::with_capacity(10);
    let (mut a, mut b) = builder.zeroed();
    a += 5;
    b += 7;
    b *= a;
    let program = builder.compile().unwrap();
    println!("{builder:?}");
    let mut executor =
        Executor::new_without_io(&program, FixedLengthTape::<10, Wrapping<u8>>::new());
    executor.run();
    println!("{executor:?}");
}
