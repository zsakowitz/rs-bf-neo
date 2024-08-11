use rs_bf_neo::{executor::Executor, program::Program, tape::FixedLengthTape};

fn main() {
    let program = Program::new("+++++++++++++++++++++++++++++++++++++++++++++++++++.").unwrap();
    let mut executor = Executor::new_stdio(&program, FixedLengthTape::<65_536, u8>::new());
    executor.run();
}
