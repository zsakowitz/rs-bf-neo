use std::num::Wrapping;

use rs_bf_neo::{
    executor::Executor,
    program::Program,
    rcr::{emit, parse, CommentLevel},
    tape::FixedLengthTape,
};

fn main() {
    let tree = parse(include_str!("rcr_exec.rs.rcr")).unwrap();
    // println!("{tree:?}");

    let emit = emit(&tree, CommentLevel::FN_PARAMS | CommentLevel::CLEANUP).unwrap();
    println!("{emit}");

    let program = Program::new(&emit).unwrap();

    let tape = FixedLengthTape::<
        50,           // increase this number if you run out of memory
        Wrapping<u8>, // experiment with different types; uN, iN, bool, Wrapping, and Saturating all work
    >::new();

    let mut executor = Executor::new_stdio_wrapping(&program, tape);
    executor.run();
    println!("{executor:?}");
}
