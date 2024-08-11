pub mod builder;
pub mod executor;
pub mod input;
pub mod output;
pub mod program;
pub mod step;
pub mod tape;

fn abc() {
    let b = builder::Builder::<30_000>::new();
}
