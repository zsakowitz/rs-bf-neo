use rs_bf_neo::builder::{Builder, Cell};

fn main() {
    let b = Builder::new();
    println!("{b:?}");
    let c0: Cell = b.zeroed();
    println!("{b:?}");
    let c1: Cell = b.zeroed();
    println!("{b:?}");
    let c2: Cell = b.zeroed();
    println!("{b:?}");
    drop(c1);
    println!("{b:?}");
    let [_b1, _b2, _b3, _b4] = b.zeroed();
    println!("{b:?}");
    drop(c2);
    println!("{b:?}");
    drop(c0);
    println!("{b:?}");

    b.zeroed::<(Cell, Cell)>();
}
