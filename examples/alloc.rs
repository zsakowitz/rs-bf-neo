use rs_bf_neo::builder::Builder;

fn main() {
    let b = Builder::<10>::new();
    println!("{b:?}");
    let c0 = b.reserve::<u8>();
    println!("{b:?}");
    let c1 = b.reserve::<u8>();
    println!("{b:?}");
    let c2 = b.reserve::<u8>();
    println!("{b:?}");
    drop(c1);
    println!("{b:?}");
    let block_a = b.reserve_block::<u8>(4);
    println!("{b:?}");
    drop(c2);
    println!("{b:?}");
    drop(block_a);
    println!("{b:?}");
    drop(c0);
    println!("{b:?}");
}
