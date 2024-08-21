use rs_bf_neo::rcr::{emit, parse};

fn main() {
    let tree = parse(include_str!("rcr_exec.rs.rcr")).unwrap();
    println!("{tree:#?}");

    let emit = emit(&tree).unwrap();
    println!("{emit}");
}
