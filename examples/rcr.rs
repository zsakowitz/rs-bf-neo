use rs_bf_neo::rcr::{Rule, parse};

fn main() {
    let tree = parse(include_str!("rcr.rs.rcr")).unwrap();
    println!("{tree:#?}");
}
