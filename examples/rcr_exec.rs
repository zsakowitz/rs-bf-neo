use rs_bf_neo::rcr::{emit, parse, CommentLevel};

fn main() {
    let tree = parse(include_str!("rcr_exec.rs.rcr")).unwrap();
    let emit = emit(&tree, CommentLevel::FN_PARAMS | CommentLevel::CLEANUP).unwrap();
    println!("{emit}");
}
