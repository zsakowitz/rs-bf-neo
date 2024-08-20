use rs_bf_neo::rcr::syntax::parse;

fn main() {
    let tree = parse(include_str!("rcr_parse.rs.rcr")).unwrap();
    println!("{tree:#?}");
}
