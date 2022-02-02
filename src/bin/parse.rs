use font::{parse};

fn main() {
    let arg = std::env::args().nth(1).unwrap();
    let data = std::fs::read(&arg).expect("can't read file");
    parse(&data).unwrap();
}
