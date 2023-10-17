use font::{parse};

fn main() {
    env_logger::init();
    let arg = std::env::args().nth(1).unwrap();
    let data = std::fs::read(&arg).expect("can't read file");
    let font = parse(&data).unwrap();
    println!("{} glyphs", font.num_glyphs());
    let gid = font::GlyphId(758);
    font.glyph(gid);
}
