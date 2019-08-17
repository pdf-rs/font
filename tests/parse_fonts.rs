use font::{parse, Font, BorrowedFont};
use std::fs;
use std::path::Path;

fn main() {
    let font_dir = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().join("fonts");
    for entry in font_dir.read_dir().unwrap() {
        if let Ok(entry) = entry {
            println!("parsing {}", entry.path().to_str().unwrap());
            let data = fs::read(entry.path()).expect("can't read file");
            let font = parse(&data);
            
            // make sure we can parse all glyphs
            font.glyphs();
        }
    }
}
