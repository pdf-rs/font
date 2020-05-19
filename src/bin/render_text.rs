use font::{Font, parse, layout::line};
use std::env;
use std::fs::{self, File};
use std::io::BufWriter;
use std::error::Error;
use pathfinder_export::{Export, FileFormat};

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    
    //let mut surface = Svg::new(BufWriter::new(File::create("glyph.svg")?), 200.0, 200.0);
    let file = args.get(1).expect("no filename given");
    let text = args.get(2).expect("no text given");
    
    let data = fs::read(file).expect("can't read specified file");
    let font = parse(&data);
    line(&*font, 20., &text)
        .export(&mut BufWriter::new(File::create("font.svg").unwrap()), FileFormat::SVG)
        .unwrap();
    
    Ok(())
}
