use font::{Font, parse, layout::line};
use vector::{Svg, Surface, PathStyle};
use std::env;
use std::fs;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    
    //let mut surface = Svg::new(BufWriter::new(File::create("glyph.svg")?), 200.0, 200.0);
    let file = args.get(1).expect("no filename given");
    let text = args.get(2).expect("no text given");
    
    let data = fs::read(file).expect("can't read specified file");
    let font = parse::<<Svg as Surface>::Outline>(&data);
    dbg!(font.font_matrix());
    let glyph_style = PathStyle {
        fill: Some((0, 0, 255, 100)),
        stroke: Some(((0, 0, 0, 255), 0.05))
    };
    let baseline_style = PathStyle {
        fill: None,
        stroke: Some(((0, 0, 0, 255), 0.05))
    };
    fs::write("text.svg", line::<Svg>(&*font, 20., &text, glyph_style, Some(baseline_style)).finish());
    
    Ok(())
}
