use std::fs::{self, File};
use std::io::BufWriter;
use std::env;
use std::error::Error;
use pathfinder_geometry::transform2d::Transform2F;
use font::{Font, parse};
use vector::{Svg, Surface, Outline, Vector, PathStyle};
use raqote::DrawTarget;

fn draw_glyph<S: Surface + 'static>(file: &str, glyph: &str) -> S {
    let data = fs::read(file).expect("can't read specified file");
    let font = parse::<S::Outline>(&data);
    let bbox = font.bbox().unwrap();
    dbg!(bbox);
    
    let gid = glyph.parse().ok().and_then(|cp| font.gid_for_codepoint(cp)).or_else(||
        font.gid_for_name(glyph)
    ).expect("not a number or valid glyph name");
    
    let scale = Vector::new(400., 400.);
    let size = scale * (font.font_matrix() * bbox.size());
    let mut surface = S::new(size);
    let style = PathStyle {
        fill: Some((0, 0, 255, 100)),
        stroke: Some(((0, 0, 0, 255), 0.1))
    };
    let style = surface.build_style(style);
    let transform = Transform2F::from_scale(scale)
        * Transform2F::from_scale(Vector::new(1.0, -1.0))
        * font.font_matrix()
        * Transform2F::from_translation(Vector::new(0., -bbox.size().y()) - bbox.origin());
    dbg!(transform);
    let mut outline = font.glyph(gid).unwrap().path.transform(transform);
    surface.draw_path(outline, &style);
    surface
}

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    
    //let mut surface = Svg::new(BufWriter::new(File::create("glyph.svg")?), 200.0, 200.0);
    let file = args.get(1).expect("no filename given");
    let glyph = args.get(2).expect("no glyph given");
    
    fs::write("glyph.svg", draw_glyph::<Svg>(&file, &glyph).finish());
    draw_glyph::<DrawTarget>(&file, &glyph).write_png("glyph.png");
    
    Ok(())
}
