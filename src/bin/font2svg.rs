use std::fs::{self, File};
use std::io::BufWriter;
use std::env;
use std::error::Error;
use pathfinder_geometry::{
    vector::Vector2F,
    transform2d::{Transform2F},
    rect::RectF
};
use pathfinder_content::outline::{Outline};
use font::{Font, parse};
use vector::{Svg, Surface};

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    let data = fs::read(args.get(1).expect("no filename given")).expect("can't read specified file");
    let font = parse::<Outline>(&data);
    
    let num_glyphs = font.num_glyphs();
    let bbox = font.bbox().unwrap_or(RectF::new(Vector2F::default(), Vector2F::new(1.0, 1.0)));
    let scale = Vector2F::new(200., 200.);
    let bbox_ratio = bbox.size().x() / bbox.size().y();
    let aspect_ratio = 4. / 3.; // width to height
    let glyphs_x = (num_glyphs as f32 * aspect_ratio / bbox_ratio).sqrt().ceil() as u32;
    let glyphs_y = (num_glyphs + glyphs_x - 1) / glyphs_x;
    let size = scale * Vector2F::new(glyphs_x as f32, glyphs_y as f32) * (font.font_matrix() * bbox.size());
    
    println!("{} glyphs in {} by {}", num_glyphs, glyphs_x, glyphs_y);
    
    let mut surface = Svg::new(size);
    let black = surface.color_rgb(0, 0, 0);
    
    for gid in 0 .. num_glyphs {
        let y = gid as u32 / glyphs_x;
        let x = gid as u32 % glyphs_x;
        let offset = Vector2F::new(x as f32, (y + 1) as f32) * bbox.size();
        let transform = Transform2F::from_scale(scale)
        * font.font_matrix()
        * Transform2F::from_translation(offset)
        * Transform2F::from_scale(Vector2F::new(1.0, -1.0))
        * Transform2F::from_translation(-bbox.origin());
        
        let mut outline = font.glyph(gid).unwrap().path;
        outline.transform(&transform);
        surface.draw_path(outline, Some(&black), None);
    }
    fs::write("font.svg", surface.finish());
    
    Ok(())
}
