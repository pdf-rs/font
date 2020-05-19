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
use font::{Font, parse, GlyphId};
use pathfinder_renderer::{
    scene::{Scene, DrawPath},
    paint::Paint
};
use pathfinder_color::ColorU;
use pathfinder_export::{Export, FileFormat};

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    let data = fs::read(args.get(1).expect("no filename given")).expect("can't read specified file");
    let font = parse(&data);
    
    let num_glyphs = font.num_glyphs();
    let mut bbox = font.bbox().unwrap_or(RectF::new(Vector2F::default(), Vector2F::new(1.0, 1.0)));
    for i in 0 .. num_glyphs {
        let glyph = font.glyph(GlyphId(i)).unwrap();
        bbox = bbox.union_rect(glyph.path.bounds());
    }
    let scale = Vector2F::new(200., 200.);
    let bbox_ratio = bbox.size().x() / bbox.size().y();
    let aspect_ratio = 4. / 3.; // width to height
    let glyphs_x = (num_glyphs as f32 * aspect_ratio / bbox_ratio).sqrt().ceil() as u32;
    let glyphs_y = (num_glyphs + glyphs_x - 1) / glyphs_x;
    let size = scale * Vector2F::new(glyphs_x as f32, glyphs_y as f32) * (font.font_matrix() * bbox.size());
    
    println!("{} glyphs in {} by {}", num_glyphs, glyphs_x, glyphs_y);
    
    let mut scene = Scene::new();
    scene.set_view_box(RectF::new(Vector2F::default(), size));
    let paint = scene.push_paint(&Paint::from_color(ColorU::black()));

    for gid in 0 .. num_glyphs {
        let y = gid as u32 / glyphs_x;
        let x = gid as u32 % glyphs_x;
        let offset = Vector2F::new(x as f32, (y + 1) as f32) * bbox.size();
        let transform = Transform2F::from_scale(scale)
        * font.font_matrix()
        * Transform2F::from_translation(offset)
        * Transform2F::from_scale(Vector2F::new(1.0, -1.0))
        * Transform2F::from_translation(-bbox.origin());
        
        let mut outline = font.glyph(GlyphId(gid)).unwrap().path;
        outline.transform(&transform);
        
        scene.push_path(DrawPath::new(outline, paint));
    }

    let mut writer = BufWriter::new(File::create("font.svg").unwrap());
    scene
        .export(&mut writer, FileFormat::SVG)
        .unwrap();
    
    Ok(())
}
